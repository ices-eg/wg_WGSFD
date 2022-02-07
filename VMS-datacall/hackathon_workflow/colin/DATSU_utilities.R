
if (!require("icesVocab")) {
  install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")
  library(icesVocab)
}

require(jsonlite)
require(httr)

# functions to be written here:

#' @examples
#' datsu_api("hi", bye = 21)
#' datsu_api("getDataverIDs")
#' @export
datsu_api <- function(service, ...) {
  url <- paste0("https://datsu.ices.dk/API/", service)
  url <- httr::parse_url(url)
  url$query <- list(...)
  url <- httr::build_url(url)

  httr::GET(url)
}

trimws_df <- function(df) {
  df[] <-
    lapply(
      df,
      function(x) if (is.character(x)) trimws(x) else x
    )

  df
}


# * open datsu page
# https://datsu.ices.dk/web/selRep.aspx?Dataset=145

# * get list of formats

#' get list of datasets
#'
#' @examples
#' formats <- getDataverIDs()
#' vms_format <- formats[grep("vms", tolower(formats$description)),]
#' vms_format
#' @export
getDataverIDs <- function() {
  res <- datsu_api("getDataverIDs")
  out <- content(res, simplifyVector = TRUE)

  trimws_df(out)
}

#' get list of dataset records for a given dataset
#'
#' @examples
#' getRecordIDs(145)
getRecordIDs <- function(datasetverID) {
  res <- datsu_api(
    paste0("getRecordIDs/", datasetverID)
  )
  out <- content(res, simplifyVector = TRUE)

  trimws_df(out)
}

#' get list of dataset records for a given dataset
#'
#' @examples
#' getDataFieldsDescription(145)
#' getDataFieldsDescription(145, "VE")
getDataFieldsDescription <- function(datasetverID, RecordType) {
  if (missing(RecordType)) {
    res <-
      datsu_api(
        paste0("getDataFieldsDescription/", datasetverID)
      )
  } else {
    res <-
      datsu_api(
        paste0("getDataFieldsDescription/", datasetverID),
        RecordType = RecordType
      )
  }

  out <- content(res, simplifyVector = TRUE)

  trimws_df(out)
}



# * get list of field names for DATSU format
#'
#' @examples
#' datsuFieldNames(145, "VE")
datsuFieldNames <- function(datasetverID, RecordType) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  datsuFields$fieldcode
}

# * get list of field names for DATSU format
#'
#' @examples
#' datsuFieldTypes(145, "VE")
datsuFieldTypes <- function(datasetverID, RecordType) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  fieldType <- gsub("[(][0-9]*[)]", "", datsuFields$name)
  mode <-
    ifelse(
      fieldType %in% c("nvarchar", "char"), "character",
      ifelse(
        fieldType == "int", "integer",
        ifelse(
          fieldType == "float",
          "numeric",
          "unknown"
        )
      )
    )

  mode
}

# * get if field is mandatory

#'
#'
#' @examples
#' datsuIsMandatory(145, "VE", "AverageVesselLength")
datsuIsMandatory <- function(datasetverID, RecordType, FieldName) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  datsuFields[datsuFields$fieldcode == FieldName, "mandatory"]
}

# * get if field has vocabulary

#'
#'
#' @examples
#' datsuHasVocabulary(145, "VE", "AverageVesselLength")
#' datsuHasVocabulary(145, "VE", "CountryCode")
datsuHasVocabulary <- function(datasetverID, RecordType, FieldName = NULL) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  if (is.null(FieldName)) {
    codeType <- datsuFields$codeGroup
  } else {
    codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]
  }

  !is.na(codeType)
}


# * get vocabulary for field

#'
#'
#' @examples
#' datsuGetVocabulary(145, "VE", "AverageVesselLength")
datsuGetVocabulary <- function(datasetverID, RecordType, FieldName = NULL) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  if (is.null(FieldName)) {
    codeType <- datsuFields$codeGroup
  } else {
    codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]
  }

  codeList <- icesVocab::getCodeList(codeType)

  codeList[!codeList$Deprecated, "Key"]
}

# * check vector against a vocabulary
#'
#'
#' @examples
#' datsuGetVocabulary(145, "VE", "AverageVesselLength")
datsuGetVocabulary <- function(datasetverID, RecordType, FieldName) {
  datsuFields <- getDataFieldsDescription(datasetverID, RecordType)

  codeType <- datsuFields[datsuFields$fieldcode == FieldName, "codeGroup"]

  codeList <- icesVocab::getCodeList(codeType)

  codeList[!codeList$Deprecated, "Key"]
}



makeEmptyTable <- function(datasetverID, RecordType) {
  df <-
    lapply(
      datsuFieldTypes(datasetverID, RecordType),
      function(x) {
        FUN <- match.fun(x)
        FUN(0)
      }
    )
  names(df) <- datsuFieldNames(datasetverID, RecordType)
  data.frame(df, check.names = FALSE)
}


# * check vector against a vocabulary
#'
#'
#' @examples
#' makeExampleDatsuTable(145, "VE", 20)
makeExampleDatsuTable <- function(datasetverID, recordType, n = 10) {
  good <- as.list(makeEmptyTable(datasetverID, recordType))

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "integer")) {
    good[[i]] <- sample(1:n, n, replace = TRUE)
  }

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "numeric")) {
    good[[i]] <- runif(n)
  }

  for (i in which(datsuFieldTypes(datasetverID, recordType) == "character")) {
    good[[i]] <- rep("non-vocabulary", n)
  }

  for (i in which(datsuHasVocabulary(datasetverID, recordType))) {
    good[[i]] <-
      sample(
        datsuGetVocabulary(datasetverID, recordType, datsuFieldNames(datasetverID, recordType)[i]),
        n,
        replace = TRUE
      )
  }

  good$RecordType <- recordType

  good <- data.frame(good, check.names = FALSE)
  good
}



# * check a data.frame against a DATSU format
#    - check feild names
#    - check mandatory feilds for NULL etc.
#   - check vocabularied feilds against vocab
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds

checkDatsuTable <- function(df, datasetverID, RecordType) {

  # check for missing feild names
  warnings <- character(0)
  df_names <- names(df)
  datsu_names <- datsuFieldNames(datasetverID, RecordType)
  if (!all(datsu_names %in% df_names)) {
    warnings <-
      c(
        warnings,
        paste0(
          "Missing names in table: ",
          paste0(
            datsu_names[!datsu_names %in% df_names],
            collapse = ", "
          )
        )
      )
  }

  # check for extra feild names
  if (any(!df_names %in% datsu_names)) {
        warnings <-
          c(
            warnings,
            paste0(
              "Extra names in table: ",
              paste0(
                df_names[!df_names %in% datsu_names],
                collapse = ", "
              )
            )
          )
  }

  # check for the ordering of the feild names
  if (
      all(datsu_names %in% df_names) &&
      !any(!df_names %in% datsu_names) &&
      !identical(df_names, datsu_names)
    ) {
    warnings <-
      c(
        warnings,
        paste0(
          "Column names are not in the correct order ",
          "see datsuFieldNames(", datasetverID, ",", RecordType, ")"
        )
      )
  }


  if (length(warnings)) {
    cat(warnings, sep = "\n")
    return(FALSE)
  }

  # check vocabularied feilds against vocab


#    - check mandatory feilds for NA or empty etc.
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds

  return(TRUE)
}


# * submit file
