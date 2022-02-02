
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
  res <- datsu_api("getRecordIDs", datasetverID = datasetverID)
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
        "getDataFieldsDescription",
        datasetverID = datasetverID
      )
  } else {
    res <-
      datsu_api(
        "getDataFieldsDescription",
        datasetverID = datasetverID, RecordType = RecordType
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
  as.data.frame(df)
}



# * check a data.frame against a DATSU format
#    - check feild names
#    - check mandatory feilds for NULL etc.
#   - check vocabularied feilds against vocab
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds

# * submit file
