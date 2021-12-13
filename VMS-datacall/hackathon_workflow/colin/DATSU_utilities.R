
if (!require("icesVocab")) {
  install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")
  library(icesVocab)
}

require(jsonlite)
require(httr)

# functions to be written here:

datsu_api <- function(service) {
  paste0("https://datsu.ices.dk/API/", service)
}

# * get list of formats

#' @examples
#' formats <- getDataverIDs()
#' vms_format <- formats[grep("vms", tolower(formats$description)),]
#' vms_format
getDataverIDs <- function() {
  url <- datsu_api("getDataverIDs")
  res <- httr::GET(url)

  content(res, simplifyVector = TRUE)
}


# * get list of field names for DATSU format

# * get if field is mandatory

# * get if field has vocabulary

# * get vocabulary for field

# * check if feild is mandatory

# * check vector against a vocabulary

# * check a data.frame against a DATSU format
#    - check feild names
#    - check mandatory feilds for NULL etc.
#   - check vocabularied feilds against vocab
#   - check order of feilds
#   - check for missing feilds
#   - check for extra feilds

# * submit file
