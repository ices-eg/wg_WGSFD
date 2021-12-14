
source("VMS-datacall/hackathon_workflow/colin/DATSU_utilities.R")

datasetverID <- 145
RecordType <- "VE"

formats <- getDataverIDs()
vms_format <- formats[grep("vms", tolower(formats$description)),]
vms_format

getRecordIDs(145)

getDataFieldsDescription(145)
getDataFieldsDescription(145, "VE")

datsuFieldNames(145, "VE")

datsuFieldTypes(145, "VE")

datsuIsMandatory(145, "VE", "AverageVesselLength")

datsuHasVocabulary(145, "VE", "AverageVesselLength")
datsuHasVocabulary(145, "VE", "CountryCode")

datsuGetVocabulary(145, "VE", "CountryCode")

# check a table
n <- 10
good <- as.list(makeEmptyTable(datasetverID, RecordType))
for (i in which(datsuFieldTypes(145, "VE") == "integer")) {
  good[[i]] <- sample(1:10, n, replace = TRUE)
}
for (i in which(datsuFieldTypes(145, "VE") == "numeric")) {
  good[[i]] <- runif(n)
}
for (i in which(datsuFieldTypes(145, "VE") == "character")) {
  good[[i]] <- rep("non-vocabulary", n)
}

for (i in which(datsuHasVocabulary(145, "VE"))) {
  good[[i]] <-
    sample(
      datsuGetVocabulary(145, "VE", datsuFieldNames(145, "VE")[i]),
      n,
      replace = TRUE
    )
}

good$MetierL6 <- paste0(good$MetierL4, "_", good$MetierL5, "_", good$LowerMeshSize, "_", good$UpperMeshSize, "_0")
good$RecordType <- "VE"
good$Year <- "2020"

good <- as.data.frame(good)
good
