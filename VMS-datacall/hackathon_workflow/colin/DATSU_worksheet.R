
source("VMS-datacall/hackathon_workflow/colin/DATSU_utilities.R")

makeExampleDatsuTable(126, "AA")

getRecordIDs(126)
getDataFieldsDescription(126, "AA")


formats <- getDataverIDs()
vms_format <- formats[grep("vms", tolower(formats$description)),]
vms_format

datasetverID <- vms_format$datasetVerID

recordIDs <- getRecordIDs(datasetverID)
recordIDs

recordType <- recordIDs$recordType[2]
recordType

getRecordIDs(datasetverID)

getDataFieldsDescription(datasetverID)
getDataFieldsDescription(datasetverID, recordType)

datsuFieldNames(datasetverID, recordType)

datsuFieldTypes(datasetverID, recordType)

datsuIsMandatory(datasetverID, recordType, "AverageVesselLength")

datsuHasVocabulary(datasetverID, recordType, "AverageVesselLength")
datsuHasVocabulary(datasetverID, recordType, "CountryCode")

datsuGetVocabulary(datasetverID, recordType, "CountryCode")

# check a table

makeExampleDatsuTable(145, "VE", 20)

good <- makeExampleDatsuTable(datasetverID, "VE", 20)

icesTAF::write.taf(good)

good$MetierL6 <- paste0(good$MetierL4, "_", good$MetierL5, "_", good$LowerMeshSize, "_", good$UpperMeshSize, "_0")
good$RecordType <- "VE"
good$Year <- "2020"

good <- data.frame(good, check.names = FALSE)
good

source("VMS-datacall/hackathon_workflow/colin/DATSU_utilities.R")


makeExampleDatsuTable(145, "VE", 20)

checkDatsuTable(good, 145, "VE")

checkDatsuTable(good[-c(4:5)], 145, "VE")
checkDatsuTable(cbind(good, new = 0), 145, "VE")

checkDatsuTable(cbind(good[-c(4:5)], new = 0), 145, "VE")

checkDatsuTable(good[ncol(good):1], 145, "VE")
