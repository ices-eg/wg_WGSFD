
source("VMS-datacall/hackathon_workflow/colin/DATSU_utilities.R")

formats <- getDataverIDs()
vms_format <- formats[grep("vms", tolower(formats$description)),]
vms_format

getRecordIDs(145)

getDataFieldsDescription(145)
getDataFieldsDescription(145, "VE")

datsuFieldNames(145, "VE")

datsuIsMandatory(145, "VE", "AverageVesselLength")

datsuHasVocabulary(145, "VE", "AverageVesselLength")
datsuHasVocabulary(145, "VE", "CountryCode")

datsuGetVocabulary(145, "VE", "CountryCode")
