
# get packages
devtools::install_github("hadley/dplyr@v0.4.3")

# create directories
if (!dir.exists("data")) dir.create("data")

# download total abrasion data ----

db <- DBI::dbConnect(RSQLServer::SQLServer(), "SQL06", database = 'VMS')

# Fetch results from a fishing hours extraction view
ICES_LE <- DBI::dbGetQuery(db, "SELECT * FROM [dbo].[_ICES_VMS_Datacall_LE]")
ICES_VE <- DBI::dbGetQuery(db, "SELECT * FROM [dbo].[_ICES_VMS_Datacall_VE]")

DBI::dbDisconnect(db)

# save to file
write.csv(ICES_LE, file = "data/ICES_LE.csv", row.names = FALSE)
write.csv(ICES_VE, file = "data/ICES_VE.csv", row.names = FALSE)

