
# packages
library(RODBC)
library(icesTAF)
library(jsonlite)

# settings
config <- read_json("wgsfd_2018_dataset/config.json", simplifyVector = TRUE)

# create directories
mkdir(config$data_dir)

# connect to DB
conn <- odbcDriverConnect(connection = config$db_connection)

msg("downloading  ... ")
  
# set up sql command
sqlq <- paste(readLines("rawDataProc/VMS_LE_summary.sql"), collapse = "\n")

# fetch
out <- sqlQuery(conn, sqlq)

# save to file
write.csv(out, file = paste0(config$data_dir, "/VMS_LE_summary.csv"), row.names = FALSE)

# disconnect
odbcClose(conn)
