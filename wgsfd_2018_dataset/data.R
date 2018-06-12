
# packages
library(RODBC)
library(icesTAF)
library(jsonlite)

# settings
config <- read_json("wgsfd_2018_dataset/config.json", simplifyVector = TRUE)

# utilities
source("QC/utilities.R")

# create directories
mkdir(config$data_dir)

# connect to DB
conn <- odbcDriverConnect(connection = config$db_connection)

msg("downloading  ... ")
  
# set up sql command
sqlq <- paste(readLines("rawDataProc/VMS_extraction_benthis_month_csquare.sql"), collapse = "\n")

# fetch
out <- sqlQuery(conn, sqlq)

# add lat long
tmp <- cbind(out, vmstools::CSquare2LonLat(out$c_square, 0.05))
tmp <- dplyr::rename(tmp,  Latitude = SI_LATI, Longitude = SI_LONG)

# save to file
write.csv(tmp, file = paste0(config$data_dir, "/vms_benthis_grouping_2018.csv"), row.names = FALSE)

# disconnect
odbcClose(conn)
