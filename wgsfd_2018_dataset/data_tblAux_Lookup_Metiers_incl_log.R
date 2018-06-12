
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
sqlq <- "select * from tblAux_Lookup_Metiers_incl_log"

# fetch
out <- sqlQuery(conn, sqlq)

# save to file
write.csv(out, file = paste0(config$data_dir, "/tblAux_Lookup_Metiers_incl_log.csv"), row.names = FALSE)

# disconnect
odbcClose(conn)
