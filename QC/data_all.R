
# packages
library(RODBC)
library(icesTAF)
library(jsonlite)

# utilities
source("QC/utilities.R")

# settings
config <- read_json("QC/config.json", simplifyVector = TRUE)

# create directories
mkdir(config$data_dir)

# connect to DB
conn <- odbcDriverConnect(connection = config$db_connection)

for (year in  config$years) {
  msg("downloading LE data for ... ", year)
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_LE", year)
  fname <- paste0(config$data_dir, "/ICES_LE_", year, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}


for (year in  config$years) {
  msg("downloading VMS data for ... ", year)
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_VMS", year)
  fname <- paste0(config$data_dir, "/ICES_VE_", year, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
