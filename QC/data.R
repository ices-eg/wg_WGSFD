
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

# get submitted countries
#sqlQuery(conn, "SELECT DISTINCT country FROM dbo._2017_ICES_VMS_Datacall_LE")$country

for (country in config$countries) {
  msg("downloading LE data for ... ", country)
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._2017_ICES_VMS_Datacall_LE WHERE country = '%s' order by year, ICES_rectangle, gear_code, month", country)
  fname <- paste0(config$data_dir, "/ICES_LE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}


for (country in config$countries) {
  msg("downloading VMS data for ... ", country)
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM [dbo].[_2017_ICES_VMS_Datacall_VMS] WHERE country = '%s' order by year, ICES_rectangle, gear_code, month", country)
  fname <- paste0(config$data_dir, "/ICES_VE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
