
# packages
library(RODBC)
# settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes'

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/QC_all")) dir.create("data/QC_all")

# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

years <- 2016:2017

for (year in years) {
  cat("downloading LE data for ... ", year, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_LE", year)
  fname <- paste0("data/QC_all/ICES_LE_", year, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}


for (year in years) {
  cat("downloading VMS data for ... ", year, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_VMS", year)
  fname <- paste0("data/QC_all/ICES_VE_", year, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
