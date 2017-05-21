
# packages
library(RODBC)
# settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes'

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/fishingPressure")) dir.create("data/fishingPressure")

# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

# get submitted countries
sqlq <- "select top(10) * from  dbo.tblFinal_Maps_withFormula"
out <- sqlQuery(conn, sqlq)

years <- 2009:2016

for (year in years) {
  cat("downloading Fishing pressure data for ... ", year, "\n")
  flush.console()
  
  # set up sql command
  sqlq <- sprintf("select * from  dbo.tblFinal_Maps_withFormula WHERE Year = '%s'", year)
  fname <- paste0("data/fishingPressure/OSPAR_intensity_data", year, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
