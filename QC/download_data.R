
# packages
library(RODBC)
# settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes'

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/QC")) dir.create("data/QC")

# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

# get submitted countries
#sqlq <- sprintf("SELECT DISTINCT country FROM dbo._2017_ICES_VMS_Datacall_LE", country)
#sqlQuery(conn, sqlq)$country
#countries <- c('DNK', 'BEL', 'SWE', 'NLD', 'FRA', 'FIN', 'LTU', 'POL', 'Latvia', 'PRT','IRL', 'GBR')
#countries <-  c('FRA', 'LTU', 'Latvia', 'GBR')
countries <-  c('ICE')

for (country in countries) {
  cat("downloading LE data for ... ", country, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._2017_ICES_VMS_Datacall_LE WHERE country = '%s'", country)
  fname <- paste0("data/QC/ICES_LE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}


for (country in countries) {
  cat("downloading VMS data for ... ", country, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM [dbo].[_2017_ICES_VMS_Datacall_VMS] WHERE country = '%s'", country)
  fname <- paste0("data/QC/ICES_VE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
