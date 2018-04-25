
# packages
library(RODBC)
# settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes'

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/QC_2018")) dir.create("data/QC_2018")

# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

# get submitted countries
#sqlQuery(conn, "SELECT DISTINCT country FROM dbo._2017_ICES_VMS_Datacall_LE")$country
#countries <- c("DNK", "BEL", "IRL", "SWE", "nld", "FRA", "POL", "Latvia", "EST", "PRT", "LTU")
#countries <- c('DNK', 'BEL', 'SWE', 'NLD', 'FRA', 'FIN', 'LTU', 'POL', 'Latvia', 'PRT','IRL', 'GBR')
#countries <-  c('FRA', 'LTU', 'Latvia', 'GBR')
#countries <-  'IRL'
countries <-  c('FIN', 'LTU', 'EST', 'IRL')

for (country in countries) {
  cat("downloading LE data for ... ", country, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._2017_ICES_VMS_Datacall_LE WHERE country = '%s'", country)
  fname <- paste0("data/QC_2018/ICES_LE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}


for (country in countries) {
  cat("downloading VMS data for ... ", country, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM [dbo].[_2017_ICES_VMS_Datacall_VMS] WHERE country = '%s'", country)
  fname <- paste0("data/QC_2018/ICES_VE_", country, ".csv")
  
  # fetch
  out <- sqlQuery(conn, sqlq)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)
}

# disconnect
odbcClose(conn)
