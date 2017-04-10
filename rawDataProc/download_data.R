
# get packages
devtools::install_github("hadley/dplyr@v0.4.3")
# set large java mem
options(java.parameters='-Xmx8g')

# create directories
if (!dir.exists("data")) dir.create("data")

# connect to DB
db <- DBI::dbConnect(RSQLServer::SQLServer(), "SQL06", database = 'VMS')

#submitted_countries <- c('DNK', 'BEL', 'SWE', 'NLD', 'FRA', 'FIN', 'LTU', 'POL', 'Latvia', 'PRT','IRL')

submitted_countries <- c('DNK', 'BEL', 'SWE', 'NLD', 'FIN', 'LTU', 'POL', 'Latvia', 'PRT','IRL')

for (country in submitted_countries) {
  cat("downloading LE data for ... ", country, "\n")
  
  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._2017_ICES_VMS_Datacall_LE WHERE country = '%s'", country)
  fname <- paste0("data/ICES_LE_", country, ".csv")
  
  # fetch
  res <- DBI::dbSendQuery(db, sqlq)
  out <- DBI::dbFetch(res, n = 1e5)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)

  # Fetch in chunks
  while (!DBI::dbHasCompleted(res)) {
    out <- DBI::dbFetch(res, n = 1e5)
    write.table(out, file = fname, 
                row.names = FALSE, sep = ",",
                append = TRUE)
  }
  DBI::dbClearResult(res)
}


for (country in submitted_countries) {
  cat("downloading VMS data for ... ", country, "\n")
  
  # connect to databse
  db <- DBI::dbConnect(RSQLServer::SQLServer(), "SQL06", database = 'VMS')

  # set up sql command
  sqlq <- sprintf("SELECT * FROM [dbo].[_2017_ICES_VMS_Datacall_VMS] WHERE country = '%s'", country)
  fname <- paste0("data/ICES_VE_", country, ".csv")
  
  # fetch
  res <- DBI::dbSendQuery(db, sqlq)
  out <- DBI::dbFetch(res, n = 1e5)
  # save to file
  write.csv(out, file = fname, row.names = FALSE)

  # Fetch in chunks
  while (!DBI::dbHasCompleted(res)) {
    out <- DBI::dbFetch(res, n = 1e5)
    write.table(out, file = fname, 
                row.names = FALSE, sep = ",",
                append = TRUE)
  }
  DBI::dbClearResult(res)

}


DBI::dbDisconnect(db)
