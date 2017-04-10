
# This script creates an Rmd file for a specific country

# utility function
makeQCRmd <- function(country, qc) {
  # make title
  qc$yaml[grep("title:", qc$yaml)] <- 
    paste0("title: \"ICES VMS datacall quality check report for ", country, "\"")
     
  # fill in file names
  qc$data <- 
    sprintf(   
      paste("<!------------------------------------------------------------------------------",
            "Data handling",
            "---------------------------------------------------------------------------- -->",
            "```{r data}",
            "#Read in latest submission -->",
            "ICES_LE <- read.csv('%s', stringsAsFactors=FALSE, na.strings = 'NULL')",
            "ICES_VE <- read.csv('%s', stringsAsFactors=FALSE, na.strings = 'NULL')",
            "```",
            "", sep = "\n"),
     paste0("data/ICES_LE_", country, ".csv"), 
     paste0("data/ICES_VE_", country, ".csv"))

  unlist(qc)
}


# main script -----------------

# read and parse template file
qc <- readLines("QC/QC.Rmd")
loc1 <- grep("<!-- QCTEMPLATE: header -->", qc)
loc2 <- grep("<!-- QCTEMPLATE: data -->", qc)
loc3 <- grep("<!-- QCTEMPLATE: body -->", qc)
qc <- list(yaml = qc[1:(loc1-1)], 
           header = qc[loc1:(loc2-1)], 
           data = qc[loc2:(loc3-1)], 
           body = qc[loc3:length(qc)])

# get list of countries
countries <- gsub("ICES_VE_|[.]csv", "", dir("data", pattern = "ICES_VE_*"))

# create report directory
if (!dir.exists("QC/reports")) dir.create("QC/reports")

# loop over countries
for (country in countries) {
  cat("Running QC for ... ", country, "\n")
  
  # setup file name
  fname <- paste0("QC_", country,".Rmd")

  # fillin and write template
  cat(makeQCRmd(country, qc), sep = "\n", file = fname)

  # run template
  rmarkdown::render(fname)
  file.copy(fname, file.path("QC/reports", fname))
  repname <- gsub(".Rmd", ".pdf", fname)
  file.copy(repname, file.path("QC/reports", repname))
  unlink(fname); unlink(repname)
}
  








