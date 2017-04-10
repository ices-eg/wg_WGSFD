
# This script creates an Rmd file for a specific country
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
            "```{r, echo=FALSE, results='asis'}",
            "#Read in latest submission -->",
            "ICES_LE <- read.csv('%s', stringsAsFactors=FALSE, na.strings = 'NULL')",
            "ICES_VE <- read.csv('%s', stringsAsFactors=FALSE, na.strings = 'NULL')",
            "```",
            "", sep = "\n"),
     paste0("data/ICES_LE_", country, ".csv"), 
     paste0("data/ICES_VE_", country, ".csv"))

  unlist(qc)
}


# read and parse from template file
qc <- readLines("QC/QC.Rmd")
loc1 <- grep("<!-- QCTEMPLATE: header -->", qc)
loc2 <- grep("<!-- QCTEMPLATE: data -->", qc)
loc3 <- grep("<!-- QCTEMPLATE: body -->", qc)

qc <- list(yaml = qc[1:(loc1-1)], 
           header = qc[loc1:(loc2-1)], 
           data = qc[loc2:(loc3-1)], 
           body = qc[loc3:length(qc)])

# update template with country info

countries <- gsub("ICES_VE_|[.]csv", "", dir("data", pattern = "ICES_VE_*"))

# create directories
if (!dir.exists("QC/reports")) dir.create("QC/reports")

for (country in countries) {
  cat("Running QC for ... ", country, "\n")
  
  # set up file names
  fname <- paste0("QC/reports/QC_", country,".Rmd")

  # fillin and write template
  cat(makeQCRmd(country, qc), sep = "\n", file = fname)

  # run template
  rmarkdown::render(fname, knit_root_dir = getwd(), output_dir = "QC/reports")
}
  








