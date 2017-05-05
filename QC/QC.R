

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

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
            "ICES_LE <- read.table('%s', sep = ',', header = TRUE,",
            "          stringsAsFactors = FALSE, na.strings = 'NULL',",
            "          colClasses = c('character', 'character', 'numeric', 'numeric'," ,
            "                         'character', 'character', 'character', 'numeric',",
            "                         'character', 'character',",
            "                         'numeric', 'numeric', 'numeric'))",
            "ICES_VE <- read.table('%s', sep = ',', header = TRUE,",
            "          stringsAsFactors = FALSE, na.strings = 'NULL',",
            "          colClasses = c('character', 'character', 'numeric', 'numeric',",
            "                         'character', 'character', 'character', 'character',",
            "                         'numeric', 'numeric', 'numeric', 'numeric',", 
            "                         'numeric', 'numeric', 'numeric', 'numeric'))",
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
  t0 <- proc.time()
  
  # setup file name
  fname <- paste0("QC_", country,".Rmd")

  # fillin and write template
  cat(makeQCRmd(country, qc), sep = "\n", file = fname)

  # run template
  ret <- try(rmarkdown::render(fname))
  if (inherits(ret, "try-error")) next
  
  # clean up
  file.copy(fname, file.path("QC/reports", fname), overwrite = TRUE)
  repname <- gsub(".Rmd", ".pdf", fname)
  file.copy(repname, file.path("QC/reports", repname), overwrite = TRUE)
  unlink(fname); unlink(repname)

  cat("ellapsed:", (proc.time() - t0)[2], "\n\n")
}
