

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# This script creates an Rmd file for a specific country
source("QC/utilities.R")

# main script -----------------
library(rmarkdown)
library(icesTAF)
library(jsonlite)

# get list of countries
config <- read_json("QC/config.json", simplifyVector = TRUE)

# create report directory
mkdir(config$report_dir)

# loop over countries
for (country in config$countries) {
  #country <- "IRL"
  msg("Running QC for ... ", country)
  
  # fillin and write template
  fname <- makeQCRmd(country, config$data_dir)

  # render Rmd
  ret <- try(render(fname, clean = FALSE, output_format = latex_document()))
  if (inherits(ret, "try-error")) {
    msg("FAILED - ", country)
    next
  }
  
  # compile pdf
  shell(paste('pdflatex -halt-on-error', ret))
  
  # copy report and Rmd file
  copyReport(fname, report_dir = config$report_dir)
  
  msg("Done ... ", country)
}
