

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# libraries
library(rmarkdown)
library(icesTAF)
library(jsonlite)

# utiities
source("QC/utilities.R")

# settings
config <- read_json("QC/config.json", simplifyVector = TRUE)

# create report directory
mkdir(config$report_dir)

# loop over countries
for (year in config$years) {
  #country <- "EST"
  msg("Running QC for ... ", year)
  
  # fillin and write template
  fname <- makeQCRmd(year, config$data_dir, template = "QC/report-QC_all-template.Rmd")

  # render Rmd
  ret <- try(render(fname, clean = FALSE, output_format = latex_document()))
  if (inherits(ret, "try-error")) {
    msg("FAILED - ", year)
    next
  }
  
  # compile pdf
  x <- shell(paste('pdflatex -halt-on-error', ret))
  
  if (x == 0) {
    # copy report and Rmd file
    copyReport(fname, report_dir = config$report_dir, keeps = c("pdf", "knit.md"))
  }
  
  msg("Done ... ", year)
}
