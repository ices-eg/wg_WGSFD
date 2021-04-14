

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# libraries
library(rmarkdown)
library(icesTAF)
library(jsonlite)

# settings
config <- read_json("QC/config.json", simplifyVector = TRUE)

# create report directory
mkdir(config$report_dir)

# loop over countries
for (country in config$countries) {
  # utiities
  source("QC/utilities.R")

  #country <- "BEL"
  msg("Running QC for ... ", country)

  # fillin and write template
  fname <- makeQCRmd(country, config$data_dir, template = "QC/report-QC-template.Rmd")

  # render Rmd
  ret <- try(render(fname, clean = FALSE, output_format = latex_document()))
  if (inherits(ret, "try-error")) {
    msg("FAILED - ", country)
    next
  }

  # compile pdf
  x <- shell(paste('pdflatex -halt-on-error', ret))

  if (x == 0) {
    # copy report and Rmd file
    copyReport(fname, report_dir = config$report_dir, keeps = c("pdf", "knit.md"))
  }

  msg("Done ... ", country)
}
