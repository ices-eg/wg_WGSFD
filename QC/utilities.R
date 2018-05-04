
# utility function
makeQCRmd <- function(country, data_dir, template = "QC/report-QC-template.Rmd") {
  
  # read and parse template file
  qc <- readLines(template)
  loc1 <- grep("<!-- QCTEMPLATE: header -->", qc)
  loc2 <- grep("<!-- QCTEMPLATE: data -->", qc)
  loc3 <- grep("<!-- QCTEMPLATE: body -->", qc)
  qc <- list(yaml = qc[1:(loc1-1)], 
         header = qc[loc1:(loc2-1)], 
         data = qc[loc2:(loc3-1)], 
         body = qc[loc3:length(qc)])
  
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
     paste0(data_dir, "/ICES_LE_", country, ".csv"), 
     paste0(data_dir, "/ICES_VE_", country, ".csv"))

  # setup file name
  fname <- paste0("QC_", country, format(Sys.time(), "_%Y-%m-%d_%b-%Y"),".Rmd")
  
  # write Rmd
  cat(unlist(qc), sep = "\n", file = fname)
  
  fname
}



copyReport <- function(fname, report_dir, keeps = c("Rmd", "pdf", "tex")) {
  # copy report and Rmd file
  fname <- tools::file_path_sans_ext(fname)
  for (ext in keeps) {
    cp(paste0(fname, ".", ext), report_dir, move = TRUE)
  }  
  
  # clean up
  Sys.sleep(1)
  unlink(dir(pattern = fname), recursive = TRUE)
  
  invisible(TRUE)
}