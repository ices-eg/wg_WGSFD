
writeDataHandling <- function(le_fname, ve_fname) {
sprintf(  
"<!------------------------------------------------------------------------------
Data handling
---------------------------------------------------------------------------- -->
```{r, echo=FALSE, results='asis'}
#Read in latest submission -->
ICES_LE <- read.csv('%s', stringsAsFactors=FALSE, sep=';', na.strings = 'NULL')
ICES_VE <- read.csv('%s', stringsAsFactors=FALSE, sep=';', na.strings = 'NULL')
```
",
le_fname, ve_fname)
}


# read from template file
qc <- paste(readLines("QC/QC.Rmd"), collapse = "\n")
qc1 <- strsplit(qc, "<!-- QCTEMPLATE: header -->")[[1]]
qc2 <- strsplit(qc1[2], "<!-- QCTEMPLATE: data -->")[[1]]
qc3 <- strsplit(qc2[2], "<!-- QCTEMPLATE: body -->")[[1]]
qc <- c(qc1[1], qc2[1], qc3)

# update template with country info

country <- "PRT"

fname <- paste0("QC/QC_", country,".Rmd")
qc[1] <- 
  gsub("ICES VMS datacall quality check report", 
     paste0("ICES VMS datacall quality check report for", country), qc[1])

qc[3] <- writeDataHandling("data/ICES_LE_PRT_2016.csv", "data/ICES_VMS_PRT_2016.csv")

# write template
cat(qc, sep = "\n", file = fname)

# run template
rmarkdown::render(fname, knit_root_dir = "../")
