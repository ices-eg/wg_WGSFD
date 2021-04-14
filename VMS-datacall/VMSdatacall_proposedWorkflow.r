#-------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
#
# By: Niels Hintzen, Katell Hamon, Marcel Machiels#
# Code by: Niels Hintzen
# Contact: niels.hintzen@wur.nl
#
# Date: 25-Jan-2017
# Update Date: 29-Jan-2019 ; Updated by: Roi Martinez
# Update Date: 04-Feb-2020 ; Updated by: Colin Millar
# Update Date: 07-Feb 2020 ; Updated by: Neil Campbell
# Client: ICES
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is a proposed workflow example to processes the ICES
# VMS datacall request. It is not an exact template to be applied to data from
# every member state and needs to be adjusted according to the data availability
# and needs of every member state.
#-------------------------------------------------------------------------------


#- Clear workspace
rm(list=ls())

library(vmstools) #- download from www.vmstools.org
library(Matrix)   #- available on CRAN
library(ggplot2)  #- available on CRAN
library(dplyr)    #- available on CRAN
library(sp)
library(doBy)
library(mixtools)
library(dplyr)
library(tidyverse)

#- Settings paths
codePath  <- "VMSdatacall/R"          #Location where you store R scripts
dataPath  <- "VMSdatacall/Data"       #Location where you store tacsat (VMS) and eflalo (logbook) data
outPath   <- "VMSdatacall/Results"    #Location where you want to store the results

#- Setting specific thresholds
spThres       <- 20   #Maximum speed threshold in analyses in nm
intThres      <- 5    #Minimum difference in time interval in minutes to prevent pseudo duplicates
intvThres     <- 240  #Maximum difference in time interval in minutes to prevent intervals being too large to be realistic
lanThres      <- 1.5  #Maximum difference in log10-transformed sorted weights

#- Re-run all years as we have new field for no. vessels
yearsToSubmit <- sort(2009:2020)

#- Set the gear names for which automatic fishing activity is wanted
#  It is important to fill out the gears you want to apply auto detection for
autoDetectionGears        <- c("TBB","OTB","OTT","SSC","SDN","DRB","PTB","HMD")

#- Decide if you want to visualy analyse speed-histograms to identify fishing activity
#  peaks or have prior knowledge and use the template provided around lines 380 below
visualInspection          <- FALSE

#- Specify how landings should be distributed over the VMS pings: By day, ICES rectangle, trip basis or otherwise
linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# other options
# linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# linkEflaloTacsat          <- c("ICESrectangle","trip")
# linkEflaloTacsat          <- c("day","trip")
# linkEflaloTacsat          <- c("trip")


#-------------------------------------------------------------------------------
#- 1) Load the data
#-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  #- 1a) load vmstools underlying data
  #-------------------------------------------------------------------------------
  # data(euharbours); if(substr(R.Version()$os,1,3)== "lin")
  data(harbours)
  data(ICESareas)
  data(europa)

  eorpa <- NULL
  for(d in 1:1991){
    eorpa <- rbind(eorpa, europa@polygons[[d]]@Polygons[[1]]@coords, c(NA, NA))
  }


  #-------------------------------------------------------------------------------
  #- 1b) Looping through the data years
  #-------------------------------------------------------------------------------

for(year in yearsToSubmit){
  print(year)
  #-------------------------------------------------------------------------------
  #- 1c) load tacsat and eflalo data from file (they need to be in tacsat2 and
  #       eflalo2 format already
  #       (see https://github.com/nielshintzen/vmstools/releases/ -> downloads ->
  #                     Exchange_EFLALO2_v2-1.doc for an example
  #-------------------------------------------------------------------------------
  tacsat_name <-
    load(
      file.path(
        dataPath,
        paste0("tacsat_", year, ".RData")
      )); #- data is saved as tacsat_2009, tacsat_2010 etc
  eflalo_name <-
    load(
      file.path(
        dataPath,
        paste0("eflalo_", year, ".RData")
      )); #- data is saved as eflalo_2009, eflalo_2010 etc

  tacsat <- get(tacsat_name) # rename to tacsat
  eflalo <- get(eflalo_name) # rename to eflalo

  #- Make sure data is in right format
  tacsat <- formatTacsat(tacsat)
  eflalo <- formatEflalo(eflalo)

  #- Take only VMS pings in the ICES areas
  ICESareas <- as(ICESareas, "SpatialPolygons")
  idxI <-
    over(
      SpatialPoints(
        tacsat[, c("SI_LONG", "SI_LATI")],
        CRS(proj4string(ICESareas))
      ),
      ICESareas
    )
  tacsat <- tacsat[which(idxI > 0), ]

  coordsEflalo <- ICESrectangle2LonLat( na.omit( unique( eflalo$LE_RECT )))
  coordsEflalo$LE_RECT <- na.omit( unique( eflalo$LE_RECT ))
  coordsEflalo <-
    coordsEflalo[
      is.na( coordsEflalo[, 1] ) == FALSE |
      is.na( coordsEflalo[, 2] ) == FALSE,
    ]

  cornerPoints <- list()
  for(i in 1:nrow(coordsEflalo)) {
    cornerPoints[[i]] <-
      cbind(
        SI_LONG = coordsEflalo[i, "SI_LONG"] + c(0, 0.5, 1, 1, 0),
        SI_LATI = coordsEflalo[i, "SI_LATI"] + c(0, 0.25, 0, 0.5, 0.5),
        LE_RECT = coordsEflalo[i, "LE_RECT"]
      )
  }

  coordsEflalo <-
    as.data.frame(
      do.call(
        rbind,
        cornerPoints
      ),
      stringsAsFactors = FALSE
    )
  coordsEflalo$SI_LONG <- as.numeric(coordsEflalo$SI_LONG)
  coordsEflalo$SI_LATI <- as.numeric(coordsEflalo$SI_LATI)
  idxI <-
    over(
      SpatialPoints(
        coordsEflalo[, c("SI_LONG", "SI_LATI")],
        CRS(proj4string(ICESareas))),
      ICESareas
    )
  eflalo <-
    subset(
      eflalo,
      LE_RECT %in% unique(coordsEflalo[which(idxI > 0), "LE_RECT"])
    )

#-------------------------------------------------------------------------------
#- 2) Clean the tacsat data
#-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  #- Keep track of removed points
  #-------------------------------------------------------------------------------
  remrecsTacsat <-
    matrix(
      NA,
      nrow = 6, ncol = 2,
      dimnames =
        list(
          c("total", "duplicates", "notPossible", "pseudoDuplicates", "harbour", "land"),
          c("rows", "percentage"))
    )
  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%")

  #-------------------------------------------------------------------------------
  #- Remove duplicate records
  #-------------------------------------------------------------------------------
  tacsat$SI_DATIM <-
    as.POSIXct(
      paste(tacsat$SI_DATE, tacsat$SI_TIME),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M"
    )
  uniqueTacsat <-
    paste(tacsat$VE_REF, tacsat$SI_LATI, tacsat$SI_LONG, tacsat$SI_DATIM)
  tacsat <- tacsat[!duplicated(uniqueTacsat), ]
  remrecsTacsat["duplicates",] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
          as.numeric(remrecsTacsat["total", 1]) *
          100,
        2)
    )

  #-------------------------------------------------------------------------------
  #- Remove points that cannot be possible
  #-------------------------------------------------------------------------------
  idx <- which(abs(tacsat$SI_LATI) > 90 | abs(tacsat$SI_LONG) > 180)
  idx <- unique(c(idx, which(tacsat$SI_HE < 0 | tacsat$SI_HE > 360)))
  idx <- unique(c(idx, which(tacsat$SI_SP > spThres)))
  if (length(idx) > 0) tacsat <- tacsat[-idx,]
  remrecsTacsat["notPossible",] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total",1])) /
          as.numeric(remrecsTacsat["total",1]) *
          100,
        2)
    )

  #-------------------------------------------------------------------------------
  #- Remove points which are pseudo duplicates as they have an interval rate < x minutes
  #-------------------------------------------------------------------------------
  tacsat <- sortTacsat(tacsat)
  tacsatp <- intervalTacsat(tacsat, level = "vessel", fill.na = TRUE)
  tacsat <-
    tacsatp[
      which(
        tacsatp$INTV > intThres |
        is.na(tacsatp$INTV) == TRUE) ,
      -grep("INTV", colnames(tacsatp))
    ]
  remrecsTacsat["pseudoDuplicates",] <-
    c(
      nrow(tacsat),
      100 +
      round(
        (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
        as.numeric(remrecsTacsat["total", 1]) *
        100,
      2)
    )

  #-------------------------------------------------------------------------------
  #- Remove points in harbour
  #-------------------------------------------------------------------------------
  idx <-
    pointInHarbour(
      tacsat$SI_LONG,
      tacsat$SI_LATI,
      harbours)
  pih <- tacsat[which(idx == 1), ]
  save(pih, file = file.path(outPath, paste0("pointInHarbour", year, ".RData")))
  tacsat <- tacsat[which(idx == 0), ]
  remrecsTacsat["harbour",] <-
    c(
      nrow(tacsat),
      100 +
      round(
        (nrow(tacsat) - as.numeric(remrecsTacsat["total",1])) /
        as.numeric(remrecsTacsat["total",1]) *
        100,
      2)
    )

  #-------------------------------------------------------------------------------
  #- Remove points on land
  #-------------------------------------------------------------------------------
  idx <- point.in.polygon(
    point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
    pol.x = eorpa[, 1], pol.y = eorpa[, 2]
  )
  pol <- tacsat[idx > 0, ]
  save(
    pol,
    file = file.path(outPath, paste0("pointOnLand", year, ".RData"))
  )
  tacsat <- tacsat[which(idx == 0), ]
  remrecsTacsat["land", ] <-
    c(
      nrow(tacsat),
      100 +
      round(
        (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
        as.numeric(remrecsTacsat["total",1]) * 100,
      2)
    )

  #-------------------------------------------------------------------------------
  #- Save the remrecsTacsat file
  #-------------------------------------------------------------------------------
  save(
    remrecsTacsat,
    file = file.path(outPath, paste0("remrecsTacsat", year, ".RData"))
  )

  #-------------------------------------------------------------------------------
  #- Save the cleaned tacsat file
  #-------------------------------------------------------------------------------
  save(
    tacsat,
    file = file.path(outPath, paste("cleanTacsat", year, ".RData", sep = ""))
  )

  message("Cleaning tacsat completed")
#-------------------------------------------------------------------------------
#- 3) Clean the eflalo data
#-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  #- Keep track of removed points
  #-------------------------------------------------------------------------------
  remrecsEflalo <-
    matrix(
      NA,
      nrow = 5, ncol = 2,
      dimnames =
        list(
          c("total", "duplicated", "impossible time", "before 1st Jan", "departArrival"),
          c("rows", "percentage"))
    )
  remrecsEflalo["total", ] <- c(nrow(eflalo), "100%")

  #-------------------------------------------------------------------------------
  #- Warn for outlying catch records
  #-------------------------------------------------------------------------------

  # Put eflalo in order of 'non kg/eur' columns, then kg columns, then eur columns
  idxkg <- grep("LE_KG_", colnames(eflalo))
  idxeur <- grep("LE_EURO_", colnames(eflalo))
  idxoth <- which( !(1:ncol(eflalo)) %in% c(idxkg, idxeur) )
  eflalo <- eflalo[, c(idxoth, idxkg, idxeur)]

  #First get the species names in your eflalo dataset
  specs  <-
    substr(
      grep("KG", colnames(eflalo), value = TRUE),
      7, 9
    )

  # Define per species what the maximum allowed catch is
  # (larger than that value you expect it to be an error / outlier
  specBounds <-
    lapply(
      as.list(specs),
      function(x)
      {
        specs_cols <- grep(x, colnames(eflalo))
        idx <-
          specs_cols[
            grep("KG", colnames(eflalo)[specs_cols])
          ]
        wgh <-
          sort(
            unique(
              eflalo[which(eflalo[, idx] > 0), idx]
            ))
        difw <- diff(log10(wgh))
        ifelse(
          any(difw > lanThres),
          wgh[rev( which(difw <= lanThres) + 1)],
          ifelse(
            length(wgh) == 0,
            0,
            max(wgh, na.rm = TRUE)
          )
        )
      })

  #Make a list of the species names and the cut-off points / error / outlier point
  specBounds <- cbind(specs, unlist(specBounds))

  #Put these values to zero
  specBounds[which( is.na(specBounds[, 2]) == TRUE), 2] <- "0"

  #Get the index (column number) of each of the species
  idx <-
    unlist(
      lapply(
        as.list(specs),
        function(x)
        {
          specs_cols <- grep(x, colnames(eflalo))
          idx <-
            specs_cols[
              grep("KG", colnames(eflalo)[specs_cols])
            ]
            idx
        }
      ))

  #If landing > cut-off turn it into an 'NA'
  warns <- list()
  fixWarns <- TRUE
  for (iSpec in idx) {
    if (
      length(
        which(
          eflalo[, iSpec] >
          as.numeric(specBounds[(iSpec - idx[1] + 1), 2])
        )
      ) > 0)
    {
      warns[[iSpec]] <-
        which(
          eflalo[, iSpec] >
          as.numeric(specBounds[(iSpec - idx[1] + 1), 2])
        )
      if (fixWarns) {
        eflalo[
          which(
            eflalo[, iSpec] >
            as.numeric(specBounds[(iSpec - idx[1] + 1), 2])),
          iSpec
        ] <- NA
      }
    }
  }
  save(
    warns,
    file = file.path(outPath, paste0("warningsSpecBound", year, ".RData"))
  )

  #Turn all other NA's in the eflalo dataset in KG and EURO columns to zero
  for (i in kgeur(colnames(eflalo))) {
    eflalo[
      which(is.na(eflalo[, i]) == TRUE),
      i] <- 0
  }

  #-------------------------------------------------------------------------------
  #- Remove non-unique trip numbers
  #-------------------------------------------------------------------------------
  eflalo <-
    eflalo[
      !duplicated(paste(eflalo$LE_ID, eflalo$LE_CDAT, sep="-")),
    ]
  remrecsEflalo["duplicated",] <-
    c(
      nrow(eflalo),
      100 +
      round(
        (nrow(eflalo) - as.numeric(remrecsEflalo["total", 1])) /
        as.numeric(remrecsEflalo["total", 1]) * 100,
        2)
    )

  #-------------------------------------------------------------------------------
  #- Remove impossible time stamp records
  #-------------------------------------------------------------------------------
  eflalo$FT_DDATIM <-
    as.POSIXct(
      paste(eflalo$FT_DDAT, eflalo$FT_DTIME, sep = " "),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM <-
    as.POSIXct(
      paste(eflalo$FT_LDAT, eflalo$FT_LTIME, sep = " "),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M")

  eflalo <-
    eflalo[
      !(is.na(eflalo$FT_DDATIM) | is.na(eflalo$FT_LDATIM)),
    ]
  remrecsEflalo["impossible time",] <-
    c(
      nrow(eflalo),
      100 +
      round(
        (nrow(eflalo) - as.numeric(remrecsEflalo["total", 1])) /
        as.numeric(remrecsEflalo["total",1]) * 100,
        2)
    )

  #-------------------------------------------------------------------------------
  #- Remove trip starting befor 1st Jan
  #-------------------------------------------------------------------------------
  eflalo <-
    eflalo[
      eflalo$FT_DDATIM >=
        strptime(paste(year,"-01-01 00:00:00",sep=''), "%Y-%m-%d %H:%M"),
      ]
  remrecsEflalo["before 1st Jan",] <-
    c(
      nrow(eflalo),
      100 +
      round(
        (nrow(eflalo) - as.numeric(remrecsEflalo["total",1])) /
        as.numeric(remrecsEflalo["total", 1]) *
        100,
      2)
    )

  #-------------------------------------------------------------------------------
  #- Remove trip with overlap with another trip
  #-------------------------------------------------------------------------------

  eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)
  overlaps <-
    lapply(
      split(eflalo, as.factor(eflalo$VE_REF)),
      function(x)
      {
        x  <- x[!duplicated( paste(x$VE_REF, x$FT_REF)), ]
        idx <-
          apply(
            triu(
              matrix(
                as.numeric(
                  outer(x$FT_DDATIM, x$FT_LDATIM, "-")
                ),
                nrow = nrow(x), ncol = nrow(x)
              )),
            2,
            function(y) {
              which(y > 0, arr.ind = TRUE)
            }
          )
        rows <- which(unlist(lapply(idx, length)) > 0)
        rows
      })
  eflalo$ID <- 1:nrow(eflalo) # this doesn't work properly - needs some thinking
  #for (iOver in 1:length(overlaps)) {
  #  if (length(overlaps[[iOver]]) > 0) {
  #    eflalo <-
  #      eflalo[
  #        which(eflalo$VE_REF == names(overlaps)[iOver]),
  #      ]
  #    eflalo <- eflalo[-overlaps[[iOver]], ]
  #  }
  #}

  #-------------------------------------------------------------------------------
  #- Remove records with arrival date before departure date
  #-------------------------------------------------------------------------------
  eflalop <- eflalo
  idx <- which(eflalop$FT_LDATIM >= eflalop$FT_DDATIM)
  eflalo <- eflalo[idx,]
  remrecsEflalo["departArrival", ] <-
    c(
      nrow(eflalo),
      100 +
      round(
        (nrow(eflalo) - as.numeric(remrecsEflalo["total", 1])) /
        as.numeric(remrecsEflalo["total", 1]) *
        100,
      2)
    )

  #-------------------------------------------------------------------------------
  #- Save the remrecsEflalo file
  #-------------------------------------------------------------------------------
  save(
    remrecsEflalo,
    file = file.path(outPath, paste0("remrecsEflalo", year, ".RData"))
  )

  #-------------------------------------------------------------------------------
  #- Save the cleaned eflalo file
  #-------------------------------------------------------------------------------
  save(
    eflalo,
    file = file.path(outPath,paste0("cleanEflalo",year,".RData"))
  )

  message("Cleaning eflalo completed")
#-------------------------------------------------------------------------------
#- 4) Merge the tacsat and eflalo data together
#-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  #- Merge eflalo and tacsat
  #-------------------------------------------------------------------------------
  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)

  #-------------------------------------------------------------------------------
  #- Assign gear and length to tacsat
  #-------------------------------------------------------------------------------
  tacsatp$LE_GEAR  <- eflalo$LE_GEAR[ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_MSZ   <- eflalo$LE_MSZ[  match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$VE_LEN   <- eflalo$VE_LEN[  match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$VE_KW    <- eflalo$VE_KW[   match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_RECT  <- eflalo$LE_RECT[ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_MET   <- eflalo$LE_MET[  match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$VE_FLT   <- eflalo$VE_FLT[  match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_CDAT  <- eflalo$LE_CDAT[ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$VE_COU   <- eflalo$VE_COU[  match(tacsatp$FT_REF, eflalo$FT_REF)]


  #-------------------------------------------------------------------------------
  #- Save not merged tacsat data
  #-------------------------------------------------------------------------------
  tacsatpmin <- subset(tacsatp, FT_REF == 0)
  save(
    tacsatpmin,
    file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
  )

  tacsatp <- subset(tacsatp,FT_REF != 0)
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )

#-------------------------------------------------------------------------------
#- 5) Define activitity
#-------------------------------------------------------------------------------

  #- Calculate time interval between points
  tacsatp <- intervalTacsat(tacsatp, level = "trip", fill.na = TRUE)
  #- Reset values that are simply too high to 2x the regular interval rate
  tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres

  #-------------------------------------------------------------------------------
  #- Remove points with NA's in them in critial places
  #-------------------------------------------------------------------------------
  idx <-
    which(
      is.na(tacsatp$VE_REF) == TRUE |
      is.na(tacsatp$SI_LONG) == TRUE |
      is.na(tacsatp$SI_LATI) == TRUE |
      is.na(tacsatp$SI_DATIM) == TRUE |
      is.na(tacsatp$SI_SP) == TRUE
    )
  if (length(idx) > 0) {
    tacsatp <- tacsatp[-idx, ]
  }

  #-------------------------------------------------------------------------------
  #- Define speed thresholds associated with fishing for gears
  #-------------------------------------------------------------------------------

  #- Investigate speed pattern through visual inspection of histograms
  png(filename = file.path(outPath, paste0("SpeedHistogram_", year, ".png")))
  ggplot(data = tacsatp, aes(SI_SP)) +
    geom_histogram(
      breaks = seq(0, 20, by =0.4), col = 1) +
    facet_wrap( ~ LE_GEAR, ncol = 4, scales = "free_y") +
    labs(x = "Speed (knots)", y = "Frequency") +
    theme(
      axis.text.y = element_text(colour = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA)
    )
  dev.off()
  #- Create speed threshold object
  speedarr <-
    as.data.frame(
      cbind(
        LE_GEAR = sort(unique(tacsatp$LE_GEAR)),
        min = NA,
        max = NA),
      stringsAsFactors = FALSE)
  speedarr$min <- rep(1, nrow(speedarr)) # It is important to fill out the personally inspected thresholds here!
  speedarr$max <- rep(6, nrow(speedarr))


  #-------------------------------------------------------------------------------
  #- Analyse activity automated for common gears only. Use the speedarr for the other gears
  #-------------------------------------------------------------------------------

  subTacsat <- subset(tacsatp, LE_GEAR %in% autoDetectionGears)
  nonsubTacsat <- subset(tacsatp, !LE_GEAR %in% autoDetectionGears)

  if (visualInspection == TRUE)
  {
    storeScheme <-
      activityTacsatAnalyse(
        subTacsat,
        units = "year",
        analyse.by = "LE_GEAR",
        identify = "means")
  } else
  {
    storeScheme <-
      expand.grid(
        years = year,
        months = 0,
        weeks = 0,
        analyse.by = unique(subTacsat[, "LE_GEAR"])
      )
    storeScheme$peaks <- NA
    storeScheme$means <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911

    #-------------------------------------------------------------------------------
    #- Fill the storeScheme values based on analyses of the pictures
    #-------------------------------------------------------------------------------

    #- Define mean values of the peaks and the number of peaks when they are different from 5
    storeScheme$means[which(storeScheme$analyse.by == "TBB")] <- c("-11.5 -6 0 6 11.5")
    storeScheme$means[which(storeScheme$analyse.by == "OTB")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$analyse.by == "OTT")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$analyse.by == "SSC")] <- c("-9 0 9")
    storeScheme$means[which(storeScheme$analyse.by == "PTB")] <- c("-10 -3 0 3 10")
    storeScheme$means[which(storeScheme$analyse.by == "DRB")] <- c("-10 0 10")
    storeScheme$means[which(storeScheme$analyse.by == "HMD")] <- c("-9 0 9")
    storeScheme$peaks[which(storeScheme$analyse.by == "SSC")] <- 3
    storeScheme$peaks[which(storeScheme$analyse.by == "DRB")] <- 3
    storeScheme$peaks[which(storeScheme$analyse.by == "HMD")] <- 3
    storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
  }

  acTa <-
    activityTacsat(
      subTacsat,
      units = "year",
      analyse.by = "LE_GEAR",
      storeScheme = storeScheme,
      plot = FALSE,
      level = "all")
  subTacsat$SI_STATE <- acTa
  subTacsat$ID <- 1:nrow(subTacsat)

  #- Check results, and if results are not satisfactory, run analyses again but now
  #   with fixed peaks
  for (iGear in autoDetectionGears) {
    subDat <- subset(subTacsat,LE_GEAR == iGear)
    minS <-
      min(
        subDat$SI_SP[which(subDat$SI_STATE == "s")],
        na.rm = TRUE)
    minF <-
      min(subDat$SI_SP[which(subDat$SI_STATE == "f")],
      na.rm = TRUE)
    if(minS < minF) {
      storeScheme$fixPeaks[which(storeScheme$analyse.by == iGear)] <- TRUE
      subacTa <-
        activityTacsat(
          subDat,
          units = "year",
          analyse.by = "LE_GEAR",
          storeScheme,
          plot = FALSE,
          level = "all"
        )
      subTacsat$SI_STATE[subDat$ID] <- subacTa
    }
  }
  subTacsat <-
    subTacsat[,
      -rev(grep("ID", colnames(subTacsat)))[1]
    ]

  #-------------------------------------------------------------------------------
  #- Assign for visually inspected gears a simple speed rule classification
  #-------------------------------------------------------------------------------

  metiers <- unique(nonsubTacsat$LE_GEAR)
  nonsubTacsat$SI_STATE <- NA
  for (mm in metiers) {
    nonsubTacsat$SI_STATE[
      nonsubTacsat$LE_GEAR == mm &
      nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == mm, "min"] &
      nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == mm, "max"]
    ] <- "f";
  }
  nonsubTacsat$SI_STATE[
    nonsubTacsat$LE_GEAR == "NA" &
    nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == "MIS", "min"] &
    nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == "MIS", "max"]
  ] <- "f"
  nonsubTacsat$SI_STATE[ is.na(nonsubTacsat$SI_STATE) ] <- "s"

  #-------------------------------------------------------------------------------
  #- Combine the two dataset together again
  #-------------------------------------------------------------------------------
  tacsatp <- rbindTacsat(subTacsat, nonsubTacsat)
  tacsatp <- orderBy( ~ VE_REF + SI_DATIM, data = tacsatp)
  #- Set fishing sequences with hauling in the middle to "f"
  idx <-
    which(
      tacsatp$SI_STATE[2:(nrow(tacsatp) - 1)] == "h" &
      tacsatp$SI_STATE[1:(nrow(tacsatp) - 2)] == "f" &
      tacsatp$SI_STATE[3:(nrow(tacsatp))    ] == "f" &
      tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[1:(nrow(tacsatp) - 2)] &
      tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[3:(nrow(tacsatp))]
    ) + 1
  tacsatp$SI_STATE[idx] <- "f"

  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatActivity", year, ".RData"))
  )

  message("Defining activity completed")
#-------------------------------------------------------------------------------
#- 6) Dispatch landings of merged eflalo at the ping scale
#-------------------------------------------------------------------------------
  idxkgeur <- kgeur(colnames(eflalo))
  eflalo$LE_KG_TOT <- rowSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
  eflalo$LE_EURO_TOT <- rowSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)
  eflalo <- eflalo[, -idxkgeur]
  eflaloNM <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo,FT_REF %in% unique(tacsatp$FT_REF))

  tacsatp$SI_STATE[which(tacsatp$SI_STATE != "f")] <- 0
  tacsatp$SI_STATE[which(tacsatp$SI_STATE == "f")] <- 1

  #- There are several options, specify at the top of this script what type of linking you require
  if (!"trip" %in% linkEflaloTacsat) stop("trip must be in linkEflaloTacsat")
  if (all(c("day", "ICESrectangle", "trip") %in% linkEflaloTacsat)) {
    tacsatEflalo <-
      splitAmongPings(
        tacsat = tacsatp,
        eflalo = eflaloM,
        variable = "all",
        level = "day",
        conserve = TRUE
      )
  } else
  {
    if (
      all(c("day","trip") %in% linkEflaloTacsat) &
      !"ICESrectangle" %in% linkEflaloTacsat
    ) {
      tmpTa <- tacsatp
      tmpEf <- eflaloM
      tmpTa$LE_RECT <- "ALL"
      tmpEf$LE_RECT <- "ALL"
      tacsatEflalo <-
        splitAmongPings(
          tacsat = tmpTa,
          eflalo = tmpEf,
          variable = "all",
          level = "day",
          conserve = TRUE
        )
    } else
    {
      if (
        all(c("ICESrectangle", "trip") %in% linkEflaloTacsat) &
        !"day" %in% linkEflaloTacsat
      )
      {
        tacsatEflalo <-
          splitAmongPings(
            tacsat = tacsatp,
            eflalo = eflaloM,
            variable = "all",
            level = "ICESrectangle",
            conserve = TRUE
          )
      } else
      {
        if (linkEflaloTacsat == "trip" & length(linkEflaloTacsat) == 1)
        {
          tacsatEflalo <-
            splitAmongPings(
              tacsat = tacsatp,
              eflalo = eflaloM,
              variable = "all",
              level = "trip",
              conserve = TRUE
            )
        }
      }
    }
  }

  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )

  print("Dispatching landings completed")

#-------------------------------------------------------------------------------
#- 7) Assign c-square, year, month, quarter, area and create table 1
#-------------------------------------------------------------------------------

  tacsatEflalo$Csquare   <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  tacsatEflalo$Year      <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month     <- month(tacsatEflalo$SI_DATIM)
  tacsatEflalo$kwHour    <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV      <- tacsatEflalo$INTV / 60
  tacsatEflalo$LENGTHCAT <- cut(tacsatEflalo$VE_LEN, breaks=c(0, 8, 10, 12, 15, 200))
  tacsatEflalo$LENGTHCAT <- as.character(tacsatEflalo$LENGTHCAT)
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(0,8]")] <- "<8"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(8,10]")] <- "8-10"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(10,12]")] <- "10-12"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(12,15]")] <- "12-15"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(15,200]")] <- ">15"

  RecordType <- "VE"

  if(year == yearsToSubmit[1]) {
    table1 <-
      cbind(
        RT = RecordType,
        tacsatEflalo[,
          c(
            "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LENGTHCAT", "LE_GEAR",
            "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"
          )
        ])
  } else {
    table1 <-
      rbind(
        table1,
        cbind(
          RT = RecordType,
          tacsatEflalo[,
            c(
              "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LENGTHCAT", "LE_GEAR",
              "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"
            )
          ])
      )
  }

#-------------------------------------------------------------------------------
#- 8) Assign  year, month, quarter, area and create table 2
#-------------------------------------------------------------------------------

  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)
  eflalo$INTV <- 1 # 1 day
  eflalo$dummy <- 1
  res <-
    aggregate(
      eflalo$dummy,
      by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
      FUN = sum,
      na.rm <- TRUE
    )
  colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")
  eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))
  eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
  eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatp$FT_REF, "Yes", "No")

  eflalo$LENGTHCAT <- cut(eflalo$VE_LEN, breaks = c(0, 8, 10, 12, 15, 200))
  eflalo$LENGTHCAT <- ac(eflalo$LENGTHCAT)
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(0,8]")] <- "<8"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(8,10]")] <- "8-10"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(10,12]")] <- "10-12"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(12,15]")] <- "12-15"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(15,200]")] <- ">15"

  RecordType <- "LE"

  if (year == yearsToSubmit[1]) {
    table2 <-
      cbind(
        RT = RecordType,
        eflalo[
          ,
          c(
            "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
            "LENGTHCAT", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
          )
        ]
      )
  } else {
    table2 <-
      rbind(
        table2,
        cbind(
          RT = RecordType,
          eflalo[
            ,
            c(
              "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
              "LENGTHCAT", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
            )
          ]
        )
      )
  }
}


### this bit deals with the new field added for the 2020 datacall
# construct a table of annonymous vessel ids accross all years
VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!

# join onto data tables
table1 <- left_join(table1, VE_lut)
table2 <- left_join(table2, VE_lut)


# summarise output and save
table1Save <-
  table1 %>%separate(col = LE_MET ,   c("met4", "met5", "mesh" ), sep = '_', remove = FALSE)%>%separate(mesh , c("min", "max"))%>%
    group_by(RT,VE_COU,Year,Month,Csquare,LE_GEAR, met5, min, max, LE_MET,LENGTHCAT) %>%
    summarise(
      mean_si_sp = mean(SI_SP),
      sum_intv =sum(INTV),
      mean_ve_len = mean(VE_LEN),
      mean_ve_kf = mean(VE_KW),
      sum_kwHour = sum(kwHour),
      sum_le_kg_tot = sum(LE_KG_TOT),
      sum_le_euro_tot  = sum(LE_EURO_TOT),      
      n_vessels = n_distinct(VE_ID),
      vessel_ids =
        ifelse (
          n_distinct(VE_ID) < 3,
          paste(unique(VE_ID), collapse = ";"),
          NA_character_
        )
      ) %>%  relocate( n_vessels,vessel_ids, .before = Csquare)%>%
      mutate (AverageGearWidth = NA  )%>% ## If this information is available modify this line of the script. By default is assumed not existing gear width information
      as.data.frame()

colnames(table1Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
    "C-square","MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6",  "VesselLengthRange",
    "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
    "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth"
  )

table2Save <-
  table2 %>%separate(col = LE_MET ,   c("met4", "met5", "mesh" ), sep = '_', remove = FALSE)%>%separate(mesh , c("min", "max"))%>%
  group_by(
    RT, VE_COU, Year, Month, LE_RECT,LE_GEAR, met5, min, max, LE_MET, 
    LENGTHCAT, tripInTacsat
  ) %>%  
  summarise(
    sum_intv = sum(INTV, na.rm = TRUE),
    sum_kwDays = sum(kwDays, na.rm = TRUE),
    sum_le_kg_tot = sum(LE_KG_TOT, na.rm = TRUE),
    sum_le_euro_tot = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids =
      ifelse (
        n_distinct(VE_ID) < 3,
        paste(
          unique(VE_ID), collapse = ";"),
          NA_character_
        )
  ) %>%  relocate( n_vessels,vessel_ids, .before = LE_RECT)%>%
as.data.frame()

colnames(table2Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID", "ICESrectangle",
    "MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6", "VesselLengthRange", "VMSEnabled", "FishingDays",
    "kWFishingDays", "TotWeight", "TotValue" 
  )

## Save the final table 1 and table 2 . Headers and quotes have been removed to be compatible with required submission format.  
write.table(table1Save, file.path(outPath, "table1Save.csv"),row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)
write.table(table2Save, file.path(outPath, "table2Save.csv"),row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)

