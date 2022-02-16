

for(year in yearsToSubmit)  {
  
  print(year)
  
  load(file = paste0(outPath,paste0("cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste("cleanTacsat", year, ".RData")) )
  

# 2.1 Merge the TACSAT and EFLALO data together --------------------------------------------

  # Merge eflalo and tacsat =================================
   
  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  # Assign gear and length to tacsat =================================

   
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


  # Save not merged tacsat data = 
  
 
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

  
# 2.2  Define activity  ---------------------------------------------------------------------
  
 
  # Calculate time interval between points ===================================
  tacsatp <- intervalTacsat(tacsatp, level = "trip", fill.na = TRUE)
  
  # Reset values that are simply too high to 2x the regular interval rate  
  
  
  tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres

  
  # Remove points with NA's in them in critial places ========================
  
   
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

  
  # Define speed thresholds associated with fishing for gears =====================
  
   
  # Investigate speed pattern through visual inspection of histograms # 
  
  
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
  
  # Create speed threshold object # 
  
  speedarr <-
    as.data.frame(
      cbind(
        LE_GEAR = sort(unique(tacsatp$LE_GEAR)),
        min = NA,
        max = NA),
      stringsAsFactors = FALSE)
  speedarr$min <- rep(1, nrow(speedarr)) # It is important to fill out the personally inspected thresholds here!
  speedarr$max <- rep(6, nrow(speedarr))


 
  # Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
  
 

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

    
    # Fill the storeScheme values based on analyses of the pictures = 
    
   
    # Define mean values of the peaks and the number of peaks when they are different from 5 # 
    
     
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
  
  # Check results, and if results are not satisfactory, run analyses again but now with fixed peaks # 
 
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

  # Assign for visually inspected gears a simple speed rule classification =============== 
  
 

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

  
  # Combine the two dataset together again =============== 
  
  
  tacsatp <- rbindTacsat(subTacsat, nonsubTacsat)
  tacsatp <- orderBy( ~ VE_REF + SI_DATIM, data = tacsatp)
  
  # Set fishing sequences with hauling in the middle to "f" ##################
  
 
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
  
  
  
  # 2.3 Dispatch landings of merged eflalo at the ping scale  -------------------------------------------------
  
   
  idxkgeur <- kgeur(colnames(eflalo))
  eflalo$LE_KG_TOT <- rowSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
  eflalo$LE_EURO_TOT <- rowSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)
  eflalo <- eflalo[, -idxkgeur]
  eflaloNM <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo,FT_REF %in% unique(tacsatp$FT_REF))

  tacsatp$SI_STATE[which(tacsatp$SI_STATE != "f")] <- 0
  tacsatp$SI_STATE[which(tacsatp$SI_STATE == "f")] <- 1
  
  tacsatEflalo <- tacsatEflalo[tacsatEflalo$SI_STATE == 1,] 
  

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
              conserve = FALSE
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

  
  # 2.4 Assign c-square, year, month, quarter, area and create table 1 ----------------------------------------
   

  tacsatEflalo$Csquare   <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  tacsatEflalo$Year      <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month     <- month(tacsatEflalo$SI_DATIM)
  tacsatEflalo$kwHour    <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV      <- tacsatEflalo$INTV / 60
  

  RecordType <- "VE"

  if(year == yearsToSubmit[1]) {
    table1 <-
      cbind(
        RT = RecordType,
        tacsatEflalo[,
          c(
            "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
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
              "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
              "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"
            )
          ])
      )
    
  }
  
  
  # Save table1   ====================
  
  
  
  save(
    table1,
    file = file.path(outPath, "table1.RData" )
  )
  
  message(glue ("Table 1 for year {year} is completed") )

  
  # 2.5 Assign  year, month, quarter, area and create table 2 ----------------------------------------
  
 

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
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatp$FT_REF, "Y", "N") # Y = Yes and N = No

 

  RecordType <- "LE"

  if (year == yearsToSubmit[1]) {
    
    table2 <-
      cbind(
        RT = RecordType,
        eflalo[
          ,
          c(
            "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
            "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
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
              "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
            )
          ]
        )
      )
    
  }
  
  
  

  # Save table2   ====================


  
  save(
    table2,
    file = file.path(outPath, "table2.RData" )
  )
  
  message(glue ("Table 2 for year {year} is completed") )
  
  
  
  
}


 



 
