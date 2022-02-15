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
  
  
   #- Take only eflalo records in ICES areas
  
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
            tril(
              matrix(
                as.numeric(
                  outer(x$FT_DDATIM, x$FT_LDATIM, "-")
                ),
                nrow = nrow(x), ncol = nrow(x)
              ),-1),
            2,
            function(y) {
              which(y < 0, arr.ind = TRUE)
            }
          )
        
        rows <- which(unlist(lapply(idx, length)) > 0) # first part of the overlapping trips
        if(length(rows)>0){
          cols = c()
          
          for(k in 1:length(rows)){
            cols <-c(cols, idx[[rows[k]]] ) # second part of the overlapping trips
          }
          
          x[unique(c(rows, cols)),1:15] # returns all the overlapping trips for the given VE_REF
          #rownames(x[unique(c(rows, cols)),]) # either the line above or this one, not sure which is better
          
        }else{
          data.frame()
        }
        
      })
  
  overlappingTrips = data.frame()
  for (iOver in 1:length(overlaps)) {
    if (nrow(overlaps[[iOver]]) > 0) { # if there are overlapping trips for the given VE_REF put them all into one file
      
      overlappingTrips = rbind(overlappingTrips, overlaps[[iOver]] )      # returns all overlapping trips
      
    }
  }
  
  if(nrow(overlappingTrips>0)){
    
    print("THERE ARE OVERLAPPING TRIPS IN THE DATASET -> SEE THE FILE overlappingTrip SAVED IN THE RESULTS FOLDER")
    
    save(
      overlappingTrips,
      file = file.path(outPath, paste0("overlappingTrips", year, ".RData"))
    )
  }
  
  
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
} 
