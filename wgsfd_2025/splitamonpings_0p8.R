####OLD SPLIT AMONG PINGS VERSIONS ##############

#' Split values or landings from eflalo over tacsat pings
#' 
#' Split the values or landings as listed in the eflalo file over the tacsat
#' pings, while taking different levels into account such as by day,
#' ICESrectangle or by trip number. Also there is a possibility to merge the
#' eflalo records without a matching tacsat trip.
#' 
#' Levels have hierachy, so if "day" is specified, also "ICESrectangle" and
#' "trip" will be used. If "ICESrectangle" is specified also "trip" will be
#' used. "Trip" can be used on its own. Same hierachy applies to merging when
#' conserve = TRUE (except for trip level).
#' 
#' Note that tacsat file needs a column SI_STATE which has 0 for non-fishing
#' and 1 for fishing records.
#' 
#' @param tacsat Tacsat object
#' @param eflalo Eflalo object
#' @param variable Indicating what to split: "all","value","kgs"
#' @param level Levels can be: "day", "ICESrectangle", "trip" or a combination. Include all levels that are needed.
#' @param conserve Logical, if kgs or value needs to be conserved if merging by
#' trip number is not possible (default = TRUE)
#' @param by Name of tacsat column by which KG and EURO should be dispatched.
#' Default to NULL which distributes KG and EURO equally by each ping. A tacsat
#' column can be used instead to generate a 'weighted' dispatch of KG and EURO.
#' @param returnAll Logical, whether all non-fishing pings should be returned
#' as well (default = FALSE)
#' @return Merged tacsat file will be returned including the splitted values
#' over the tacsat pings where SI_STATE is not zero.
#' @author Niels T. Hintzen, Francois Bastardie
#' @seealso \code{\link{mergeEflalo2Tacsat}}, \code{\link{mergeEflalo2Pings}}
#' @references EU Lot 2 project
#' @examples
#' 
#' data(tacsat); tacsat <- tacsat[1:1000,]
#' data(eflalo); eflalo <- eflalo[1:1000,]
#' 
#' tacsatp           <- mergeEflalo2Tacsat(eflalo,tacsat)
#' 
#' #- Create a column names SI_STATE which holds values 0 or 1 which denotes no
#' #  fishing & fishing.
#' tacsatp$IDX       <- 1:nrow(tacsatp)
#' tacsatFilter      <- filterTacsat(tacsatp,st=c(1,6),hd=NULL,remDup=TRUE)
#' tacsatp$SI_STATE  <- 0
#' tacsatp$SI_STATE[tacsatFilter$IDX] <- 1
#' 
#' #-Add interval to tacsatp
#' tacsatp           <- intervalTacsat(tacsatp,level="trip",fill.na=TRUE)
#' 
#' tacsatp           <- subset(tacsatp,SI_STATE == 1)
#' 
#' tacsatEflalo      <- splitAmongPings(tacsat=tacsatp,eflalo=eflalo,
#'                         variable="all",level=c("day","ICESrectangle","trip"),conserve=TRUE)
#'                         
#'                         
#' #- When using the 'by' statement, make sure the by column does not contain NA,
#' #  or zeros
#' tacsatp           <- subset(tacsatp,!is.na(tacsatp$INTV) | tacsatp$INTV != 0)
#' tacsatEflalo      <- splitAmongPings(tacsat=tacsatp,eflalo=eflalo,
#'                         variable="all",level=c("day","trip"),conserve=TRUE,by="INTV")
#' 
#' @export splitAmongPings




### splitAmongPings_0p77  ########################



splitAmongPings_0p77 <- function(tacsat,eflalo,variable="all",level="day",conserve =TRUE, conserve_ML2 = TRUE ,by=NULL,returnAll=F){
  
  
  # tacsat = tacsatp |>  filter  ( VE_REF== 'A13171' & SI_FT == '900068325688' )
  # eflalo = eflaloM |>  filter ( FT_REF == '900068325688')
  # 
  # 
  # library(sf)
  # tacsatTrip |>  arrange(SI_DATIM) |> 
  #   st_as_sf( coords = c("SI_LONG", "SI_LATI"), crs = 4326, remove = F) |> 
  #  
  #   #st_combine() %>% 
  #   # group_by(ID) |> 
  #   # summarize(do_union=FALSE) |>   # do_union=FALSE doesn't work as well
  #   # st_cast("LINESTRING") |> print()
  # ggplot() + geom_sf(aes( color = as.factor(ID) ))  +                            # Plot points
  #   geom_text(aes(x= SI_LONG, y = SI_LATI,label = ID), vjust = 1)  +
  #   geom_line(aes( x= SI_LONG, y = SI_LATI))
  
  require(data.table)
  require(lubridate)
  #level: day,ICESrectangle,trip
  #conserve: T,F
  #variable: kgs,value,effort
  
  if(!"FT_DDATIM" %in% colnames(eflalo)) eflalo$FT_DDATIM   <- as.POSIXct(paste(eflalo$FT_DDAT, eflalo$FT_DTIME, sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
  if(!"FT_LDATIM" %in% colnames(eflalo)) eflalo$FT_LDATIM   <- as.POSIXct(paste(eflalo$FT_LDAT, eflalo$FT_LTIME, sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
  
  acrossYear <- which(year(eflalo$FT_DDATIM) != year(eflalo$FT_LDATIM))
  if(length(acrossYear)>0)
    warning("There are trips that cross the year. This is not accounted for in splitAmongPings. Consider splitting those trips into half")
  
  #- Create extra columns with time stamps
  if(!"FT_REF" %in% colnames(tacsat)) stop("tacsat file needs FT_REF detailing trip number")
  if(!"SI_STATE" %in% colnames(tacsat)) stop("tacsat file needs SI_STATE detailing activity of vessel")
  if(all(!c("day","ICESrectangle") %in% level) & conserve == TRUE) stop("conserve catches only at level = ICESrectangle or day")
  
  if(!"SI_DATIM" %in%   colnames(tacsat)) tacsat$SI_DATIM     <- as.POSIXct(paste(tacsat$SI_DATE,   tacsat$SI_TIME,     sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
  if(!"LE_CDATIM" %in%  colnames(eflalo)) eflalo$LE_CDATIM    <- as.POSIXct(eflalo$LE_CDAT,                                     tz="GMT", format="%d/%m/%Y")
  
  if(is.null(by)==FALSE){
    if(any(is.na(tacsat[,by])) | any(tacsat[,by] == 0)) stop("'by' column in tacsat contains NA or zero's. Cannot execute with NA's or zeros")
  }
  
  #- Levels have hierachy, and need to be suplemented with lower levels
  
  if(length(which(!level %in% c("day","ICESrectangle","trip")))>0)
    stop(level[which(!level %in% c("day","ICESrectangle","trip"))])
  
  #- Add ID to keep track of merged and non-merged sets
  tacsat$ID               <- 1:nrow(tacsat)  ## create TACSAT ID's 
  
  #- identifyers of eflalo colnames
  eflaloCol               <- colnames(eflalo)
  kgs                     <- grep("LE_KG",colnames(eflalo))
  eur                     <- grep("LE_EURO",colnames(eflalo))
  
  #- Subset tacsat file
  remtacsat               <- subset(tacsat,SI_STATE == 0 ) # If the input TACSAT include also fishign pings as steaming. Only added at the end
  tacsat                  <- subset(tacsat,SI_STATE != 0) #only attribute variable to fishing pings
  tacsatTrip              <- subset(tacsat,FT_REF != 0)  ###only get teh tacsata with assigned trip 
  remainTacsat            <- sort(unique(tacsatTrip$ID))  ## TACSAT ID's vector ( ID create as a sequence by unique records) 
  
  #- Subset eflalo file
  eflalo$ID               <- 1:nrow(eflalo)
  
  ##subset only  the EFLALO recods  with common TRIPS/VESSEL IN TACSAT 
  eflaloTrip              <- subset(eflalo, FT_REF %in% sort(unique(tacsatTrip$FT_REF)) & VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  #eflaloNoTrip            <- subset(eflalo,      !FT_REF %in% sort(unique(tacsatTrip$FT_REF)))
  eflaloNoTrip            <- eflalo[which(!eflalo$ID %in% eflaloTrip$ID),-match("ID",colnames(eflalo))]
  #eflaloVessel            <- subset(eflaloNoTrip, VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  #eflaloNoVessel          <- subset(eflaloNoTrip,!VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  eflaloVessel            <- eflaloNoTrip[which(paste(eflaloNoTrip$VE_REF,format(eflaloNoTrip$LE_CDATIM,"%Y")) %in% unique(paste(tacsatTrip$VE_REF,format(tacsatTrip$SI_DATIM,"%Y")))),]
  eflaloNoVessel          <- eflaloNoTrip[which(!paste(eflaloNoTrip$VE_REF,format(eflaloNoTrip$LE_CDATIM,"%Y")) %in% unique(paste(tacsatTrip$VE_REF,format(tacsatTrip$SI_DATIM,"%Y")))),]
  
  
  
  #-------------------------------------------------------------------------------
  # 1a) Merge eflalo to tacsat with matching FT_REF
  #-------------------------------------------------------------------------------
  
  if(dim(tacsatTrip)[1]>0 & dim(eflaloTrip)[1] >0){
    
    if("day" %in% level){
      
      print("Match Level 0.a : Vessel + Fishing Trip + ICES Rect + Day")
      
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"SI_DAY" %in%  colnames(tacsatTrip))  tacsatTrip$SI_DAY     <- an(format(tacsatTrip$SI_DATIM,format="%j"))
      if(!"LE_RECT" %in% colnames(tacsatTrip))  tacsatTrip$LE_RECT    <- ICESrectangle(tacsatTrip)
      
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      if(!"SI_DAY" %in%  colnames(eflaloTrip))  eflaloTrip$SI_DAY     <- an(format(eflaloTrip$LE_CDATIM,format="%j"))
      
      #- Calculate the total SUM of VMS records BY attribute by GROUP BY variables in the  formula 
      nPings                <- countPings(~VE_REF+FT_REF+icesrectangle+day,tacsatTrip,by=by)
      
      #-Calculate the proportional weight of each VMS record and the total LE_KG and LE_EURO to each VMS record
      res           <- eflalo2Pings(eflalo = eflaloTrip,tacsat = tacsatTrip,pings = nPings,vars = c("VE_REF","FT_REF","LE_RECT","SI_DAY"),eflaloCol = eflaloCol[c(kgs,eur)],remainTacsat = remainTacsat,by=by)
     
      
      ## Save results to track results and modify datasets needed in the follow up Match Level 
      
      eflaloTrip    <- res[["eflalo"]]
      byDayTacsat   <- res[["tacsat"]]
      byDayTacsat = byDayTacsat |> mutate (match_level = "VE_REF,FT_REF,LE_RECT,SI_DAY")
      
      remainTacsat  <- res[["remainTacsat"]]
      stats_landings <- res[["stats_landings"]] 
      stats_VMS      <- res[["stats_VMS"]] 
      
      
    
      
      
    }
    
    if("ICESrectangle" %in% level){
      
      print("Match Level 0.b : Vessel + Fishing Trip + ICES Rect")
      
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"LE_RECT" %in% colnames(tacsatTrip))  tacsatTrip$LE_RECT    <- ICESrectangle(tacsatTrip)
      
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      
      #- Calculate the total SUM of VMS records BY attribute by GROUP BY variables in the  formula 
      nPings                <- countPings(~VE_REF+FT_REF+icesrectangle,tacsatTrip,by=by)
      
      #-Calculate the proportional weight of each VMS record and the total LE_KG and LE_EURO to each VMS record
      res           <- eflalo2Pings(eflaloTrip,tacsatTrip,nPings,c("VE_REF","FT_REF","LE_RECT"),eflaloCol[c(kgs,eur)],remainTacsat,by=by)
      
      ## Save results to track results and modify datasets needed in the follow up MAtch Level 
      
      eflaloTrip    <- res[["eflalo"]]      ## EFLALO modified with the landings distributed removed
      byRectTacsat  <- res[["tacsat"]]      ## The TACSAT with the calculated toal landings by GRPOUP and weight of each point
      byRectTacsat = byRectTacsat |> mutate (match_level = "VE_REF,FT_REF,LE_RECT") ## track teh levels of match 
      
      remainTacsat  <- res[["remainTacsat"]]   ## Remaining ID's of the TACSAT that have not been matched 
      stats_landings <- res[["stats_landings"]] |> rbind(stats_landings) ### Stats of distributed landings
      stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      
      
      }
    
    if("trip" %in% level){
      
      print("Match Level 0.c : Vessel + Fishing Trip")
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      
      #- Calculate the total SUM of VMS records BY attribute by GROUP BY variables in the  formula 
      nPings                <- countPings(~VE_REF+FT_REF,tacsatTrip,by=by)
      
      #-Calculate the proportional weight of each VMS record and the total LE_KG and LE_EURO to each VMS record
      res           <- eflalo2Pings(eflaloTrip,tacsatTrip,nPings,c("VE_REF","FT_REF"),eflaloCol[c(kgs,eur)],remainTacsat,by=by)
     
      eflaloTrip    <- res[["eflalo"]]
      byTripTacsat  <- res[["tacsat"]]
      byTripTacsat = byTripTacsat |> mutate (match_level = "VE_REF,FT_REF")
      
      remainTacsat  <- res[["remainTacsat"]]
      stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
      stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
    }
    
    
    
    #-------------------------------------------------------------------------------
    # 1b) Bind all TACSAT datasets for each Match Level   of records with common  FT_REF
    #-------------------------------------------------------------------------------
    
    if(length(remainTacsat) > 0) warning("Not all tacsat records with tripnumber have been merged!!")
    if(nrow(eflaloTrip) > 0) warning("Not all eflalo records with matching VMS tripnumber have been merged!!")
    
    ### 1. Combine all the TACSAT results of efalo2pings function at each Match Level 0 
    
    if("day"  %in% level & !"ICESrectangle" %in% level & !"trip" %in% level) tacsatFTREF <- byDayTacsat
    if(!"day" %in% level &  "ICESrectangle" %in% level & !"trip" %in% level) tacsatFTREF <- byRectTacsat
    if(!"day" %in% level & !"ICESrectangle" %in% level &  "trip" %in% level) tacsatFTREF <- byTripTacsat
    if("day" %in% level &   "ICESrectangle" %in% level & !"trip" %in% level) tacsatFTREF <- rbind(byDayTacsat,byRectTacsat)
    if("day" %in% level &  !"ICESrectangle" %in% level &  "trip" %in% level) tacsatFTREF <- rbind(byDayTacsat,byTripTacsat)
    if(!"day" %in% level &  "ICESrectangle" %in% level &  "trip" %in% level) tacsatFTREF <- rbind(byRectTacsat,byTripTacsat)
    if("day" %in% level &   "ICESrectangle" %in% level &  "trip" %in% level) tacsatFTREF <- rbind(byDayTacsat,byRectTacsat,byTripTacsat)
    
    
    
    ### 2. Divide the summed total landings by  GROUP variables in each Match Level by the VMS record weights saved in 'pings' field
    
    tacsatFTREF[,kgeur(colnames(tacsatFTREF))]    <- sweep(tacsatFTREF[,kgeur(colnames(tacsatFTREF))],1,tacsatFTREF$pings,"/")
    
    tacsatFTREF
    
    #tacsatFTREF$ID                                <- tacsatFTREF$ID.x # af(ac(tacsatFTREF$ID.x))
    DT                                            <- data.table(tacsatFTREF)
    eq1                                           <- c.listquote(paste("sum(",colnames(tacsatFTREF[,kgeur(colnames(tacsatFTREF))]),",na.rm=TRUE)",sep=""))
    tacsatFTREF                                   <- DT[,eval(eq1),by=.(ID.x, match_level)]; tacsatFTREF <- data.frame(tacsatFTREF); setnames(tacsatFTREF,colnames(tacsatFTREF),c("ID","match_level",colnames(eflaloTrip[,kgeur(colnames(eflaloTrip))])))
   
     
   
   
  }
  
  #-------------------------------------------------------------------------------
  # 2a) Merge eflalo to tacsat with no matching FT_REF
  #-------------------------------------------------------------------------------
  
  #- If you don't want to loose catch or value data, conserve the non-merged
  #   eflalo catches and distribute these over the tacsat records
  if(conserve == TRUE){
    
    if(dim(tacsat)[1]>0 & dim(eflaloVessel)[1] > 0){
      
      #-------------------------------------------------------------------------------
      # 2a-1) Merge eflalo to tacsat with matching VE_REF
      #-------------------------------------------------------------------------------
      
      if("day" %in% level){
        
        print("CONSERVE Match Level 1.a : Vessel + Day")
       
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(tacsat))  tacsat$SI_DAY                 <- an(format(tacsat$SI_DATIM,format="%j"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(eflaloVessel))  eflaloVessel$SI_DAY     <- an(format(eflaloVessel$LE_CDATIM,format="%j"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~VE_REF+icesrectangle+day,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("VE_REF","LE_RECT","SI_DAY"),      eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byDayTacsat   <- res[["tacsat"]]
        byDayTacsat = byDayTacsat |> mutate (match_level = "VE_REF,LE_RECT,SI_DAY")
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
        
        
      }
      
      if("ICESrectangle" %in% level){
        print("CONSERVE Match Level 1.b : Vessel + ICES Rectangle")
        
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~VE_REF+icesrectangle,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("VE_REF","LE_RECT"),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byRectTacsat  <- res[["tacsat"]]
        byRectTacsat = byRectTacsat |> mutate (match_level = "VE_REF,LE_RECT")
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      }
      if(TRUE){ #-For remainder of vessel merging not at ICESrectangle level
        
        print("CONSERVE Match Level 1.c : Vessel + Year")
        
        if(!"SI_YEAR" %in% colnames(tacsat))              tacsat$SI_YEAR    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        
        
        #- Count pings in tacsat set
        nPings                <- countPings(~VE_REF,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("VE_REF" ),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byVessTacsat  <- res[["tacsat"]]
        byVessTacsat = byVessTacsat |> mutate (match_level = "VE_REF")
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      }
      
      #-------------------------------------------------------------------------------
      # 2b-1) Bind all tacsat files with matching VE_REF
      #-------------------------------------------------------------------------------
      
      if("day"  %in% level & !"ICESrectangle" %in% level) tacsatVEREF <- rbind(byDayTacsat,byVessTacsat)
      if(!"day" %in% level &  "ICESrectangle" %in% level) tacsatVEREF <- rbind(byRectTacsat,byVessTacsat)
      if("day" %in% level &   "ICESrectangle" %in% level) tacsatVEREF <- rbind(byDayTacsat,byRectTacsat,byVessTacsat)
      
      tacsatVEREF[,kgeur(colnames(tacsatVEREF))]    <- sweep(tacsatVEREF[,kgeur(colnames(tacsatVEREF))],1,tacsatVEREF$pings,"/")
      #tacsatVEREF$ID                                <- af(ac(tacsatVEREF$ID.x))
      DT                                            <- data.table(tacsatVEREF)
      eq1                                           <- c.listquote(paste("sum(",colnames(tacsatVEREF[,kgeur(colnames(tacsatVEREF))]),",na.rm=TRUE)",sep=""))
      tacsatVEREF                                   <- DT[,eval(eq1), by=.(ID.x, match_level)]; tacsatVEREF <- data.frame(tacsatVEREF); setnames(tacsatVEREF,colnames(tacsatVEREF),c("ID","match_level",colnames(eflaloVessel[,kgeur(colnames(eflaloVessel))])))
       
      
      }
    
    if(dim(tacsat)[1] > 0 & dim(eflaloNoVessel)[1] > 0 & conserve_ML2 == TRUE){
      
      #-------------------------------------------------------------------------------
      # 2a-2) Merge eflalo to tacsat with no matching FT_REF or VE_REF
      #-------------------------------------------------------------------------------
      if("day" %in% level){
        
        print("CONSERVE Match Level 2.a : Day")
        
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(tacsat))  tacsat$SI_DAY                     <- an(format(tacsat$SI_DATIM,format="%j"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                    <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(eflaloNoVessel))  eflaloNoVessel$SI_DAY     <- an(format(eflaloNoVessel$LE_CDATIM,format="%j"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~icesrectangle+day,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("LE_RECT","SI_DAY"),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byDayTacsat       <- res[["tacsat"]]
        byDayTacsat = byDayTacsat |> mutate (match_level = "LE_RECT,SI_DAY")
        
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      }
      
      if("ICESrectangle" %in% level){
        
        print("CONSERVE Match Level 2.b : ICES Rectangle")
        
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT    <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings            <- countPings(~icesrectangle,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("LE_RECT"),eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byRectTacsat      <- res[["tacsat"]]
        byRectTacsat = byRectTacsat |> mutate (match_level = "LE_RECT")
        
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      }
      
      if(TRUE){ #-For remainder of merging not at ICESrectangle level
        
        print("CONSERVE Match Level 2.b : Year")
        
        if(!"SI_YEAR" %in% colnames(tacsat))          tacsat$SI_YEAR            <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings            <- countPings(~year,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("SI_YEAR"),                        eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byVessTacsat      <- res[["tacsat"]]
        byVessTacsat = byVessTacsat |> mutate (match_level = "YEAR")
        
        stats_landings <- res[["stats_landings"]] |> rbind(stats_landings)
        stats_VMS      <- res[["stats_VMS"]] |> rbind(stats_VMS) 
      }
      #-------------------------------------------------------------------------------
      # 2b-2) Bind all tacsat files with no matching FT_REF or VE_REF
      #-------------------------------------------------------------------------------
      
      if("day"  %in% level & !"ICESrectangle" %in% level) tacsatREF <- rbind(byDayTacsat,byVessTacsat)
      if(!"day" %in% level &  "ICESrectangle" %in% level) tacsatREF <- rbind(byRectTacsat,byVessTacsat)
      if("day" %in% level &   "ICESrectangle" %in% level) tacsatREF <- rbind(byDayTacsat,byRectTacsat,byVessTacsat)
      
      tacsatREF[,kgeur(colnames(tacsatREF))]      <- sweep(tacsatREF[,kgeur(colnames(tacsatREF))],1,tacsatREF$pings,"/")
     
      
      #tacsatREF$ID                                <- af(ac(tacsatREF$ID.x))
      DT                                          <- data.table(tacsatREF)
      eq1                                         <- c.listquote(paste("sum(",colnames(tacsatREF[,kgeur(colnames(tacsatREF))]),",na.rm=TRUE)",sep=""))
      tacsatREF                                   <- DT[,eval(eq1), by=.(ID.x, match_level)]; tacsatREF <- data.frame(tacsatREF); setnames(tacsatREF,colnames(tacsatREF),c("ID","match_level", colnames(eflaloVessel[,kgeur(colnames(eflaloVessel))])))
       
      
      }
  }#End conserve
  
  #-------------------------------------------------------------------------------
  # 3) Merge all tacsat files together and return
  #-------------------------------------------------------------------------------
  
  if(conserve == TRUE){
    
    
    
    ## create a table for the 3 cases 
    if(exists("tacsatFTREF")){one   <- tacsatFTREF  }   else  { one   <- numeric() }
    if(exists("tacsatVEREF")){two   <- tacsatVEREF  }   else  { two   <- numeric() }
    if(exists("tacsatREF"))  {three <- tacsatREF    }   else  { three <- numeric() }
    
  
    
    ##bin together the 3 tables ( trip, vessel  , no vessel /trip)
   
    tacsatTot       <- rbind(one,two,three)
    DT              <- data.table(tacsatTot)
    eq1             <- c.listquote(paste("sum(",colnames(tacsatTot[,kgeur(colnames(tacsatTot))]),",na.rm=TRUE)",sep=""))
    tacsatTot       <- DT[,eval(eq1),by=.(ID )]; tacsatTot <- data.frame(tacsatTot); setnames(tacsatTot,colnames(tacsatTot),c("ID" ,colnames(eflalo[,kgeur(colnames(eflalo))])))
    
    tacsatTot_match_levels = DT[, total := sum(LE_KG_TOT ), by =  .(ID )][, proportion := ifelse (total > 0, round(LE_KG_TOT / total,5), 0 )  ][, match_level_prop := paste0(match_level,"(",proportion,")")  ][, .(Match_level_proportion = paste(match_level_prop, collapse = ";")), by = .(ID)];
    
    tacsatReturn1    <- merge(tacsat,tacsatTot,by="ID",all.x=TRUE) ## Merge the original TACSAT with the results of LE_KG, LE_EURO columsn aportioned
    tacsatReturn    <- merge(tacsatReturn1,tacsatTot_match_levels,by="ID",all.x=TRUE)
    
    
    if(variable == "value") tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("EURO",colnames(tacsatReturn)))]
    if(variable == "kgs")   tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("KG",colnames(tacsatReturn)))]
    if(variable == "all")   tacsatReturn <- tacsatReturn
  
  } else {
    
    if(exists("tacsatFTREF")==FALSE){stop("You have selected not to conserve catches, but there is no trip identifier in the tacsat file")}
    
    DT              <- data.table(tacsatFTREF)
    
    tacsatTot       <- DT[,eval(eq1),by=.(ID)]; tacsatTot <- data.frame(tacsatTot); setnames(tacsatTot,colnames(tacsatTot),c("ID",colnames(eflalo[,kgeur(colnames(eflalo))])))
    
    tacsatTot_match_levels = DT[, total := sum(LE_KG_TOT ), by =  .(ID )][, proportion := ifelse (total>0, round(LE_KG_TOT / total,2), 0 )  ][, match_level_prop := paste0(match_level,"(",proportion,")")  ][, .(Match_level_proportion = paste(match_level_prop, collapse = ";")), by = .(ID)];
    
    tacsatReturn  <- merge(tacsat,tacsatFTREF,by="ID",all.x=TRUE)
    
    
    if(variable == "value") tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("EURO",colnames(tacsatReturn)))]
    if(variable == "kgs")   tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("KG",colnames(tacsatReturn)))]
    if(variable == "all")   tacsatReturn <- tacsatReturn
  }
  
  if(returnAll & nrow(remtacsat)>0)
    tacsatReturn <- orderBy(~ID,data=rbindTacsat(tacsatReturn,remtacsat))
  
  return(list (tacsatEflalo =  orderBy(~ID,data=tacsatReturn)[,-match("ID",colnames(tacsatReturn))],
               stats_landings =  stats_landings , 
               stats_VMS      = stats_VMS  ) ) 
         
         }




######################### COUNTPINGS FUNCTION #####################################

countPings <- function(formula,tacsat,grid=NULL,by=NULL){
  
  
  require(sf)
  require(data.table)
  
  
  #1. Turn selected variables  in the formula into element list. OUTPUT: vars
  
  form <- formula
  if (form[[1]] != "~")
    stop("Error: Formula must be one-sided.")
  formc <- as.character(form[2])
  formc <- gsub(" ", "", formc)
  if (!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "")
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars != ""]
  
  # Get the signs in the formula. but Currently SAP0p77 do not use signs 
  signs <- formc
  
  for (i in 1:length(vars)) {
    signs <- gsub(vars[i], "", signs)
  }
  
  signs <- unlist(strsplit(signs, "")) 
  
  #2. Define which variables selected are column names, time variables or spatial variables
  #INPUT: vars
  
  Vars      <- vars[which(!vars %in% c("day","week","month","quarter","year","gridcell","icesrectangle","icesarea"))]
  timeVars  <- vars[which(vars %in% c("day","week","month","quarter","year"))]
  spatVars  <- vars[which(vars %in% c("gridcell","icesrectangle","icesarea"))]
  
  #3. Add to TACSAT the time category passed as function argument in the  formula 
  ## INPUT: The time  variables in the formula extracted in Step 2. 
  
  if(length(timeVars)>0){
    
    if(!length(grep("SI_DATIM",colnames(tacsat)))>0) tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE,  tacsat$SI_TIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
    if("day" %in% timeVars & !"SI_DAY" %in% colnames(tacsat)){       tacsat$SI_DAY   <- an(format(tacsat$SI_DATIM,format="%j"))};      if("day" %in% timeVars){   ; timeVars[which(timeVars=="day")]      <- "SI_DAY"}
    if("week" %in% timeVars & !"SI_WEEK" %in% colnames(tacsat)){      tacsat$SI_WEEK  <- an(format(tacsat$SI_DATIM,format="%W"))};     if("week" %in% timeVars){   ; timeVars[which(timeVars=="week")]     <- "SI_WEEK" }
    if("month" %in% timeVars & !"SI_MONTH" %in% colnames(tacsat)){     tacsat$SI_MONTH <- an(format(tacsat$SI_DATIM,format="%m"))};    if("month" %in% timeVars){   ; timeVars[which(timeVars=="month")]    <- "SI_MONTH"}
    if("quarter" %in% timeVars & !"SI_QUART" %in% colnames(tacsat)){   tacsat$SI_QUART <- an(substr(quarters(tacsat$SI_DATIM),2,2))};  if("quarter" %in% timeVars){ ; timeVars[which(timeVars=="quarter")]  <- "SI_QUART"}
    if("year" %in% timeVars & !"SI_YEAR" %in% colnames(tacsat)){      tacsat$SI_YEAR  <- an(format(tacsat$SI_DATIM,format="%Y"))};     if("year" %in% timeVars){   ; timeVars[which(timeVars=="year")]     <- "SI_YEAR" }
  
    }
  ## OUTPUT: TACSAT with time categories fields
  
  #3. Add to TACSAT the Spatial category passed as function argument in the  formula 
  ## INPUT: The spatial variables in the formula extracted in Step 2.
  
  if(length(spatVars)>0){
    if("gridcell" %in% spatVars & is.null(grid) == TRUE) stop("Grid needs to be specified to use the 'gridcell' option")
    if("gridcell" %in% spatVars & is.null(grid) == FALSE){
      #Create coordinates of tacsat data
      coords                    <- cbind(x=tacsat$SI_LONG,y=tacsat$SI_LATI)
      sPDF                      <- st_as_sf(tacsat,coords=c("SI_LONG","SI_LATI"))
      idx                       <- sapply(st_intersects(sPDF,grid), function(z) if (length(z)==0) NA_integer_ else z[1])
      newCoords                 <- st_coordinates(st_centroid(grid))[idx,]
      
      tacsat$GR_LONG            <- newCoords[,1]
      tacsat$GR_LATI            <- newCoords[,2]
      spatVars[which(spatVars=="gridcell")] <- "GR_LONG"; spatVars <- c(spatVars,"GR_LATI")
    }
    
    
    ### 3.1 Calculates the ICES Rectangle where the VMS record is located
    ### Important step to match with Logbook ICES rectangle in Match Level 0.a and 0.b
    
    if("icesrectangle" %in% spatVars){
      if(!"LE_RECT" %in% colnames(tacsat))
        tacsat$LE_RECT <- ICESrectangle(tacsat)
      spatVars[which(spatVars=="icesrectangle")] <- "LE_RECT"
    }
    
    if("icesarea" %in% spatVars){
      if(!"LE_AREA" %in% colnames(tacsat))
        tacsat$LE_AREA <- ICESarea(tacsat)
      spatVars[which(spatVars=="icesarea")] <- "LE_AREA"
    }
  }
  
  ## OUTPUT: TACSAT with SPATIAL categories fields
  
  #4. Sum the number of VMS pings ( if BY is NULL) or the variable in BY ( eg. sum INTV as number of hours ) 
  ## INPUT: TACSAT
  
  
  if(is.null(by)){
    tacsat$SUM      <- 1
    tacsat$n_vms_records      <- 1
    
  } else {
    tacsat$SUM                <- tacsat[,by]
    tacsat$n_vms_records      <- 1  ## keep track to number of records
  }
  
  ## OUTPUT: TACSAT with the SUM field with value of 1 ( if BY is NULL) or with BY variable ( e.g. INTV value if choosen in BY)
  
  
  
  #5. Sum the number of VMS pings ( if BY is NULL) or the variable in BY ( eg. sum INTV as number of hours ) 
  
      ## Convert all variables into FACTOR prior the SUM of step 4 create values 
  
      totVars         <- c(Vars,timeVars,spatVars)
      for(iVars in 1:length(totVars)) tacsat[,totVars[iVars]] <- af(ac(tacsat[,totVars[iVars]]))
      
      
      ## Prepare the TACSAT as Data Table  and the variable as a quoted list to use in the GROUP BY DT function  
      
      DT              <- data.table(tacsat)
      eq              <- c.listquote(totVars)
      
  
      # 5.1 Sum the values added in Step 4 in the SUM column by the GROUP BY categories ( eq = totVars  as quoted list) 
  
      res             <- DT[,sum(SUM),by=eval(eq)]
      
      ## Change the total sum field name to "pings" name . 
      ## This is passed in eflal2pings functions and used in 
      
      setnames(res,colnames(res),c(totVars,"pings"))
      
  #colnames(res)   <- c(totVars,"pings")
  
  return(data.frame(res))
  
  }




################# EFLALO2PINGS FUNCTION ######################################

#' Merge eflalo to tacsat pings
#' 
#' Internal function of splitAmongPings to merge eflalo landings or values to
#' tacsat pings
#' 
#' 
#' @param eflalo eflalo dataset
#' @param tacsat tacsat dataset
#' @param pings number of pings by variable
#' @param vars variable to merge eflalo to tacsat
#' @param eflaloCol column names of eflalo
#' @param remainTacsat number of tacsat pings that have not been merged yet
#' @return Returns a list of the eflalo dataset, but without the landings and
#' values that have been merged, returns the merged tacsat dataset and returns
#' the number tacsat pings that have not been merged yet
#' @author Niels T. Hintzen
#' @seealso \code{\link{splitAmongPings}}, \code{\link{mergeEflalo2Tacsat}},
#' \code{\link{mergeEflalo2Pings}}
#' @references EU Lot 2 project
#' @export eflalo2Pings


eflalo2Pings <- function(eflalo,tacsat,pings,vars,eflaloCol,remainTacsat,by=NULL){
  
  ## INPUT : EFLALO and TACSAT datasets
  ## pings argumetn is passed the output  of countPings function 
  ## vars is the list of variables for this Match Level  ( e.g. FT_REF, ICESRect, SI_DAY)
  ## efaloCol  are the columns with  LE_KG and LE_EURO
  ## remainTacsat  , the unique ID's of the TACSAT records that were not matched at each step . 
  ## BY , the variabel to be used to weight the landings distribution
  
  ######################################################################
  #1.  SUM the LE_KG and LE_EURO columns by GROUP BY 'vars' 
  #####################################################################
  
        # Convert the variables in EFLALO and TACSAT in factors
  
        for(iVars in 1:length(vars)){
          eflalo[,vars[iVars]] <- af(ac(eflalo[,vars[iVars]]))
          tacsat[,vars[iVars]] <- af(ac(tacsat[,vars[iVars]]))
        }
  
        # Prepare the EFLALO data as Data Table  
        DT                    <- data.table(eflalo)
        ## Create the formula to SUM the  LE_KG , LE_EURO  variables and been evaluated in Data Table 
        eq1                   <- c.listquote(paste("sum(",colnames(eflalo[,kgeur(colnames(eflalo))]),",na.rm=TRUE)",sep=""))
        #Converted to quoted list  the variables for GROUP BY ( vars_ )
        eq2                   <- c.listquote(vars)
  
  # Sum using  Data Table  the LE_KG, LE_EURO vars by GROUP BY variables 
  # OUTPUT: Replace the EFLALO variable 
  eflalo            <- data.frame(DT[,eval(eq1),by=eval(eq2)]); 
     
       #Change the names of the resulted  EFLALO data frame according to variables names and LE_KG and LE_EURO variables 
        colnames(eflalo) <- c(vars,eflaloCol)
       # Create and ID for the records in the summarised EFLALO version 
        eflalo$ID         <- 1:nrow(eflalo)
        
  
  ########################################################################
  #2.  Merge summarised EFLALO to TACSAT pings to distribute the landings in the LB  by VMS record
  ########################################################################
        
        
  ## 2.1 JOIN the summarised EFLALO with the summarised TACSAT ( 'pings'  output  in countPings function ) 
  ## This will give the total number of VMS records or hours ( if By is used with INTV) by GROUP of variables 
  byPing             <- merge(eflalo,data.frame(pings),by=vars,all=FALSE)
        
  ## 2.2 JOIN the JOINED   data in step 2.1 to the original TACSAT data 
  ## This will give the total number of VMS records or hours and the total number of landings weight and vlaue in LE_KG and LE_EURO
  byTacsat           <- merge(tacsat,byPing,by=vars,all=FALSE)
   
  ########################################################################
  #3.  Divide each VMS record INTV by the total SUMMED by GROUP BY attributes in countPIngs function section 5.1
  ########################################################################
  
  if(is.null(by)==FALSE)
    byTacsat$pings   <- byTacsat$pings / byTacsat[,by]
  
  ########################################################################
  #3.  Create the statistics of landings distributed at each match level process
  ########################################################################
  
  ## Create the data frame to track stats
  stats_landings = data.frame ( match_level = paste(vars, collapse = ',') , category = c( "kg in eflalo", "kg in merged tacsat" ,"kg removed from eflalo" ), total = 0 )
  stats_VMS = data.frame ( match_level = paste(vars, collapse = ',') , category = c( "total VMS records in TACSAT", "number of VMS records", "avg number of records by Match Level vars"  ), total = 0 )
  
  # Calculate the total sum of ladings that were in EFLALO
  stats_landings[1,'total' ] = round(sum(byPing [,kgeur(colnames(byPing))[1]]))
  try(print(  paste("kg in eflalo",   stats_landings[1,'total' ]   ) ))
 
  # Calculate the total sum of ladings that were matched with PINGS 
  stats_landings[2,'total' ] = round(sum(sweep(byTacsat[,kgeur(colnames(byTacsat))][1],1,byTacsat$pings,"/")))
  try(print(paste("kg in merged tacsat", stats_landings[2,'total' ]  )))
   
  # Calculate the total sum of VMS records  that were matched at this Match LEvel  
  
  
  stats_VMS[1,'total' ] = dim(tacsat)[1]
  
  try(print(paste("Total VMS records", stats_VMS[1,'total' ]  )))
  
  if ( dim( byTacsat)[1] > 0  ) { 
    byTacsat$n_records = 1 
     stats_VMS[2,'total' ] = round(sum( byTacsat$n_records )) 
  
  
 
  
  byTacsatDT = data.table(byTacsat)
  mean_n_records_group = byTacsatDT[,sum(n_records),by=eval(eq2)]; 
  setnames(mean_n_records_group,colnames(mean_n_records_group),c(vars,"total_by_groups"))
  stats_VMS[3,'total' ] = round(mean( mean_n_records_group$total_by_groups ))
  
  } else {
    stats_VMS[2,'total' ] = 0
    stats_VMS[3,'total' ] = 0
  }
  
  try(print(paste("VMS records matched", stats_VMS[2,'total' ]  )))
  try(print(paste("VMS records matched",round( (  stats_VMS[2,'total' ]/  stats_VMS[1,'total' ]) , 2)*100, '%'  )))
  
  
  
    
  #- Bookkeeping which TACSAT ID's have been merged and which have not yet been merged
  remainTacsat          <- remainTacsat[which(!remainTacsat %in% byTacsat$ID.x)]
  
  #- Bookkeeping which EFLALO  catches have been merged and which have not yet been merged
  idx                   <- sort(unique(byPing$ID))
  
  # Calculate the total sum of ladings in EFLALO that are going to be removed 
  stats_landings[3,'total' ] = round(sum(eflalo[idx,kgeur(colnames(eflalo))[1]]))
  
  try(print(paste("kg removed from eflalo",stats_landings[3,'total' ]  )))
  
  ########################################################################
  #4.  Change to 0 all EFLALO landings records already distributed
  ########################################################################
  
  eflalo[idx,kgeur(colnames(eflalo))]   <- 0
  
  ########################################################################
  # Return the list of datasets
  ########################################################################
  
  return(list(eflalo=eflalo,  ## eflalo modified with removed landings already matched
              tacsat=byTacsat,   #### TACSAT with TOTAL LE_KG and LE_EURO and the 'ping' field that contains the proportion weigth of each record from teh total by GROUP BY variuables
              remainTacsat=remainTacsat,   ##ID's of remaining TACSAT records not matched 
              stats_landings = stats_landings, 
              stats_VMS = stats_VMS ) ## stats dataset to track landings distributions
         )
  
  }






 
