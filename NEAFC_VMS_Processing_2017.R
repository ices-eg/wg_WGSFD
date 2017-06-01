######################
##  NEAFC VMS DATA  ##
##    PROCESSING,   ##
##     Mar 2017     ##
#################nc###

# load libraries, data and set working directory
library(sp)
library(SDMTools)
library(adehabitat)

work.dec <-"C:/Users/campbelln/Documents/WGSFD/2017/NEAFC_DATA"

setwd(work.dec)


       vms <- read.csv("NEAFC_POSITIONS_DATA.csv", sep=";", header=T)
catch.data <- read.csv("NEAFC_CATCH_DATA.csv", sep=";", header=T) 
   vessels <- read.csv("NEAFC_VESSELS_DATA.csv", sep=";", header=T)

       vms <- vms[,-1]
## drop row numbers

colnames(vessels)[1]<- colnames(vms)[1] <- colnames(catch.data)[1]<-"RID"
## consistently refer to random ID number across tables

vms$Year <- as.numeric(substr(vms$DA, 1, 4))
## year values are missing for 2016

stored.vms <- vms
## park previous years for now

vms <- vms[vms$Year == 2016,]
## and just look at 2016

vms <- vms[duplicated(vms)==F,]
## remove duplicates

vms$DateTime <- as.POSIXct(paste(vms$DA, sprintf("%04d", vms$TI), sep=" "), format="%Y%m%d %H%M", tz="UTC")
## extract polling times

vms <- vms[order(vms$DateTime),]
## sort the data choronologically

catch.data <- catch.data[catch.data$RID %in% vms$RID,]
   vessels <- vessels[vessels$RID %in% vms$RID,]
## drop vessels and catches from earlier than 2016

vms$Gear <- vessels$FISHING_GEAR[match(vms$RID, vessels$RID)]
## assign gear type to vessels using vessel registry table

sort(tapply(rep(1, dim(vms)[1]), vms$Gear, sum))
## we still have 56k pings (~12.5%) with no gear - will come back to these


vms.all <- vms
## park this for now too


#vms <- vms[vms$Gear %in% c("OTB", "SSC", "PTB", "TBS", "OTT"),]

# now 59147 rows, 180 RIDs



vms$RID <- paste(vms$RID)

rids <- unique(vms$RID)
# we have 703 vessels

sum(tapply(rep(1, dim(vms)[1]), vms$RID, sum, na.rm=T)==0)
## no trips have zero pings this year, but some have one, or a just ENT and EXT value

sum(tapply(rep(1, dim(vms)[1]), vms$RID, sum, na.rm=T)==1)

## we'll exclude these and remove everything that's not a POS or MAN
## and try again

vms <-vms[vms$TM %in% c("POS", "MAN"),]
## removes ENT & EXT records

single.pings <- names(tapply(rep(1, dim(vms)[1]), vms$RID, sum, na.rm=T)==1)[(tapply(rep(1, dim(vms)[1]), vms$RID, sum, na.rm=T)==1)==TRUE]

vms <- vms[vms$RID %in% single.pings == FALSE,]
## removes trips with a single ping (?!)

rids <- unique(vms$RID)

sum(tapply(rep(1, dim(vms)[1]), vms$RID, sum, na.rm=T)==0)



## so, here's a loop which orders VMS records chronologically by vessel, calculates the distance covered and speed

vms.out <- NULL

for (i in (1:length(rids))){
  
  
  temp.vms <- vms[vms$RID == rids[i],]
  
  temp.hours <- round(as.numeric(paste(difftime(temp.vms$DateTime[2:dim(temp.vms)[1]], temp.vms$DateTime[1:(dim(temp.vms)[1]-1)], units = "hours"))),2)
  temp.hours <- c(temp.hours, mean(temp.hours[temp.hours <3], na.rm=T))
  
  temp.vms$estimatedTimeDifHOurs <- temp.hours
  
  temp.vms$LG <- as.numeric(paste(temp.vms$LG))
  temp.vms$LT <- as.numeric(paste(temp.vms$LT))
  
  for(j in (1:((dim(temp.vms)[1])-1))){
    
    ll <- matrix(c(temp.vms$LG[j], temp.vms$LG[j+1], temp.vms$LT[j],  temp.vms$LT[j+1]), ncol=2)
    temp.vms$Derived.Speed[j] <- round((0.54*spDistsN1(ll, ll[1,], longlat=TRUE)[2])/temp.hours[j])
  }
  
  temp.vms$Derived.Speed[temp.vms$Derived.Speed>15] <- 15
  
  vms.out <- rbind(vms.out, temp.vms)
  
}

vms.out$estimatedTimeDifHOurs <- as.numeric(paste(vms.out$estimatedTimeDifHOurs))
vms.out$estimatedTimeDifHOurs[is.na(vms.out$estimatedTimeDifHOurs)] <-2
vms.out$estimatedTimeDifHOurs[vms.out$estimatedTimeDifHOurs>2] <-2


## specify the range of latitude and longitudes we want to grid over
lons <- seq(-42, -7.05, by = 0.05)
lats <- seq(39.5, 63.95, by = 0.05)

vms.all <- vms.out
## park this for now

## Specify gear groupings
 trawl.gears <- c("OTB", "PTB", "TBS", "OTT")
static.gears <- c("LL", "LLS", "LLD", "GND", "GNS", "LNB")
    no.gears <- c("NULL", "NIL")

vms.trawl  <- vms.all[vms.all$Gear %in% trawl.gears,]
vms.static <- vms.all[vms.all$Gear %in% static.gears,]
vms.nogear <- vms.all[vms.all$Gear %in% no.gears,]
## subset vms by gear type


## Choose static, mobile or no gear groups to work on...this time we do no gear
vms.out <- vms.nogear
# vms.out <- vms.trawl
# vms.out <- vms.static

effort.mat <- matrix(nrow=length(lons), ncol=length(lats))
dim(effort.mat)
#plot(vms.out$estimatedTimeDifHOurs)
for(i in(1:length(lons))){
  for(j in(1:length(lats))){
    
    effort.mat[i,j] <- sum(vms.out$estimatedTimeDifHOurs[vms.out$Derived.Speed>=0 & vms.out$Derived.Speed<=5 & vms.out$LG>=lons[i] & vms.out$LG<(lons[i]+0.05)&vms.out$LT>=lats[j]&vms.out$LT<(lats[j]+0.05)], na.rm=T)

  }
}

effort.mat[effort.mat==0] <- (-9999)
## sets no data value to -9999 (neccessary for outputting to .asc)
effort.mat[effort.mat>50] <-50
## caps effort at 50h/cell to keep z-scale common

effort.list <- list(x=lons, y=lats, z=effort.mat)
image(effort.list, zlim=c(1, max(effort.list$z)))
## plots a test output

temp.out <- as.asc(x=effort.mat, xll=-41.975, yll=39.525, cellsize = 0.05, type="numeric")
export.asc(temp.out, "VMS2016(no_gears).asc")
## exports gridded data product for plotting by WG-DEC

##... and now repeat for static gears
vms.out <- vms.static

effort.mat <- matrix(nrow=length(lons), ncol=length(lats))
dim(effort.mat)
#plot(vms.out$estimatedTimeDifHOurs)
for(i in(1:length(lons))){
  for(j in(1:length(lats))){
    
    effort.mat[i,j] <- sum(vms.out$estimatedTimeDifHOurs[vms.out$Derived.Speed>=0 & vms.out$Derived.Speed<=5 & vms.out$LG>=lons[i] & vms.out$LG<(lons[i]+0.05)&vms.out$LT>=lats[j]&vms.out$LT<(lats[j]+0.05)], na.rm=T)
    #choose lower speed between 1 and 3
  }
}

effort.mat[effort.mat==0] <- (-9999)
effort.mat[effort.mat>50] <- 50

effort.list <- list(x=lons, y=lats, z=effort.mat)
image(effort.list, zlim=c(1, max(effort.list$z)))


temp.out <- as.asc(x=effort.mat, xll=-41.975, yll=39.525, cellsize = 0.05, type="numeric")
export.asc(temp.out, "VMS2016(static_gears).asc")



## and trawls
vms.out <- vms.trawl

effort.mat <- matrix(nrow=length(lons), ncol=length(lats))
dim(effort.mat)
#plot(vms.out$estimatedTimeDifHOurs)
for(i in(1:length(lons))){
  for(j in(1:length(lats))){
    
    effort.mat[i,j] <- sum(vms.out$estimatedTimeDifHOurs[vms.out$Derived.Speed>=0 & vms.out$Derived.Speed<=5 & vms.out$LG>=lons[i] & vms.out$LG<(lons[i]+0.05)&vms.out$LT>=lats[j]&vms.out$LT<(lats[j]+0.05)], na.rm=T)
    #choose lower speed between 1 and 3
  }
}

effort.mat[effort.mat==0] <- (-9999)

effort.list <- list(x=lons, y=lats, z=effort.mat)
image(effort.list, zlim=c(1, max(effort.list$z)))


temp.out <- as.asc(x=effort.mat, xll=-41.975, yll=39.525, cellsize = 0.05, type="numeric")
export.asc(temp.out, "VMS2016(trawl_gears).asc")

## The next piece of code groups consecutive pings at fishing speeds for mobile gears into putative "hauls"
## if you are not doing this straight after the above section, make sure vms.out <- vms.trawl

rids <- unique(vms.out$RID)

haul.id <- 1

haul.vms <- NULL
## set starting values and structures

for(i in (1:length(rids))){
  
  temp.vms <- vms.out[vms.out$RID==rids[i],]
  temp.vms$Derived.Speed[is.na(temp.vms$Derived.Speed)] <- temp.vms$VesselSpeed[is.na(temp.vms$Derived.Speed)]
  # remove anything where there isn't a valid speed
  start.state  <- ifelse(temp.vms$Derived.Speed[1]<5, "fishing", "steaming")
  # determine if first ping is at fishing or steaming speed
  haul.id <- ifelse(start.state == "fishing", haul.id+1, haul.id)
  # if the vessel is finshing, add 1 to "HAUL_ID", otherwise, leave it along
  temp.vms$Haul.No[1] <- ifelse(start.state=="fishing", haul.id, NA)
  
  for(j in (2:dim(temp.vms)[1])){
  # for each subsequent ping
    if(temp.vms$Derived.Speed[j] >5){temp.vms$Haul.No[j] <- NA}
    # if the vessel remains above 5 knots, no change to haul ID
    if(temp.vms$Derived.Speed[j] < 1){temp.vms$Haul.No[j] <- NA}
    # if the vessel remains below 1 knot, no change
    if(temp.vms$Derived.Speed[j] <=5 & temp.vms$Derived.Speed[j] >= 1 & temp.vms$Derived.Speed[j-1] > 5){
      haul.id <- haul.id + 1
      temp.vms$Haul.No[j] <- haul.id
    }
    # if the vessel was travelling faster than 5 knots and drops below it, deem this to be
    # change of activity and add 1 to haul ID
    if(temp.vms$Derived.Speed[j] <= 5 & temp.vms$Derived.Speed[j] >=1 & temp.vms$Derived.Speed[j-1] <= 5 & temp.vms$Derived.Speed[j-1] >=1) {temp.vms$Haul.No[j] <- haul.id}
    # if the vessel continues to bimble along at 1-5 knots, it remains fishing and no change to haul ID
  }
  haul.vms <- rbind(haul.vms, temp.vms)
  # binds temporary data to output
}

hauls.out <- haul.vms[!is.na(haul.vms$Haul.No),]
## drop rows where there isn't fishing activity
hauls.out <- hauls.out[Haul.No != 4,] 
# there are problems with no. 4 which I haven't got to the bottom of... 
hauls.out <-  hauls.out[hauls.out$Haul.No %in% unique(hauls.out$Haul.No)[tapply(rep(1, dim(hauls.out)[1]), hauls.out$Haul.No, sum)>1],]
## drop hauls where we only have one point - no way to plot lines

hauls.out <- hauls.out[,colnames(hauls.out) %in% c("LT", "LG", "Haul.No")]
## drops other columns for outputting