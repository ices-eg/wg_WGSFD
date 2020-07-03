####################################################################################################
## TOR D: Identifying potential drivers and describing spatial conflicts of fisheries in the past ##
##        and future on displacement of fishing activities over various time-scales               ##   
##                                                                                                ##
## Author: M.Woillez, T.Wilkes, I.Katara, S.Orey, N.Hintzen                                       ## 
## Date: 15.06.2020, TW                                                                           ##
##       26.06.2020, MW                                                                           ##               
##                                                                                                ##
####################################################################################################


################################################################################
# SET-UP ====
#
# NOTES:
# -> It is advised to re-start R before running this R script.
# -> Please set working directory to source file location.
# -> Select Fishery Gear in a later section before running this code.

rm(list=ls())
workdir <- getwd()
set.seed(1)

if(!require(easypackages)) {install.packages("easypackages")}
library(easypackages)
VMStoolspackages <- c("cluster", "data.table", "doBy", "maps", "mapdata", "maptools", "PBSmapping", "sp")
mypackages <- c("MASS", "corrgram", "openxlsx", "autoimage", "car", "svglite")
packages(c(VMStoolspackages, mypackages), prompt = F)
libraries(mypackages)

library(vmstools)

# INLA:
if(!require(INLA)) {
  install.packages(
    "INLA", repos=c(getOption("repos"),
                    INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}
library(INLA)


################################################################################
# Read in Data ====
#

dat <- fread("Data/data_modelling.csv", header=T, stringsAsFactors=F, sep = ",")
dat <- as.data.frame(dat)
print(object.size(dat), units="auto")

# remove rows for which basic aggregation grouping variables don't exist (also saves memory):
ind <- which(!complete.cases(subset(dat,select=c(lon, lat, c_square, year, month))))
length(ind)
dat <- dat[-ind,]

print(object.size(dat), units="auto")

class(dat)



################################################################################
# Check data and add seasons ====
#
summary(dat)
str(dat)
head(dat)
tail(dat)
whichNA <- sapply(dat, FUN=function(x)mean(is.na(x)))

# Defining seasons: 
dat$season <- NA
dat$season[dat$month %in% c(12, 1, 2)] <- "winter"
dat$season[dat$month %in% 3:5] <- "spring"
dat$season[dat$month %in% 6:8] <- "summer"
dat$season[dat$month %in% 9:11] <- "autumn"

dat$in_shore <- as.numeric(dat$in_shore) # as.numeric


################################################################################
# SELECT FISHERY GEAR TYPE ====
#

Gears <- with(dat, unique(metier_benth[!is.na(metier_benth)]))
print(Gears)
# [1] "OT_DMF"         "OT_SPF"         "DRB_MOL"        "TBB_DMF"       
# [5] "OT_CRU"         "OT_MIX"         "TBB_CRU"        "OT_MIX_CRU_DMF"
# [9] "SSC_DMF"        "SDN_DMF"        "OT_MIX_DMF_BEN" "TBB_MOL"       
      
FisheryGear <- "OT_SPF" # select fishery gear type here!
FisheryGears.0infl <- c("DRB_MOL", "SDN_DMF", "OT_SPF", "OT_MIX",
                        "OT_MIX_DMF_BEN", "TBB_CRU", "TBB_MOL")

if(!FisheryGear %in% Gears){
  stop("Unknown Fishery Gear!")
}

  
FG.path <- FisheryGear


################################################################################
# Make one subsetted, and one non-subsetted data file ====
#
# Get data for only selected fishery type.
# Keep also one non-subsetted dataset.
# The non-subsetted dataset is used as a sort-of "empty" grid

# Make non-subsetted data:
# Note: all variables are unique for each combination of year, MONTH & c-square
# ...Therefore, removing duplicates to save memory and computation time later
# Use months here, not seasons.
zerodat <- dat[!duplicated(cbind(dat$lon, dat$lat, dat$c_square,
                                 dat$year, dat$month)),] 

# Subset data for selected Fishery gear Type:
subdat <- subset(dat, metier_benth==FisheryGear)



################################################################################
# Aggregate Data ====
#

# aggregate empty grid data:
grouping.columns <- c("month", "year", "c_square", "season", "lon", "lat", "ID")
vessel.columns <- c("fishing_hours", "FHR",  "kw_fishinghours", "totweight", "totvalue")
ignore.columns <- c(grouping.columns, vessel.columns)

numeric.columns <- colnames(zerodat)[sapply(zerodat, FUN = is.numeric)]
listcolumns <- setdiff(numeric.columns, ignore.columns)
print(listcolumns)

zerodat <- as.data.table(zerodat)
griddat <- zerodat[, lapply(.SD, mean, na.rm=T), 
                   by=list(c_square, year, month),
                   .SDcols=listcolumns] # fast aggregation
griddat <- as.data.frame(griddat)
griddat <- griddat[complete.cases(griddat),]


# aggregate response part:
subdat <- as.data.table(subdat)
expl <- subdat[, lapply(.SD, sum, na.rm=T),
               by=list(c_square, year, month),
               .SDcols=c("fishing_hours", "kw_fishinghours")] # fast aggregation
expl <- as.data.frame(expl)
expl <- expl[complete.cases(expl),]

# merge data:
datflat <- merge(expl, griddat, by=c("c_square", "year", "month"), all.y=T)

# set response for non-observations to 0:
datflat$fishing_hours <- with(datflat,
                              ifelse(is.na(fishing_hours), 0, fishing_hours))
datflat$kw_fishinghours <- with(datflat,
                              ifelse(is.na(kw_fishinghours), 0, kw_fishinghours))

# re-create longitude and latitude:
# NOTE: # Data is made in 0.05 degree C-squares
pos <-  CSquare2LonLat(datflat$c_square, 0.05) 
datflat$lon <- pos$SI_LONG
datflat$lat <- pos$SI_LATI

# save temporary aggregated data
# (in case of errors later)
imagename <- paste0(FG.path, "/RData/TemporaryAggregatedData.RData")
dir.create(file.path(getwd(), paste0(FG.path, "/RData")), recursive = TRUE)
save.image(imagename)


# End
################################################################################
