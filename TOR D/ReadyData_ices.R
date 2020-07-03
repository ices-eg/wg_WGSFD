
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
VMStoolspackages <- c("cluster", "data.table", "doBy", "maps", "mapdata", 
                      "maptools", "PBSmapping", "sp")
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

dat <- fread("Data/data_modelling.csv", header=T, stringsAsFactors=F,
             sep = ",")

dat <- as.data.frame(dat)

# remove rows for which basic aggregation grouping variables don't exist (also saves memory):
ind <- which(!complete.cases(subset(dat,
                                    select=c(lon, lat, c_square, year, month)))
             )
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

# Defining seasons: there are too many months, and seasons are more informative
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
griddat <- zerodat[, lapply(.SD, mean, na.rm=T), by=list(c_square, year, season),
        .SDcols=listcolumns] # fast aggregation
griddat <- as.data.frame(griddat)
griddat <- griddat[complete.cases(griddat),]


# aggregate response part:
subdat <- as.data.table(subdat)
expl <- subdat[, lapply(.SD, sum, na.rm=T),
               by=list(c_square, year, season),
                .SDcols=c("fishing_hours", "kw_fishinghours")] # fast aggregation
expl <- as.data.frame(expl)
expl <- expl[complete.cases(expl),]

# merge data:
datflat <- merge(expl, griddat, by=c("c_square", "year", "season"),
                 all.y=T)

# set response for non-observations to 0:
datflat$fishing_hours <- with(datflat,
                              ifelse(is.na(fishing_hours), 0, fishing_hours))
datflat$kw_fishinghours <- with(datflat,
                              ifelse(is.na(kw_fishinghours), 0, kw_fishinghours))

# save temporary aggregated data
# (in case of errors later)
imagename <- paste0(FG.path, "/RData/TemporaryAggregatedData.RData")
save.image(imagename)

rm(list=c("dat", "expl", "griddat", "subdat", "zerodat"))



################################################################################
# Clean data ====
#
# check for missing data per column:
sort(sapply(datflat, function(x)mean(is.na(x))), decreasing = T)

# remove any remaining missing cases:
idx <- which(!complete.cases(datflat))
print(idx)
if(length(idx)>0){
  remdata <- data[idx,]
  filename <- paste0(FG.path, "/Output/removed_aggregated_data.csv")
  write.csv(remdata, file=filename)
  datflat <- datflat[-idx,]
}

# make response variable (round to one decimal point, because 0.01 hours doesn't seem very meaningful)
datflat$FHR <- round(datflat$fishing_hours, 1)

# write density plot:
reset.par()
svglite(paste0(FG.path, "/Output/FHR density.svg"))
plot(density(datflat$FHR))
dev.off()

# check for extremely high response values (less then 0.1 promille likely AND higher than 300),
# and re-set them, if found
if(max(datflat$FHR)>300) {
  reset.par()
  extremecutoff <- as.numeric(quantile(datflat$FHR, probs = 999.9/1000)) # 999.9 promille quantile
  print(extremecutoff)
  ind <- which(datflat$FHR > extremecutoff)
  svglite(paste0(FG.path, "/Output/FHR density.svg"))
  par(mfrow=c(1,2))
  plot(density(datflat$FHR))
  mtext(paste0("cutoff=", extremecutoff))
  abline(v=extremecutoff, col="red")
  
  # re-set said extreme values:
  datflat$FHR <- with(datflat, ifelse(FHR > extremecutoff, extremecutoff, FHR))
  plot(density(datflat$FHR))
  dev.off()
}

# check presence
prop.presence <- mean(datflat$FHR > 0)
cat("proportion presence in data = ", prop.presence)
prop.0 <- mean(datflat$FHR==0)
cat("proportion zeroes in data = ", prop.0)



################################################################################
# Make or adjust some variables ====
#
# re-create longitude and latitude:
# NOTE: # Data is made in 0.05 degree C-squares
pos <-  CSquare2LonLat(datflat$c_square, 0.05) 
datflat$lon <- pos$SI_LONG
datflat$lat <- pos$SI_LATI

# sum TAC:
datflat$TAC <- datflat$tac_ple + datflat$tac_sol

# replace percentage variables with proportion variables:
datflat$mud_prop <- datflat$mud_percent/100
datflat$gravel_prop <- datflat$gravel_percent/100
datflat$sand_prop <- datflat$sand_percent/100
datflat$mud_percent <- datflat$gravel_percent <- datflat$sand_percent <- NULL

# replace distance variables from meters to kilometers:
datflat$distance_coast_avg <- datflat$distance_coast_avg/1000

# create dummy variables for years
dummy.years <- 2010:2014 # 2009 is reference level
print(dummy.years)
yearvars <- paste("year", dummy.years, sep="")
print(yearvars)
for (i in 1:length(yearvars)) {
  datflat[,yearvars[i]] <- with(datflat, ifelse(year==dummy.years[i], 1, 0))
}
check.years <- cbind(datflat$year, datflat[,yearvars])

# Create dummy variables for seasons (winter is reference level):
datflat$autumn <- with(datflat, ifelse(season=="autumn", 1, 0))
datflat$spring <- with(datflat, ifelse(season=="spring", 1, 0))
datflat$summer <- with(datflat, ifelse(season=="summer", 1, 0))

# Split response for hurdle
check.cont01 <- datflat$FHR>0 # continuous
check.int01 <- round(datflat$FHR)>0 # discrete
datflat$FHR.cont01 <- as.numeric(check.cont01)
datflat$FHR.int01 <- as.numeric(check.int01)
datflat$FHR.semcont <- with(datflat, ifelse(check.cont01, FHR, NA)) # for semi-continuous distributions
datflat$FHR.0trunc <- with(datflat, ifelse(check.int01, round(FHR), NA)) # for ZTNB


################################################################################
# Save RData image ====
#
warnings()

rm(list = lsf.str())
rm(list=c("workdir"))


imagename <- paste0(FG.path, "/RData/KBWOT_SpT1_Data.RData")
save.image(imagename)


# End
################################################################################
