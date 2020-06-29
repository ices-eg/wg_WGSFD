####################################################################################################
## TOR D: Identifying potential drivers and describing spatial conflicts of fisheries in the past ##
##        and future on displacement of fishing activities over various time-scales               ##   
##                                                                                                ##
## Author: M.Woillez, T.Wilkes, I.Katara, S.Orey, N.Hintzen                                       ## 
## Date: 26.06.2020, MW                                                                           ##               
##                                                                                                ##
####################################################################################################


################################################################################
# SET-UP ====
#

rm(list=ls())
workdir <- getwd()
set.seed(1)

if(!require(easypackages)) {install.packages("easypackages")}
library(easypackages)
VMStoolspackages <- c("cluster", "data.table", "doBy", "maps", "mapdata", "maptools", "PBSmapping", "sp")
mypackages <- c("MASS", "corrgram", "openxlsx", "autoimage", "car", "svglite","raster","PerformanceAnalytics")
packages(c(VMStoolspackages, mypackages), prompt = F)
libraries(mypackages)

library(vmstools)



################################################################################
# Read in Data ====
#

FG.path <- "OT_SPF"
imagename <- paste0(FG.path, "/RData/TemporaryAggregatedData.RData")
load(imagename)


################################################################################
# Exploratory plots ====
#

# histogram of the explanatory variable: fishing hours
dir.create(file.path(getwd(), paste0(FG.path, "/Figures")), recursive = TRUE)
tiff(paste0(FG.path,"/Figures/Fig_1a.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
hist(datflat$fishing_hours,100000)
dev.off()
tiff(paste0(FG.path,"/Figures/Fig_1b.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
hist(datflat$fishing_hours,100000,ylim=c(0,3500))
dev.off()
tiff(paste0(FG.path,"/Figures/Fig_1c.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
hist(datflat$fishing_hours,100000,ylim=c(0,50))
dev.off()

# proportion of integer values (+/-0.001)
s <- seq(0,max(datflat$fishing_hours))
s1 <- s+0.001
s2 <- s-0.001
i <- findInterval(datflat$fishing_hours,sort(c(s1,s2)))
cat("proportion of pseudo-integer values:",round(sum((i%%2) == 1)/length(i)*100,1),"%\n")
# proportion of pseudo-integer values: 99.2 %

# Visualization reponse variables
points <- SpatialPoints(coords=datflat[,c("lon","lat")], proj4string=crs("+proj=longlat +datum=WGS84"))
pixels <- SpatialPixelsDataFrame(points, tolerance = 0.01, data=datflat)
for(y in unique(datflat$year)){
  for(m in unique(datflat$month)){
    r <- raster(pixels[pixels$year==y & pixels$month==m,"fishing_hours"])
    tiff(paste0(FG.path,"/Figures/Fig_2_fishing_hours_",y,"_",m,".tiff"),compression="lzw",width=6,height=6,res=200,units="in")
    plot(r,main=paste0(FG.path," - Fishing hours (",sprintf("%02d",m),"/",y,")"),zlim=range(pixels$fishing_hours))
    map("worldHires",col=8,fill=T,add=T);box() 
    dev.off()
  }
}

for(y in unique(datflat$year)){
  for(m in unique(datflat$month)){
    r <- raster(pixels[pixels$year==y & pixels$month==m,"kw_fishinghours"])
    tiff(paste0(FG.path,"/Figures/Fig_2_kw_fishinghours_",y,"_",m,".tiff"),compression="lzw",width=6,height=6,res=200,units="in")
    plot(r,main=paste0(FG.path," - kw_fishinghours (",sprintf("%02d",m),"/",y,")"),zlim=range(pixels$kw_fishinghours))
    map("worldHires",col=8,fill=T,add=T);box() 
    dev.off()
  }
}

# Visualization covariates
r <- raster(pixels[pixels$year==2009 & pixels$month==1,"in_shore"])
tiff(paste0(FG.path,"/Figures/Fig_2_in_shore.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - in_shore"),zlim=range(pixels$in_shore))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"distance_coast_avg"])
tiff(paste0(FG.path,"/Figures/Fig_2_distance_coast_avg.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - distance_coast_avg"),zlim=range(pixels$distance_coast_avg))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"bpi5"])
tiff(paste0(FG.path,"/Figures/Fig_2_bpi5.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - bpi5"),zlim=range(pixels$bpi5))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"bpi10"])
tiff(paste0(FG.path,"/Figures/Fig_2_bpi10.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - bpi10"),zlim=range(pixels$bpi10))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"bpi30"])
tiff(paste0(FG.path,"/Figures/Fig_2_bpi30.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - bpi30"),zlim=range(pixels$bpi30))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"bpi50"])
tiff(paste0(FG.path,"/Figures/Fig_2_bpi50.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - bpi50"),zlim=range(pixels$bpi50))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"bpi75"])
tiff(paste0(FG.path,"/Figures/Fig_2_bpi75.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - bpi75"),zlim=range(pixels$bpi75))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"mud_percent"])
tiff(paste0(FG.path,"/Figures/Fig_2_mud_percent.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - mud_percent"),zlim=range(pixels$mud_percent))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"sand_percent"])
tiff(paste0(FG.path,"/Figures/Fig_2_sand_percent.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - sand_percent"),zlim=range(pixels$sand_percent))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==2009 & pixels$month==1,"gravel_percent"])
tiff(paste0(FG.path,"/Figures/Fig_2_gravel_percent.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - gravel_percent"),zlim=range(pixels$gravel_percent))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==y & pixels$month==m,"total_d50"])
tiff(paste0(FG.path,"/Figures/Fig_2_ total_d50.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - total_d50"),zlim=range(pixels$total_d50))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

r <- raster(pixels[pixels$year==y & pixels$month==m,"tidalvelmean"])
tiff(paste0(FG.path,"/Figures/Fig_2_tidalvelmean.tiff"),compression="lzw",width=6,height=6,res=200,units="in")
plot(r,main=paste0(FG.path," - tidalvelmean"),zlim=range(pixels$tidalvelmean))
map("worldHires",col=8,fill=T,add=T);box() 
dev.off()

for(y in unique(datflat$year)){
  for(m in unique(datflat$month)){
    r <- raster(pixels[pixels$year==y & pixels$month==m,"sea_bottom_temp"])
    tiff(paste0(FG.path,"/Figures/Fig_2_sea_bottom_temp_",y,"_",m,".tiff"),compression="lzw",width=6,height=6,res=200,units="in")
    plot(r,main=paste0(FG.path," - sea_bottom_temp (",sprintf("%02d",m),"/",y,")"),zlim=range(pixels$sea_bottom_temp))
    map("worldHires",col=8,fill=T,add=T);box() 
    dev.off()
  }
}

# correlation plots
# It takes very long time, but it works...
tiff(paste0(FG.path,"/Figures/Fig_3_correlation.tiff"),compression="lzw",width=20,height=20,res=200,units="in")
chart.Correlation(datflat[,4:(dim(datflat)[2]-2)])
dev.off()



# End
################################################################################
