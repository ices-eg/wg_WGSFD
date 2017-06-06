# R-script:     Computing DCF indicators
# date:         01/06/2017
# R version:    3.0.2

# start time
start.time <- Sys.time()

# clear workspace
rm(list = ls())

# load package
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(maptools)
library(rgeos)
library(rasterVis)

# load function for DCF indicators
source("spreading.area.r")

# load shape file for land
land <- readShapePoly("ices_ecoregions/GSHHS_h_L1_decoupe.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
land <- spTransform(land,CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"))

# load ecoregions (coarse resolution)
poly <- readShapePoly("ices_ecoregions/ices_ecoregions_bf005-005.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

# load VMS data
DCFagg0 <- read.table("../../Data/tbl_VMS_Final_Maps_31502017/tbl_VMS_Final_Maps_31502017.csv",header=T,sep=";",stringsAsFactors=F,na.strings="NULL")
names(DCFagg0)[c(1,2)]<-c("year","No_Recs")
head(DCFagg0)
# year No_Recs   totweight    totvalue kw_fishinghours fishing_hours      Surface     area
# 1 2009       2  6954.37212 10908.12496     29691.66667    87.4333333 160.16859850 30.91037
# 2 2009       7 26710.34587 84790.61646     88884.30000   164.7666667 119.04125172 30.91037
# 3 2010       6 44975.58576 80066.91746     56284.66667    91.6666667  64.89385985 30.91037
# 4 2012       8 57815.85062  2027.28122    100648.00000   181.0000000 248.38659939 30.91037
# 5 2013       1    13.86928    85.57462        36.66667     0.1666667   0.01620264 30.91037
# 6 2013       2 13006.83313  2710.02567      2287.00000     3.0000000   2.65424944 30.91037
# lat SubSurFaceSweptArea Surface_SweptAreaRatio SubSurface_SweptAreaRatio
# 1 0.025         8.008429925           5.1817104216              0.2590855211
# 2 0.025        26.967911032           3.8511749519              0.8724550689
# 3 0.025        14.367008459           2.0994201926              0.4647957100
# 4 0.025        79.249703029           8.0357038939              2.5638546878
# 5 0.025         0.008457778           0.0005241813              0.0002736227
# 6 0.025         0.207031456           0.0858692161              0.0066977989
# c_square Fishing_category subsurface_prop Latitude Longitude
# 1 1000:100:100:1   Demersal seine              NA    0.025     0.025
# 2 1000:100:100:1            Otter              NA    0.025     0.025
# 3 1000:100:100:1            Otter              NA    0.025     0.025
# 4 1000:100:100:1            Otter              NA    0.025     0.025
# 5 1000:100:100:1             Beam              NA    0.025     0.025
# 6 1000:100:100:1            Otter              NA    0.025     0.025

# check fishing categories
unique(DCFagg0$Fishing_category)
str(DCFagg0)
dim(DCFagg0)

# current assessment year
yy <- 2016
DCFagg0 <- DCFagg0[DCFagg0$year<=yy,]

# Bottom gears grouped together -------------------------------------------

# aggregate gears
DCFagg1 <- aggregate(cbind(Surface,SubSurFaceSweptArea,Surface_SweptAreaRatio,SubSurface_SweptAreaRatio) ~ year+c_square+Longitude+Latitude+area, data = DCFagg0, sum)
DCFagg1allyears <- aggregate(cbind(Surface,SubSurFaceSweptArea,Surface_SweptAreaRatio,SubSurface_SweptAreaRatio) ~ c_square+Longitude+Latitude+area, data = DCFagg0, sum)
DCFagg1allyears$year <- "all"
DCFagg1 <- rbind(DCFagg1,DCFagg1allyears);rm(DCFagg1allyears)
dim(DCFagg1)
str(DCFagg1)

# Grouped fishing category (beam, otter, seine, dredge)
i <- "Mobile bottom contacting gears"

# specification for saving
shell(paste("md \"",paste("RFig/",i,sep=""),"\"",sep=""),intern=T,mustWork=T,translate=T)
patfig<-paste(wd,"/RFig/",i,sep="")

# visualization
jpeg(paste(patfig,"/General_map.jpg",sep=""),res=200,width=6,height=6,units="in")
sel <- DCFagg1$year==yy
plot(DCFagg1$Longitude[sel],DCFagg1$Latitude[sel],pch=".",
     xlab="Longitude (°)",ylab="Latitude (°)",col=2,
     xlim=c(-50,50),ylim=c(30,90),asp=1/cos(60*180/pi))
plot(poly,border=8,add=T)
map("worldHires",border=8,add=T,fill=T)
dev.off()

# Link ecoregions to DCFagg1 table
coordinates(DCFagg1) <- ~ Longitude + Latitude
proj4string(DCFagg1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rk <- over(DCFagg1,as(poly,"SpatialPolygons"))
Ecoregion <- as.character(poly$Ecoregion[rk])
DCFagg1 <- spCbind(DCFagg1,Ecoregion)

# check
DCFagg1$Ecoregion <- as.character(DCFagg1$Ecoregion)
table(DCFagg1$Ecoregion) # effort in every ecoregions except in the Artic Ocean

# take out lines without Ecoregions allocated (NA's)
DCFagg1 <- DCFagg1[!is.na(DCFagg1$Ecoregion),]

# Compute ICES ecoregions areas
polyhires <- readShapePoly("ices_ecoregions/ices_ecoregions.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
r <- raster(ncol=(68.5--44)/0.05,
            nrow=(90-30.25)/0.05)
extent(polyhires)
extent(r) <- extent(-44,68.5,30.25,90)
rp <- rasterize(polyhires, r, 'Ecoregion')

# calculate surface area of each ecoregion
lat <- seq(30.25,90,0.05)
midlat <- rev(lat[2:length(lat)]-0.025)
area <- 0.05*cos(midlat*pi/180)*60*1.852 * 0.05*60*1.852 
area <- rep(area,each=rp@ncols)
area[is.na(rp@data@values)] <- NA
ecoregion_area <- data.frame(Ecoregion=as.character(levels(polyhires$Ecoregion)),Area=tapply(area,list(rp@data@values),sum,na.rm=T))

# Compute DCF indicators per years/ICES regions

# Ditribution of fishing activities

# Surface:

# option 1: regular 
DCF5o1 <- round(rbind(by(data.frame(swept_area=DCFagg1$Surface,cell_area=DCFagg1$area),
                       list(DCFagg1$Ecoregion,DCFagg1$year),
                       function(x){
                         z <- x$swept_area
                         z[z>x$cell_area] <- x$cell_area[z>x$cell_area]
                         sum(z,na.rm=T)
                       })))
DCF5o1[is.na(DCF5o1)] <- 0
write.table(DCF5o1,file=paste(patfig,"/DCF5o1_surface.csv",sep=""),sep=";")
DCF5o1

DCF5o1r <- round(DCF5o1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o1))],dim(DCF5o1)[1],dim(DCF5o1)[2])*100,1)
write.table(DCF5o1r,file=paste(patfig,"/DCF5o1_surface_prop.csv",sep=""),sep=";")
DCF5o1r

# option 2: randomly distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
DCF5o2 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                       list(DCFagg1$Ecoregion,DCFagg1$year),
                       function(x,size=100000000000000000000){
                         sum((1-pnbinom(q=0, size=size, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                       })))
DCF5o2[is.na(DCF5o2)] <- 0
write.table(DCF5o2,file=paste(patfig,"/DCF5o2_surface.csv",sep=""),sep=";")
DCF5o2

DCF5o2r <- round(DCF5o2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o2))],dim(DCF5o2)[1],dim(DCF5o2)[2])*100,1)
write.table(DCF5o2r,file=paste(patfig,"/DCF5o2_surface_prop.csv",sep=""),sep=";")
DCF5o2r

# option 3: aggregated distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
# beta = 1.24 (extreme case of aggregation)
DCF5o3 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                         list(DCFagg1$Ecoregion,DCFagg1$year),
                         function(x,beta=1.24){
                           sum((1-pnbinom(q=0, size=x$swept_area_ratio/beta, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                         })))
DCF5o3[is.na(DCF5o3)] <- 0
write.table(DCF5o3,file=paste(patfig,"/DCF5o3_surface.csv",sep=""),sep=";")
DCF5o3

DCF5o3r <- round(DCF5o3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o3))],dim(DCF5o3)[1],dim(DCF5o3)[2])*100,1)
write.table(DCF5o3r,file=paste(patfig,"/DCF5o3_surface_prop.csv",sep=""),sep=";")
DCF5o3r

# SubSurface:

# option 1: regular 
DCF5subo1 <- round(rbind(by(data.frame(swept_area=DCFagg1$SubSurFaceSweptArea,cell_area=DCFagg1$area),
                         list(DCFagg1$Ecoregion,DCFagg1$year),
                         function(x){
                           z <- x$swept_area
                           z[z>x$cell_area] <- x$cell_area[z>x$cell_area]
                           sum(z,na.rm=T)
                         })))
DCF5subo1[is.na(DCF5subo1)] <- 0
write.table(DCF5subo1,file=paste(patfig,"/DCF5o1_subsurface.csv",sep=""),sep=";")
DCF5subo1

DCF5subo1r <- round(DCF5subo1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo1))],dim(DCF5subo1)[1],dim(DCF5subo1)[2])*100,1)
write.table(DCF5subo1r,file=paste(patfig,"/DCF5o1_subsurface_prop.csv",sep=""),sep=";")
DCF5subo1r

# option 2: randomly distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
DCF5subo2 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                         list(DCFagg1$Ecoregion,DCFagg1$year),
                         function(x,size=100000000000000000000){
                           sum((1-pnbinom(q=0, size=size, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                         })))
DCF5subo2[is.na(DCF5subo2)] <- 0
write.table(DCF5subo2,file=paste(patfig,"/DCF5o2_subsurface.csv",sep=""),sep=";")
DCF5subo2

DCF5subo2r <- round(DCF5subo2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo2))],dim(DCF5subo2)[1],dim(DCF5subo2)[2])*100,1)
write.table(DCF5subo2r,file=paste(patfig,"/DCF5o2_subsurface_prop.csv",sep=""),sep=";")
DCF5subo2r

# option 3: aggregated distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
# beta = 1.24 (extreme case of aggregation)
DCF5subo3 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                         list(DCFagg1$Ecoregion,DCFagg1$year),
                         function(x,beta=1.24){
                           sum((1-pnbinom(q=0, size=x$swept_area_ratio/beta, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                         })))
DCF5subo3[is.na(DCF5subo3)] <- 0
write.table(DCF5subo3,file=paste(patfig,"/DCF5o3_subsurface.csv",sep=""),sep=";")
DCF5subo3

DCF5subo3r <- round(DCF5subo3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo3))],dim(DCF5subo3)[1],dim(DCF5subo3)[2])*100,1)
write.table(DCF5subo3r,file=paste(patfig,"/DCF5o3_subsurface_prop.csv",sep=""),sep=";")
DCF5subo3r

# Aggregation of fishing activities

# surface:
DCF6 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                       list(DCFagg1$Ecoregion,DCFagg1$year),
                       function(x){
                         spreading.area(z=x$intensity_total,w=x$cell_area)
                       })))
DCF6[is.na(DCF6)] <- 0
write.table(DCF6,file=paste(patfig,"/DCF6_surface.csv",sep=""),sep=";")
DCF6

DCF6r <- round(DCF6/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF6))],dim(DCF6)[1],dim(DCF6)[2])*100,1)
write.table(DCF6r,file=paste(patfig,"/DCF6_surface_prop.csv",sep=""),sep=";")
DCF6r

# subsurface:
DCF6sub <- round(rbind(by(data.frame(intensity_total=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                       list(DCFagg1$Ecoregion,DCFagg1$year),
                       function(x){
                         spreading.area(z=x$intensity_total,w=x$cell_area)
                       })))
DCF6sub[is.na(DCF6sub)] <- 0
write.table(DCF6sub,file=paste(patfig,"/DCF6_subsurface.csv",sep=""),sep=";")
DCF6sub

DCF6subr <- round(DCF6sub/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF6sub))],dim(DCF6sub)[1],dim(DCF6sub)[2])*100,1)
write.table(DCF6subr,file=paste(patfig,"/DCF6_subsurface_prop.csv",sep=""),sep=";")
DCF6subr

# Area not impacted by bottom contacting gears
# option 1
DCF7o1 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o1))],dim(DCF5o1)[1],dim(DCF5o1)[2])-DCF5o1)
write.table(DCF7o1,file=paste(patfig,"/DCF7o1_prop.csv",sep=""),sep=";")
DCF7o1

DCF7o1r <- round(DCF7o1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o1))],dim(DCF7o1)[1],dim(DCF7o1)[2])*100,1)
write.table(DCF7o1r,file=paste(patfig,"/DCF7o1r.csv",sep=""),sep=";")
DCF7o1r

# option 2
DCF7o2 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o2))],dim(DCF5o2)[1],dim(DCF5o2)[2])-DCF5o2)
write.table(DCF7o2,file=paste(patfig,"/DCF7o2_prop.csv",sep=""),sep=";")
DCF7o2

DCF7o2r <- round(DCF7o2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o2))],dim(DCF7o2)[1],dim(DCF7o2)[2])*100,1)
write.table(DCF7o2r,file=paste(patfig,"/DCF7o2r.csv",sep=""),sep=";")
DCF7o2r

# option 3
DCF7o3 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o3))],dim(DCF5o3)[1],dim(DCF5o3)[2])-DCF5o3)
write.table(DCF7o3,file=paste(patfig,"/DCF7o3_prop.csv",sep=""),sep=";")
DCF7o3

DCF7o3r <- round(DCF7o3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o3))],dim(DCF7o3)[1],dim(DCF7o3)[2])*100,1)
write.table(DCF7o3r,file=paste(patfig,"/DCF7o3r.csv",sep=""),sep=";")
DCF7o3r

# WKBENTH: average intensity over ecoregion
avg_intensity <- round(tapply(DCFagg1$Surface_SweptAreaRatio,list(DCFagg1$Ecoregion,DCFagg1$year),mean,na.rm=T),2)
avg_intensity[is.na(avg_intensity)] <- 0
write.table(avg_intensity,file=paste(patfig,"/avg_intensity_surface.csv",sep=""),sep=";")
avg_intensity

# Adapted from the table of Jennings et al. (2012)
A_FI0.7 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                          list(DCFagg1$Ecoregion,DCFagg1$year),
                          function(x){
                            if(sum(x[,1])!=0){
                              o <- order(x$intensity_total,decreasing=T)
                              f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.7))
                              round(f$y/sum(x$cell_area)*100)
                            }else{0}
                          })))
A_FI0.7[is.na(A_FI0.7)] <- 0
write.table(A_FI0.7,file=paste(patfig,"/A_FI0.7.csv",sep=""),sep=";")
A_FI0.7

A_FI0.8 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                          list(DCFagg1$Ecoregion,DCFagg1$year),
                          function(x){
                            if(sum(x[,1])!=0){
                              o <- order(x$intensity_total,decreasing=T)
                              f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.8))
                              round(f$y/sum(x$cell_area)*100)
                            }else{0}
                          })))
A_FI0.8[is.na(A_FI0.8)] <- 0
write.table(A_FI0.8,file=paste(patfig,"/A_FI0.8.csv",sep=""),sep=";")
A_FI0.8

A_FI0.9 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                          list(DCFagg1$Ecoregion,DCFagg1$year),
                          function(x){
                            if(sum(x[,1])!=0){
                              o <- order(x$intensity_total,decreasing=T)
                              f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.9))
                              round(f$y/sum(x$cell_area)*100)
                            }else{0}
                            })))
A_FI0.9[is.na(A_FI0.9)] <- 0
write.table(A_FI0.9,file=paste(patfig,"/A_FI0.9.csv",sep=""),sep=";")
A_FI0.9



# Bottom gears treated separatly ------------------------------------------

for(i in unique(DCFagg0$Fishing_category)){
  
  # Select a gear
  DCFagg1 <- DCFagg0[DCFagg0$Fishing_category==i,]
  DCFagg1allyears <- aggregate(cbind(Surface,SubSurFaceSweptArea,Surface_SweptAreaRatio,SubSurface_SweptAreaRatio) ~ c_square+Longitude+Latitude+area+Fishing_category, data = DCFagg0, sum)
  DCFagg1allyears$year <- "all"
  DCFagg1 <- rbind(DCFagg1[,c("c_square","Longitude","Latitude","area","Fishing_category","Surface","SubSurFaceSweptArea","Surface_SweptAreaRatio","SubSurface_SweptAreaRatio","year")],DCFagg1allyears);rm(DCFagg1allyears)
  
  # specification for saving
  shell(paste("md \"",paste("RFig/",i,sep=""),"\"",sep=""),intern=T,mustWork=T,translate=T)
  patfig<-paste(wd,"/RFig/",i,sep="")
  
  # visualization
  jpeg(paste(patfig,"/General_map.jpg",sep=""),res=200,width=6,height=6,units="in")
  sel <- DCFagg1$year==yy
  plot(DCFagg1$Longitude[sel],DCFagg1$Latitude[sel],pch=".",
       xlab="Longitude (°)",ylab="Latitude (°)",col=2,
       xlim=c(-50,50),ylim=c(30,90),asp=1/cos(60*180/pi))
  plot(poly,border=8,add=T)
  map("worldHires",border=8,add=T,fill=T)
  dev.off()
  
  # Link ecoregions to DCFagg1 table
  coordinates(DCFagg1) <- ~ Longitude + Latitude
  proj4string(DCFagg1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  rk <- over(DCFagg1,as(poly,"SpatialPolygons"))
  Ecoregion <- as.character(poly$Ecoregion[rk])
  DCFagg1 <- spCbind(DCFagg1,Ecoregion)
  
  # check
  DCFagg1$Ecoregion <- as.character(DCFagg1$Ecoregion)
  table(DCFagg1$Ecoregion) # effort in every ecoregions except in the Artic Ocean
  
  # take out lines without Ecoregions allocated (NA's)
  DCFagg1 <- DCFagg1[!is.na(DCFagg1$Ecoregion),]
  
  # Compute ICES ecoregions areas
  polyhires <- readShapePoly("ices_ecoregions/ices_ecoregions.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  r <- raster(ncol=(68.5--44)/0.05,
              nrow=(90-30.25)/0.05)
  extent(polyhires)
  extent(r) <- extent(-44,68.5,30.25,90)
  rp <- rasterize(polyhires, r, 'Ecoregion')
  
  # calculate surface area of each ecoregion
  lat <- seq(30.25,90,0.05)
  midlat <- rev(lat[2:length(lat)]-0.025)
  area <- 0.05*cos(midlat*pi/180)*60*1.852 * 0.05*60*1.852 
  area <- rep(area,each=rp@ncols)
  area[is.na(rp@data@values)] <- NA
  ecoregion_area <- data.frame(Ecoregion=as.character(levels(polyhires$Ecoregion)),Area=tapply(area,list(rp@data@values),sum,na.rm=T))

  
  # Compute DCF indicators per years/ICES regions/gear groups
  
  # Ditribution of fishing activities
  
  # Surface:
  
  # option 1: regular 
  DCF5o1 <- round(rbind(by(data.frame(swept_area=DCFagg1$Surface,cell_area=DCFagg1$area),
                           list(DCFagg1$Ecoregion,DCFagg1$year),
                           function(x){
                             z <- x$swept_area
                             z[z>x$cell_area] <- x$cell_area[z>x$cell_area]
                             sum(z,na.rm=T)
                           })))
  DCF5o1[is.na(DCF5o1)] <- 0
  write.table(DCF5o1,file=paste(patfig,"/DCF5o1_surface.csv",sep=""),sep=";")
  DCF5o1
  
  DCF5o1r <- round(DCF5o1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o1))],dim(DCF5o1)[1],dim(DCF5o1)[2])*100,1)
  write.table(DCF5o1r,file=paste(patfig,"/DCF5o1_surface_prop.csv",sep=""),sep=";")
  DCF5o1r
  
  # option 2: randomly distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
  DCF5o2 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                           list(DCFagg1$Ecoregion,DCFagg1$year),
                           function(x,size=100000000000000000000){
                             sum((1-pnbinom(q=0, size=size, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                           })))
  DCF5o2[is.na(DCF5o2)] <- 0
  write.table(DCF5o2,file=paste(patfig,"/DCF5o2_surface.csv",sep=""),sep=";")
  DCF5o2
  
  DCF5o2r <- round(DCF5o2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o2))],dim(DCF5o2)[1],dim(DCF5o2)[2])*100,1)
  write.table(DCF5o2r,file=paste(patfig,"/DCF5o2_surface_prop.csv",sep=""),sep=";")
  DCF5o2r
  
  # option 3: aggregated distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
  # beta = 1.24 (extreme case of aggregation)
  DCF5o3 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                           list(DCFagg1$Ecoregion,DCFagg1$year),
                           function(x,beta=1.24){
                             sum((1-pnbinom(q=0, size=x$swept_area_ratio/beta, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                           })))
  DCF5o3[is.na(DCF5o3)] <- 0
  write.table(DCF5o3,file=paste(patfig,"/DCF5o3_surface.csv",sep=""),sep=";")
  DCF5o3
  
  DCF5o3r <- round(DCF5o3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o3))],dim(DCF5o3)[1],dim(DCF5o3)[2])*100,1)
  write.table(DCF5o3r,file=paste(patfig,"/DCF5o3_surface_prop.csv",sep=""),sep=";")
  DCF5o3r
  
  # SubSurface:
  
  # option 1: regular 
  DCF5subo1 <- round(rbind(by(data.frame(swept_area=DCFagg1$SubSurFaceSweptArea,cell_area=DCFagg1$area),
                              list(DCFagg1$Ecoregion,DCFagg1$year),
                              function(x){
                                z <- x$swept_area
                                z[z>x$cell_area] <- x$cell_area[z>x$cell_area]
                                sum(z,na.rm=T)
                              })))
  DCF5subo1[is.na(DCF5subo1)] <- 0
  write.table(DCF5subo1,file=paste(patfig,"/DCF5o1_subsurface.csv",sep=""),sep=";")
  DCF5subo1
  
  DCF5subo1r <- round(DCF5subo1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo1))],dim(DCF5subo1)[1],dim(DCF5subo1)[2])*100,1)
  write.table(DCF5subo1r,file=paste(patfig,"/DCF5o1_subsurface_prop.csv",sep=""),sep=";")
  DCF5subo1r
  
  # option 2: randomly distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
  DCF5subo2 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                              list(DCFagg1$Ecoregion,DCFagg1$year),
                              function(x,size=100000000000000000000){
                                sum((1-pnbinom(q=0, size=size, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                              })))
  DCF5subo2[is.na(DCF5subo2)] <- 0
  write.table(DCF5subo2,file=paste(patfig,"/DCF5o2_subsurface.csv",sep=""),sep=";")
  DCF5subo2
  
  DCF5subo2r <- round(DCF5subo2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo2))],dim(DCF5subo2)[1],dim(DCF5subo2)[2])*100,1)
  write.table(DCF5subo2r,file=paste(patfig,"/DCF5o2_subsurface_prop.csv",sep=""),sep=";")
  DCF5subo2r
  
  # option 3: aggregated distributed (Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746)
  # beta = 1.24 (extreme case of aggregation)
  DCF5subo3 <- round(rbind(by(data.frame(swept_area_ratio=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                              list(DCFagg1$Ecoregion,DCFagg1$year),
                              function(x,beta=1.24){
                                sum((1-pnbinom(q=0, size=x$swept_area_ratio/beta, mu=x$swept_area_ratio))*x$cell_area,na.rm=T)
                              })))
  DCF5subo3[is.na(DCF5subo3)] <- 0
  write.table(DCF5subo3,file=paste(patfig,"/DCF5o3_subsurface.csv",sep=""),sep=";")
  DCF5subo3
  
  DCF5subo3r <- round(DCF5subo3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5subo3))],dim(DCF5subo3)[1],dim(DCF5subo3)[2])*100,1)
  write.table(DCF5subo3r,file=paste(patfig,"/DCF5o3_subsurface_prop.csv",sep=""),sep=";")
  DCF5subo3r
  
  # Aggregation of fishing activities
  
  # surface:
  DCF6 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                         list(DCFagg1$Ecoregion,DCFagg1$year),
                         function(x){
                           spreading.area(z=x$intensity_total,w=x$cell_area)
                         })))
  DCF6[is.na(DCF6)] <- 0
  write.table(DCF6,file=paste(patfig,"/DCF6_surface.csv",sep=""),sep=";")
  DCF6
  
  DCF6r <- round(DCF6/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF6))],dim(DCF6)[1],dim(DCF6)[2])*100,1)
  write.table(DCF6r,file=paste(patfig,"/DCF6_surface_prop.csv",sep=""),sep=";")
  DCF6r
  
  # subsurface:
  DCF6sub <- round(rbind(by(data.frame(intensity_total=DCFagg1$SubSurface_SweptAreaRatio,cell_area=DCFagg1$area),
                            list(DCFagg1$Ecoregion,DCFagg1$year),
                            function(x){
                              spreading.area(z=x$intensity_total,w=x$cell_area)
                            })))
  DCF6sub[is.na(DCF6sub)] <- 0
  write.table(DCF6sub,file=paste(patfig,"/DCF6_subsurface.csv",sep=""),sep=";")
  DCF6sub
  
  DCF6subr <- round(DCF6sub/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF6sub))],dim(DCF6sub)[1],dim(DCF6sub)[2])*100,1)
  write.table(DCF6subr,file=paste(patfig,"/DCF6_subsurface_prop.csv",sep=""),sep=";")
  DCF6subr
  
  # Area not impacted by bottom contacting gears
  # option 1
  DCF7o1 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o1))],dim(DCF5o1)[1],dim(DCF5o1)[2])-DCF5o1)
  write.table(DCF7o1,file=paste(patfig,"/DCF7o1_prop.csv",sep=""),sep=";")
  DCF7o1
  
  DCF7o1r <- round(DCF7o1/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o1))],dim(DCF7o1)[1],dim(DCF7o1)[2])*100,1)
  write.table(DCF7o1r,file=paste(patfig,"/DCF7o1r.csv",sep=""),sep=";")
  DCF7o1r
  
  # option 2
  DCF7o2 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o2))],dim(DCF5o2)[1],dim(DCF5o2)[2])-DCF5o2)
  write.table(DCF7o2,file=paste(patfig,"/DCF7o2_prop.csv",sep=""),sep=";")
  DCF7o2
  
  DCF7o2r <- round(DCF7o2/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o2))],dim(DCF7o2)[1],dim(DCF7o2)[2])*100,1)
  write.table(DCF7o2r,file=paste(patfig,"/DCF7o2r.csv",sep=""),sep=";")
  DCF7o2r
  
  # option 3
  DCF7o3 <- round(matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF5o3))],dim(DCF5o3)[1],dim(DCF5o3)[2])-DCF5o3)
  write.table(DCF7o3,file=paste(patfig,"/DCF7o3_prop.csv",sep=""),sep=";")
  DCF7o3
  
  DCF7o3r <- round(DCF7o3/matrix(ecoregion_area$Area[which(ecoregion_area$Ecoregion %in% rownames(DCF7o3))],dim(DCF7o3)[1],dim(DCF7o3)[2])*100,1)
  write.table(DCF7o3r,file=paste(patfig,"/DCF7o3r.csv",sep=""),sep=";")
  DCF7o3r
  
  # WKBENTH: average intensity over ecoregion
  avg_intensity <- round(tapply(DCFagg1$Surface_SweptAreaRatio,list(DCFagg1$Ecoregion,DCFagg1$year),mean,na.rm=T),2)
  avg_intensity[is.na(avg_intensity)] <- 0
  write.table(avg_intensity,file=paste(patfig,"/avg_intensity_surface.csv",sep=""),sep=";")
  avg_intensity
  
  # Adapted from the table of Jennings et al. (2012)
  A_FI0.7 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                            list(DCFagg1$Ecoregion,DCFagg1$year),
                            function(x){
                                if(sum(x[,1])!=0){
                                  o <- order(x$intensity_total,decreasing=T)
                                  f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.7))
                                  round(f$y/sum(x$cell_area)*100)
                                }else{0}
                            })))
  A_FI0.7[is.na(A_FI0.7)] <- 0
  write.table(A_FI0.7,file=paste(patfig,"/A_FI0.7.csv",sep=""),sep=";")
  A_FI0.7
  
  A_FI0.8 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                            list(DCFagg1$Ecoregion,DCFagg1$year),
                            function(x){
                              if(sum(x[,1])!=0){
                                o <- order(x$intensity_total,decreasing=T)
                                f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.8))
                                round(f$y/sum(x$cell_area)*100)
                              }else{0}                            })))
  A_FI0.8[is.na(A_FI0.8)] <- 0
  write.table(A_FI0.8,file=paste(patfig,"/A_FI0.8.csv",sep=""),sep=";")
  A_FI0.8
  
  A_FI0.9 <- round(rbind(by(data.frame(intensity_total=DCFagg1$Surface_SweptAreaRatio,cell_area=DCFagg1$area),
                            list(DCFagg1$Ecoregion,DCFagg1$year),
                            function(x){
                              if(sum(x[,1])!=0){
                                o <- order(x$intensity_total,decreasing=T)
                                f <- approx(cumsum(x$cell_area[o])~cumsum(x$intensity_total[o])/sum(x$intensity_total),xout=c(0.9))
                                round(f$y/sum(x$cell_area)*100)
                              }else{0}
                            })))
  A_FI0.9[is.na(A_FI0.9)] <- 0
  write.table(A_FI0.9,file=paste(patfig,"/A_FI0.9.csv",sep=""),sep=";")
  A_FI0.9
  
  
}

# end time
end.time <- Sys.time()

# time taken
time.taken <- end.time - start.time
time.taken

