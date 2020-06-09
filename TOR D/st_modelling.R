
# Need R version 3.5.1 !!!!

### Load necessary libraries
library(INLA) 
library(data.table) 
library(fields) 
library(maps) 
library(mapdata) 
library(maptools) 
library(rgeos)
library(graphics) 
library(ggplot2) 
library(PerformanceAnalytics)

# specification for saving
patfig<-paste(getwd(),"/RFig/",sep="")

# read data
df <- fread("data_modelling.csv")
head(df)

# create zeroes and rounding
df$fishing_hours[is.na(df$fishing_hours)] <- 0
df$fishing_hours <- round(df$fishing_hours)

# histogram fishing hours
tiff(paste(patfig,"/Fig_A_.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
hist(df$fishing_hours,100000,xlim=c(0,20))
dev.off()

# my color palettes  
jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))

# blocking
ix <- seq(min(df$lon,na.rm=T)-0.025,max(df$lon,na.rm=T)+0.025,0.05)
iy <- seq(min(df$lat,na.rm=T)-0.025,max(df$lat,na.rm=T)+0.025,0.05)
it <- seq(min(df$year,na.rm=T)-0.5,max(df$year,na.rm=T)+0.5,1)
im <- seq(0.5,12.5,1)
xcut <- cut(df$lon,ix,include.bounds=T)
ycut <- cut(df$lat,iy,include.bounds=T)
mcut <- cut(df$month,im,include.bounds=T)
tcut <- cut(df$year,it,include.bounds=T)
Z <- tapply(df$fishing_hours,list(xcut,ycut,mcut,tcut),sum)
year <- sort(unique(df$year))
for(i in 1:length(unique(df$year))){
  for(j in 1:12){
    tiff(paste(patfig,"/Fig_B_",sprintf("%02d",j),".",year[i],".tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
    image.plot(ix,iy,Z[,,j,i],main=paste(sprintf("%02d",j)," - ",year[i],sep=""),col=jet.colors(100),xlab="Longitude (in degree)",ylab="Latitude (in degree)")
    map("worldHires",col=8,fill=T,add=T);box()   
    dev.off()
  }
}

# check
tmp <- tapply(df$fishing_hours,list(xcut,ycut,mcut,tcut),length)
summary(tmp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.0     1.0     1.0     1.1     1.0    10.0 1776240
    
# # blocking over months and years
# Z <- tapply(df$fishing_hours,list(xcut,ycut),sum,na.rm=T)
# tiff(paste(patfig,"/Fig_C_aggregated_over_years_and_months.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
# image.plot(ix,iy,Z,main="",col=jet.colors(100),xlab="Longitude (in degree)",ylab="Latitude (in degree)")
# map("worldHires",col=8,fill=T,add=T);box()   
# dev.off()
# 
# # format dataset
# tmp1 <- aggregate(fishing_hours ~ lon + lat, data = df, FUN = sum, na.rm=T);dim(tmp1)
# tmp2 <- aggregate(cbind(in_shore,distance_coast_avg,bpi5,bpi10,bpi30,bpi50,bpi75,tac_ple,tac_sol,
#                         mud_percent,sand_percent,gravel_percent,total_d50,tidalvelmean,
#                         oil_price,sea_bottom_temp) ~ lon + lat, data = df, FUN = mean, na.rm=T);dim(tmp2)
# tmp <- merge(x=tmp1, y=tmp2, all.x=T, by=c("lon","lat"));dim(tmp)
# tmp <- na.omit(tmp);dim(tmp)
# head(tmp)

# 
df <- df[,-c("metier_benth","totweight","totvalue","kw_fishinghours")];dim(df)
df <- na.omit(df);dim(df)
table(df$year,df$month)

# sub-sample
set.seed(123)
tmp0 <- df
tmp <- NULL
for(y in unique(df$year)){
  for(m in  unique(df$month)){
    index <- sample(1:nrow(df[df$month==m & df$year==y,]),100)    
    tmp <- rbind(tmp,df[df$month==m & df$year==y,][index,])
  }
}
table(tmp$year,tmp$month)
dim(tmp)

# correlation plots
tiff(paste(patfig,"/Fig_D_cor_.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
chart.Correlation(tmp[,4:length(tmp)])
dev.off()

# histogram fishing hours
tiff(paste(patfig,"/Fig_D_hist_.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
hist(tmp$fishing_hours,100000,xlim=c(0,20))
dev.off()

# plots
tiff(paste(patfig,"/Fig_D.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$fishing_hours), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()

# plots covars
tiff(paste(patfig,"/Fig_D_in_shore.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = as.numeric(tmp$in_shore)), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_distance_coast_avg.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$distance_coast_avg), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_bpi5.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$bpi5), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_bpi10.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$bpi10), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_bpi30.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$bpi30), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_bpi50.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$bpi50), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_bpi75.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$bpi75), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_tac_ple.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$tac_ple), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_tac_sol.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$tac_sol), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_mud_percent.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$mud_percent), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_sand_percent.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$sand_percent), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_gravel_percent.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$gravel_percent), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_total_d50.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$total_d50), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_tidalvelmean.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$tidalvelmean), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_oil_price.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$oil_price), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()
tiff(paste(patfig,"/Fig_D_sea_bottom_temp.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$sea_bottom_temp), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()

tiff(paste(patfig,"/Fig_E.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
par(mfrow = c(3, 2))
plot(tmp$lon, tmp$fishing_hours, cex = 0.5, xlab = "Longitude")
plot(tmp$lat, tmp$fishing_hours, cex = 0.5, xlab = "Latitude")
plot(tmp$mud_percent, tmp$fishing_hours, cex = 0.5, xlab = "Mud percent")
plot(tmp$gravel_percent, tmp$fishing_hours, cex = 0.5, xlab = "Gravel percent")
plot(tmp$distance_coast_avg, tmp$fishing_hours, cex = 0.5, xlab = "Average distance to the coastline (km)")
dev.off()
par(mfrow = c(1, 1))

m.bad <- inla.mesh.create(cbind(tmp$lon,tmp$lat))
tiff(paste(patfig,"/Fig_F_mesh1.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(m.bad, asp=1, main="")
points(cbind(tmp$lon,tmp$lat),col="red",pch=19,cex=0.5)
dev.off()

# Create shapefile for study area - to be used later
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data("worldHiresMapEnv")
NS_poly <- map("worldHires",  fill=TRUE, col="transparent", plot=FALSE, xlim=c(-5,8.5), ylim=c(51,62))
NS_poly$names
IDs <- sapply(strsplit(NS_poly$names, ":"), function(x) x[1])
NS <- map2SpatialPolygons(NS_poly, IDs=IDs,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# Defining boundary of the study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
box.sel <- matrix(c(-4.5,9.5,9.5,-4.5,-4.5,50,50,61,61,50), ncol=2)

# Load square polygon function (to be used later)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# From Bakka et al. (2017)
square.polygon = function(xlim, ylim, ret.SP = F, id=runif(1)){
  # - ret.SP=T : Return a SpatialPolygons object
  # - ret.SP=F : Return a Polygon object
  xlim = range(xlim); ylim = range(ylim)
  corner1 = c(xlim[1], ylim[2])
  corner2 = c(xlim[2], ylim[1])
  poly = Polygon(rbind(corner1, c(corner1[1], corner2[2]), corner2, c(corner2[1], corner1[2]), corner1), hole = FALSE)
  if (ret.SP) {
    return(SpatialPolygons(list(Polygons(list(poly), ID = id))))
  } else {
    return(poly)
  }
}

# Splitting polygon into land and water polygon  
square <- square.polygon(xlim=range(box.sel[ ,1]), ylim=range(box.sel[ ,2]), ret.SP = T)
square@proj4string <- NS@proj4string

poly.land <- gIntersection(NS, square) #land polygon
poly.water <- gDifference(square, poly.land) # water polygon 

# To have a better overview of what has been done so far
tiff(paste(patfig,"/Fig_F_polygons.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(poly.water, xlim=range(box.sel[ ,1]), ylim=range(box.sel[ ,2]), axes=T,col="lightblue")
plot(poly.land,col="grey70",add=T)
points(cbind(tmp$lon,tmp$lat),col="red",pch=19,cex=0.5)
dev.off()

m1 <- inla.mesh.2d(cbind(tmp$lon,tmp$lat), max.edge = c(0.45, 1), cutoff = 0.2)
tiff(paste(patfig,"/Fig_F_mesh2.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(m1, asp = 1, main = "")
plot(poly.land, col = 3, add=T)
points(tmp$lon, tmp$lat, pch = 19, cex = 0.5, col = "red")
dev.off()

prdomain <- inla.nonconvex.hull(cbind(tmp$lon,tmp$lat), -0.03, -0.05, resolution = c(100, 100))

### create final mesh
prmesh <- inla.mesh.2d(boundary = poly.water, max.edge = c(0.45, 1), cutoff = 0.2)
tiff(paste(patfig,"/Fig_F_mesh3.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(prmesh, asp = 1, main = "")
plot(poly.land, col = 3, add=T)
points(tmp$lon, tmp$lat, pch = 19, cex = 0.5, col = "red")
dev.off()

### define SPDE
spde <- inla.spde2.matern(prmesh)

### Observation matrix (A) for each year
table(repl <- tmp$year-2008)#year index from 1:6
dim(A <- inla.spde.make.A(prmesh, repl=repl, loc=cbind(tmp$lon,tmp$lat)))

### Spatial index 
mesh.index <- inla.spde.make.index(name='spatial.field', n.spde=spde$n.spde, n.repl=6)

### Stack data
head(tmp,1)
stk.dat <- inla.stack(data=list(y=tmp$fishing_hours), 
                   tag='est', A=list(A, 1), 
                   effects=list(mesh.index, 
                                data.frame(Intercept=1, tmp[,c(2:length(tmp))])))

# The INLA stack --------------------------------------------------------------------------------------------------- ICI !!!

stk.dat <- inla.stack(data = list(y = Y), A = list(A, 1), tag = "est", 
                      effects = list(c(mesh.index, list(Intercept = 1)), 
                                     list(long = inla.group(tmp$lon), lat = inla.group(tmp$lat),
                                          in_shore = inla.group(in_shore),
                                          distance_coast_avg = inla.group(distance_coast_avg),
                                          bpi5 = inla.group(bpi5),
                                          bpi10 = inla.group(bpi10),
                                          bpi30 = inla.group(bpi30),
                                          bpi50 = inla.group(bpi50),
                                          bpi75 = inla.group(bpi75),
                                          tac_ple = inla.group(tac_ple),
                                          tac_sol = inla.group(tac_sol),
                                          mud_percent = inla.group(mud_percent),
                                          sand_percent = inla.group(sand_percent),
                                          gravel_percent = inla.group(gravel_percent),
                                          total_d50 = inla.group(total_d50),
                                          tidalvelmean = inla.group(tidalvelmean),
                                          oil_price = inla.group(oil_price),
                                          sea_bottom_temp = inla.group(sea_bottom_temp))))

# save results
save.image("Input.RData")
