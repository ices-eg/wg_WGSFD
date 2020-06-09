####################################################################################################
## TOR D: Identifying potential drivers and describing spatial conflicts of fisheries in the past ##
##        and future on displacement of fishing activities over various time-scales               ##   
##                                                                                                ##
## Author: M.Woillez                                                                              ## 
## Date: 09.06.2020                                                                               ## 
####################################################################################################

# Need R version 3.5.1 !!!!

### Load libraries
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

# create folder to save figures
dir.create(file.path(getwd(), "RFig"))
patfig<-paste(getwd(),"/RFig",sep="")

# read data
df <- fread("data_modelling.csv")
head(df)
dim(df)

# select a metier
df <- df[df$metier_benth=="OT_DMF",]
dim(df)

# histogram of the explanatory variable: fishing hours
tiff(paste(patfig,"/Fig_A.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
hist(df$fishing_hours,100000,xlim=c(0,20))
dev.off()

# color palette
jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))

# Data visualization
ix <- seq(min(df$lon,na.rm=T)-0.025,max(df$lon,na.rm=T)+0.025,0.05)
iy <- seq(min(df$lat,na.rm=T)-0.025,max(df$lat,na.rm=T)+0.025,0.05)
it <- seq(min(df$year,na.rm=T)-0.5,max(df$year,na.rm=T)+0.5,1)
im <- seq(0.5,12.5,1)
xcut <- cut(df$lon,ix,include.bounds=T)
ycut <- cut(df$lat,iy,include.bounds=T)
mcut <- cut(df$month,im,include.bounds=T)
tcut <- cut(df$year,it,include.bounds=T)
Z <- tapply(df$fishing_hours,list(xcut,ycut,mcut,tcut),sum,na.rm=T)
year <- sort(unique(df$year))
for(i in 1:length(unique(df$year))){
  for(j in 1:12){
    tiff(paste(patfig,"/Fig_B_",sprintf("%02d",j),".",year[i],".tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
    image.plot(ix,iy,Z[,,j,i],main=paste(sprintf("%02d",j)," - ",year[i],sep=""),col=jet.colors(100),xlab="Longitude (in degree)",ylab="Latitude (in degree)")
    map("worldHires",col=8,fill=T,add=T);box()   
    dev.off()
  }
}

# size of the dataset if all c_squares were informed 
length(Z)

# actual size of the dataset
dim(df)[1]

# Zero values are missing, but covariates values are missing too at those zeroes location... Database need to be revised...

# Delete unwanted variables 
df <- df[,-c("metier_benth","totweight","totvalue","kw_fishinghours")]

# sample size per months and years
table(df$year,df$month)

# sub-sample data
set.seed(123)
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

# plot explanatory variable
tiff(paste(patfig,"/Fig_D.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
ggplot() + 
  geom_point(aes(x = tmp$lon, y = tmp$lat, colour = tmp$fishing_hours), size = 2, alpha = 1) +
  scale_colour_gradientn(colours = tim.colors(100))
dev.off()

# plots co-variates
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

# Other plots
tiff(paste(patfig,"/Fig_E.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
par(mfrow = c(3, 2))
plot(tmp$lon, tmp$fishing_hours, cex = 0.5, xlab = "Longitude")
plot(tmp$lat, tmp$fishing_hours, cex = 0.5, xlab = "Latitude")
plot(tmp$mud_percent, tmp$fishing_hours, cex = 0.5, xlab = "Mud percent")
plot(tmp$gravel_percent, tmp$fishing_hours, cex = 0.5, xlab = "Gravel percent")
plot(tmp$distance_coast_avg, tmp$fishing_hours, cex = 0.5, xlab = "Average distance to the coastline (km)")
dev.off()
par(mfrow = c(1, 1))

# Create shapefile for study area

# load North Sea coastlines
data("worldHiresMapEnv")
NS_poly <- map("worldHires",  fill=TRUE, col="transparent", plot=FALSE, xlim=c(-5,8.5), ylim=c(51,62))
IDs <- sapply(strsplit(NS_poly$names, ":"), function(x) x[1])
NS <- map2SpatialPolygons(NS_poly, IDs=IDs,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# Defining boundary of the study area
box.sel <- matrix(c(-4.5,9.5,9.5,-4.5,-4.5,50,50,61,61,50), ncol=2)

# Load square polygon function
square.polygon = function(xlim, ylim, ret.SP = F, id=runif(1)){
  # From Bakka et al. (2017)
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
poly.land <- gIntersection(NS, square) 
poly.water <- gDifference(square, poly.land) 

# View polygons and estimation data
tiff(paste(patfig,"/Fig_F_polygons.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(poly.water, xlim=range(box.sel[ ,1]), ylim=range(box.sel[ ,2]), axes=T,col="lightblue")
plot(poly.land,col="grey70",add=T)
points(cbind(tmp$lon,tmp$lat),col="red",pch=19,cex=0.5)
dev.off()

# create triangulation mesh
prmesh <- inla.mesh.2d(boundary = poly.water, max.edge = c(0.45, 1), cutoff = 0.2)
tiff(paste(patfig,"/Fig_F_mesh3.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(prmesh, asp = 1, main = "")
plot(poly.land, col = 3, add=T)
points(tmp$lon, tmp$lat, pch = 19, cex = 0.5, col = "red")
dev.off()

# Construct the SPDE object
spde <- inla.spde2.matern(prmesh)

# Observation structure for estimation data
table(repl <- as.numeric(factor(paste(tmp$year,sprintf("%02d", tmp$month),sep="-")))) # from 1 to 72
dim(A <- inla.spde.make.A(prmesh, repl=repl, loc=cbind(tmp$lon,tmp$lat)))

# Observation structure for field prediction 
mesh.index <- inla.spde.make.index(name='spatial.field', n.spde=spde$n.spde, n.repl=length(unique(repl)))

# Stack data
stk.dat <- inla.stack(data=list(y=tmp$fishing_hours), A=list(A, 1), 
                   effects=list(c(mesh.index, list(Intercept=1)), list(tmp[,4:(length(tmp)-3)])),
                   tag='est')

# define model formula
formula <- (y ~ -1 + 
              in_shore +
              distance_coast_avg +
              bpi5 +
              bpi10 +
              bpi30 +
              bpi50 +
              bpi75 +
              tac_ple +
              tac_sol +
              mud_percent +
              sand_percent +
              gravel_percent +
              total_d50 +
              tidalvelmean +
              oil_price +
              sea_bottom_temp +
              f(field, model=spde, group=field.group, control.group=list(model="ar1")))

# fit the spatio-temporal model
# + perform cross-validation and compute the DIC value
r.s <- inla(formula, family = "nbinomial", data = inla.stack.data(stk.dat), verbose = TRUE, 
            control.predictor = list(A = inla.stack.A(stk.dat), compute = TRUE), 
            control.compute = list(dic = TRUE, cpo = TRUE, waic = T))

# check criterion
r.s$waic$waic
mean(log(r.s$cpo$cpo))

