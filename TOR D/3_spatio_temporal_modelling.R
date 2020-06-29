####################################################################################################
## TOR D: Identifying potential drivers and describing spatial conflicts of fisheries in the past ##
##        and future on displacement of fishing activities over various time-scales               ##   
##                                                                                                ##
## Author: M.Woillez, T.Wilkes, I.Katara, S.Orey, N.Hintzen                                       ## 
## Date: 09.06.2020, MW                                                                           ##
##       26.06.2020, MW                                                                           ##               
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
mypackages <- c("INLA","rgeos","maps","mapdata","maptools")
packages(mypackages, prompt = F)
libraries(mypackages)


################################################################################
# Read in Data ====
#

FG.path <- "OT_SPF"
imagename <- paste0(FG.path, "/RData/TemporaryAggregatedData.RData")
load(imagename)


### Load libraries
library(data.table) 
library(fields) 
library(maps) 
library(mapdata) 
library(maptools) 
library(graphics) 




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


################################################################################
# Create shapefile for study area ====
#

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
tiff(paste0(FG.path,"/Figures/Fig_4_polygons.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(poly.water, xlim=range(box.sel[ ,1]), ylim=range(box.sel[ ,2]), axes=T,col="lightblue")
plot(poly.land,col="grey70",add=T)
points(cbind(datflat$lon[datflat$year==2009],datflat$lat[datflat$year==2009]),col="red",pch=19,cex=0.5)
dev.off()

# create triangulation mesh
prmesh <- inla.mesh.2d(boundary = poly.water, max.edge = c(0.45, 1), cutoff = 0.2)
tiff(paste0(FG.path,"/Figures/Fig_5_mesh.tiff",sep=""),compression="lzw",width=6,height=6,res=200,units="in")
plot(prmesh, asp = 1, main = "")
plot(poly.land, col = 3, add=T)
points(datflat$lon[datflat$year==2009], datflat$lat[datflat$year==2009], pch = 19, cex = 0.5, col = "red")
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

