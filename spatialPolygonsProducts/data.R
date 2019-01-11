
library(sp)
library(icesTAF)

#years <- 2009:2017

# get ospar region
ospar <- rgdal::readOGR("data/shapefiles/ospar.shp", verbose = FALSE)

# region for request 2017
ospar_2017 <- ospar #[ospar$Region %in% c(2, 3),]
ospar_2017 <- rgeos::gUnaryUnion(ospar_2017)
raster::crs(ospar_2017) <- CRS("+init=epsg:4326")

# region for request 2018
ospar_2018 <- ospar[ospar$Region %in% c(2, 3),]
ospar_2018 <- rgeos::gUnaryUnion(ospar_2018)
raster::crs(ospar_2018) <- CRS("+init=epsg:4326")

# get HELCOM region
helcom <- rgdal::readOGR("data/shapefiles/helcom.shp", verbose = FALSE)
helcom <- rgeos::gUnaryUnion(helcom)
raster::crs(helcom) <- CRS("+init=epsg:4326")

# list of run details (could be moved to config)
runs <- list(
    list(
      region = "HELCOM",
      year = 2017,
      datacall = "_ICES_VMS_Datacall_VMS",
      shape = helcom
    ),
    list(
      region = "OSPAR",
      year = 2017,
      datacall = "_ICES_VMS_Datacall_VMS",
      shape = ospar_2017
    ),
    list(
      region = "OSPAR",
      year = 2018,
      datacall = "_ICES_VMS_Datacall_VMS",
      shape = ospar_2018
    )
  )


for (run in runs) {

  root_dir <- function(which = "shapefiles") {
    paste0("spatialPolygonsProducts/", which, "/", run$datacall, "/", run$year, "/", run$region)
  }

  mkdir(root_dir())

  for (year in 2009:(run$year-1)) {
  
    cat("Creating Fishing pressure shapefiles for ... ", run$region, run$year, "year:", year, "\n")
    flush.console()

    for (type in c("total", "jncc", "benthis")) {
  
      # read in total data
      data <- read.csv(paste0("data/fishingPressure/", run$datacall, "/OSPAR_intensity_data_", type ,"_", year, ".csv"))
      
      # convert NA to -9
      data[is.na(data)] <- -9

      # calculate unique c_squares
      spdata <- unique(data[,c("c_square", "Latitude", "Longitude")])
      rownames(spdata) <- spdata$c_square
      spdata <- dplyr::rename(spdata, mid_lat = Latitude, mid_lon = Longitude)
    
      # form the spatial dataframe
      r1 <- rbind(x = c(-1, -1, 1,  1, -1) * 0.05/2,
                  y = c(-1,  1, 1, -1, -1) * 0.05/2)
  
      spatialLookup <- 
        SpatialPolygons(
          lapply(1:nrow(spdata), 
                 function(i) {
                   Polygons(
                     list(
                       Polygon(
                         t(r1 + c(spdata$mid_lon[i], spdata$mid_lat[i]))
                       )
                     ), ID = spdata$c_square[i])
                   }
                 )
          )
  
      spatialLookup <- SpatialPolygonsDataFrame(spatialLookup, spdata)
      proj4string(spatialLookup) <- CRS("+init=epsg:4326")
      
      ids <- rgeos::gContains(run$shape, spatialLookup, byid = TRUE)
      spatialLookup$inShape <- unname(rowSums(ids)) > 0
      
      # now assign data to the columns
      out <- spatialLookup[data$c_square,]
      out$SubsurfSAR <- data$SubSurface_SweptAreaRatio
      out$SurfSAR <- data$Surface_SweptAreaRatio
      out$SubsurfSA <- data$SubSurfaceSweptArea
      out$SurfSA <- data$Surface
      out$totweight <- data$totweight
      out$totvalue <- data$totvalue
      out$KWfishingH <- data$kw_fishinghours
      out$FishingH <- data$fishing_hours
  
      # subset
      data <- data[out$inShape,]
      out <- out[out$inShape,]
      out <- out[,!names(out) %in% c("inShape")]
  
      if (type == "total") {
        # save output
        rgdal::writeOGR(out, paste0(root_dir(), "/", year), 
                        paste0(run$region, "_intensity_total_", year), 
                        driver = "ESRI Shapefile", overwrite_layer = TRUE)
      } else {
        if (type == "benthis") {
          geartype <- data$Benthis_metiers
        } else {
          # type is jncc
          geartype <- data$Fishing_category
        }
        for (igeartype in unique(geartype)) {
          if (sum(geartype == igeartype) > 0) {
            rgdal::writeOGR(out[geartype == igeartype,], 
                            paste0(root_dir() ,"/", year), 
                            paste0(run$region, "_intensity_", igeartype, "_", year), 
                            driver = "ESRI Shapefile", overwrite_layer = TRUE)          
          }
        }
      }
    }
  }
}
