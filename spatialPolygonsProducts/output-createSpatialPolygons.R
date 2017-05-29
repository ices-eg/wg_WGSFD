
library(sp)

years <- 2015

for (year in years) {
  cat("Creating Fishing pressure shapefiles for ... ", year, "\n")
  flush.console()

  for (type in c("total", "jncc", "benthis")) {
  
    # read in total data
    data <- read.csv(paste0("data/fishingPressure/OSPAR_intensity_data_", type ,"_", year, ".csv"))

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

    # subset out OSPAR region and HELCOM region
    if (type == "total") {
      # save output
      rgdal::writeOGR(out, "spatialPolygonsProducts/shapefiles", paste0("OSPAR_intensity_total_", year), 
                      driver = "ESRI Shapefile", overwrite_layer = TRUE)
    } else {
      if (type == "benthis") {
        geartype <- data$Benthis_metiers
      } else {
        geartype <- data$Fishing_category
      }
      for (igeartype in unique(geartype)) 
        rgdal::writeOGR(out[geartype == igeartype,], 
                        "spatialPolygonsProducts/shapefiles", paste0("OSPAR_intensity_", igeartype, "_", year), 
                        driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }
  }
}
