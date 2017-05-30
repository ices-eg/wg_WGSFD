# Make Maps of the fishing hours -------
library(raster)

# create drectory
if (!dir.exists("spatialPolygonsProducts/maps")) dir.create("spatialPolygonsProducts/maps")


rm(list = ls())

# source common
source("spatialPolygonsProducts/plots-00-plotting-functions.R")

####

# shape files
coast <- rgdal::readOGR("data/shapefiles", "coast", verbose = FALSE)
helcom <- rgdal::readOGR("data/shapefiles", "helcom", verbose = FALSE)
ospar <- rgdal::readOGR("data/shapefiles", "ospar", verbose = FALSE)

# which table
datacall <- "_2017_ICES_VMS_Datacall_VMS"
years <- 2009:2016

if (!dir.exists(paste0("spatialPolygonsProducts/maps/", datacall))) dir.create(paste0("spatialPolygonsProducts/maps/", datacall))

for (year in years) {

  if (!dir.exists(paste0("spatialPolygonsProducts/maps/", datacall, "/", year))) dir.create(paste0("spatialPolygonsProducts/maps/", datacall, "/", year))

  
shpfiles <- dir(paste0("spatialPolygonsProducts/shapefiles/", datacall, "/", year), pattern = "*.prj", recursive = TRUE)
shpfiles <- gsub(".prj", "", shpfiles)

#shpfiles <- c("OSPAR_intensity_Beam_2015",
#              "OSPAR_intensity_Dredge_2015",
#              "OSPAR_intensity_Otter_2015",
#              "OSPAR_intensity_Seine_2015",
#              "OSPAR_intensity_total_2015")
#shpfile <- "OSPAR_intensity_total_2009"
for (shpfile in shpfiles) {

# get data
vms <- rgdal::readOGR(paste0("spatialPolygonsProducts/shapefiles/", datacall, "/", year), shpfile, verbose = TRUE)
vms <- as.data.frame(vms)
vms <- dplyr::rename(vms, Lat = mid_lat, Lon = mid_lon,
                     Surface_SweptAreaRatio = SurfSAR,
                     SubSurface_SweptAreaRatio = SubsurfSAR)

# plot EO stuff

q1 <- ceiling(quantile(vms$Surface_SweptAreaRatio, 0.95))
m1 <- ceiling(max(vms$Surface_SweptAreaRatio))

q2 <- ceiling(quantile(vms$SubSurface_SweptAreaRatio, 0.95))
m2 <- ceiling(max(vms$SubSurface_SweptAreaRatio))

breaks <- list(unique(c(seq(0, q1, length = 15), m1)),
               unique(c(seq(0, q2, length = 15), m2)))

cvalues <- c("Surface_SweptAreaRatio", "SubSurface_SweptAreaRatio")
main <- c("SurSAR", "Sub-sur SAR")
palette <- gplots::rich.colors(15)[-c(1:3)]


pdata <- vms

  for (i in 1:2) {
    fname <- paste0("spatialPolygonsProducts/maps/", datacall, "/", year, "/", shpfile, "_", cvalues[i], ".png")
    png(fname, width = 22, height = 22, res = 1200, units = "cm", pointsize = 20)

    par(mfrow = c(1,1), oma = c(1,1,1,6), mar = c(2,2,2,2),
        cex.axis = .7, cex.lab = 1,
        cex.main = 1.2, font.main = 2)

    rast <- makeRaster(pdata, value = cvalues[i])
    if (is.null(rast)) {
      maxval <- 0
    } else {
      maxval <- max(rast[], na.rm = TRUE)
    }
    plotRaster(rast,
               region = ospar, land = coast,
               main = paste0(main[i], " - ", gsub("OSPAR_intensity_", "", shpfile)),
               breaks = breaks[[i]],
               palette = palette)

    # do legend
    plotLegend(breaks[[i]], maxvalue = maxval, palette = palette, digits = 1)

    dev.off()
  }

}

}
