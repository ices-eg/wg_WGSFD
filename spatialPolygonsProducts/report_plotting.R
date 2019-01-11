# Make Maps of the fishing hours -------
library(raster)
library(sf)
library(icesTAF)

# create drectory
mkdir("spatialPolygonsProducts/maps")

# source common
source("spatialPolygonsProducts/utilities_report.R")

####

# shape files
coast <- rgdal::readOGR("data/shapefiles", "coast", verbose = TRUE)
helcom <- rgdal::readOGR("data/shapefiles", "helcom")
ospar <- rgdal::readOGR("data/shapefiles", "ospar")
ospar_2017 <- ospar
ospar_2018 <- ospar[ospar$Region %in% c(2, 3),]

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
  root_dir <- function(which = "maps") {
    paste0("spatialPolygonsProducts/", which, "/", run$datacall, "/", run$year, "/", run$region)
  }
  mkdir(root_dir())

  for (year in (run$year - 2):(run$year - 1)) {
  
    mkdir(paste0(root_dir(), "/", year))

    shpfiles <- dir(paste0(root_dir("shapefiles"), "/", year), pattern = "*.prj", recursive = TRUE)
    shpfiles <- gsub(".prj", "", shpfiles)
  
    for (shpfile in shpfiles) {
  
      # get data
      vms <- rgdal::readOGR(paste0(root_dir("shapefiles"), "/", year), shpfile, verbose = TRUE)
      vms <- as.data.frame(vms)
      vms <- dplyr::rename(vms, Lat = mid_lat, Lon = mid_lon,
                           Surface_SweptAreaRatio = SurfSAR,
                           SubSurface_SweptAreaRatio = SubsurfSAR)
  
      # plot EO stuff
  
      q1 <- ceiling(quantile(vms$Surface_SweptAreaRatio, 0.95, na.rm = TRUE))
      m1 <- ceiling(max(vms$Surface_SweptAreaRatio, na.rm = TRUE))
      
      q2 <- ceiling(quantile(vms$SubSurface_SweptAreaRatio, 0.95, na.rm = TRUE))
      m2 <- ceiling(max(vms$SubSurface_SweptAreaRatio, na.rm = TRUE))
      
      breaks <- list(unique(c(-10, seq(0, q1, length = 15), pmax(m1, 1))),
                     unique(c(-10, seq(0, q2, length = 15), pmax(m2, 1))))
      
      cvalues <- c("Surface_SweptAreaRatio", "SubSurface_SweptAreaRatio")
      main <- c("SurSAR", "Sub-sur SAR")
      palette <- gplots::rich.colors(15)[-c(1:3)]
      palette <- c(grey(0.5, alpha = 1), palette[-1])
      
      pdata <- vms
  
      for (i in 1:2) {
        fname <- paste0(root_dir(), "/", year, "/", shpfile, "_", cvalues[i], ".png")
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
                   region = run$shape, land = coast,
                   main = paste0(main[i], " - ", gsub(paste0(run$region, "_intensity_"), "", shpfile)),
                   breaks = breaks[[i]],
                   palette = palette)
    
        # do legend
        if (length(breaks) > 1)
          #plotLegend(breaks[[i]], maxvalue = maxval, palette = palette, digits = 1)
          plotLegend(breaks[[i]], maxvalue = maxval, palette = palette, digits = 1)
        dev.off()
      }
    }
  }
}
