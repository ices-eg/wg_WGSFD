# ------------------------------------------------------------------------------
# Code snipttet to demonstrate creation of leaflet maps in R
#
# What you will end with is: www.hafro.is/~einarhj/leaflet_demo.html
#
# Case example used is the difference in the sar values in 2015 for the
# 2017 and 2016 datacall. The data are from the shapefiles on the
# wgsfd2017 sharepoint.
#

library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)
library(raster)
library(stringr)
library(rgdal)
library(tidyverse)
library(tools)
library(icesTAF)

# also need sp- and raster-package, but needed function are called directly
# in the code below

datacalls <- c(2017, 2018)
years <- c(2016, 2016)

# Read in the 2015 data from data call 2016 and 2017
# what shapes are there:
# Detect files to read
file2017 <- 
  dir(paste0("spatialPolygonsProducts/shapefiles/_", datacalls[1] ,"_ICES_VMS_Datacall_VMS/", years[1] ,"/"), pattern = "shp", full.names = TRUE)
i <- str_detect(file2017, "PEL")
file2017 <- file2017[!i]

file2018 <- 
  dir(paste0("spatialPolygonsProducts/shapefiles/_", datacalls[2] ,"_ICES_VMS_Datacall_VMS/", years[2] ,"/"), pattern = "shp", full.names = TRUE)
i <- str_detect(file2018, "PEL")
file2018 <- file2018[!i]

label <- file2017 %>%
         basename %>%
         file_path_sans_ext

label <- gsub("(OSPAR_intensity_)|(_20[0-9][0-9])", "", label)
label <- paste(label, years[1], "vs", years[2])


for (i in 1:17) {
#for (i in 1:2) {
  # 2018 datacall data
  thisyear <- 
    rgdal::readOGR(file2018[i])@data %>%
    select(c_square, lon = mid_lon, lat = mid_lat, sar = SurfSAR, ssar = SubsurfSAR) %>% 
    mutate(c_square = as.character(c_square)) %>% 
    gather(variable, thisyear, -c(c_square, lon, lat))
  # 2017 datacall data
  lastyear <- 
    rgdal::readOGR(file2017[i])@data %>% 
    select(c_square, lon = mid_lon, lat = mid_lat, sar = SurfSAR, ssar = SubsurfSAR) %>% 
    mutate(c_square = as.character(c_square)) %>% 
    gather(variable, lastyear, -c(c_square, lon, lat))
  
 # Merge the data and calculate the difference in the sar value
  d <-
    thisyear %>%
    full_join(lastyear) %>%
    mutate(thisyear = ifelse(is.na(thisyear), 0, thisyear),             # new code
           lastyear = ifelse(is.na(lastyear), 0, lastyear)) %>%         # new code
    mutate(diff = thisyear - lastyear) %>%
    tbl_df()
  

  # Create a raster object for difference in the sar only
  r <- 
    d %>% 
    filter(variable == "sar") %>% 
    dplyr::select(lon, lat, diff) %>% 
    raster::rasterFromXYZ()
  sp::proj4string(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # peek on the distribution of the sar difference
  hist(raster::values(r))
  
  # Generate a colour function
  mx <- max(abs(raster::values(r)), na.rm = TRUE)
  pal <- colorNumeric(palette = c("red", "white", "blue"),
                      domain = c(raster::values(r), -mx, mx),
                      reverse = TRUE,
                      na.color = "transparent")
  
  # Generate a leaflet map
  m <- 
    leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addRasterImage(r, colors = pal, opacity = 1, group = "Difference") %>%
    addLegend(pal = pal, values = c(raster::values(r), -mx, mx),
              title = "dsar")
  m
  
  # If you want to save the data as a html-file to be open in a browser
  #saveWidget(m, file="leaflet_demo.html", selfcontained = TRUE)
  
  # ------------------------------------------------------------------------------
  # Add SAR values from the 2018 datacall
  # the script is only a sligth modification from the above
  
  d <-
    thisyear %>% 
    full_join(lastyear) %>% 
    mutate(value = thisyear) %>% 
    tbl_df()

 r <- 
    d %>% 
    filter(variable == "sar") %>% 
    dplyr::select(lon, lat, value) %>% 
    raster::rasterFromXYZ()
  sp::proj4string(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  pal <- colorNumeric(palette = viridis(20, option = "B", direction = 1),
                      domain = raster::values(r),
                      reverse = TRUE,
                      na.color = "transparent")
  # Add the new layer
  m <-
    m %>% 
    addRasterImage(r, colors = pal, opacity = 1, group = paste(datacalls[2], "datacall year:", years[2])) %>% 
    addLegend(pal = pal, values = raster::values(r),
              title = "SAR")
  
  # ------------------------------------------------------------------------------
  # Add SAR values from the 2017 datacall
  
  d <-
    thisyear %>% 
    full_join(lastyear) %>% 
    mutate(value = lastyear) %>% 
    tbl_df()

  r <- 
    d %>% 
    filter(variable == "sar") %>% 
    dplyr::select(lon, lat, value) %>% 
    raster::rasterFromXYZ()
  sp::proj4string(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  pal <- colorNumeric(palette = viridis(20, option = "B", direction = 1),
                      domain = raster::values(r),
                      reverse = TRUE,
                      na.color = "transparent")
  # Add the new layer
  m <-
    m %>% 
    addRasterImage(r, colors = pal, opacity = 1, group = paste(datacalls[1], "datacall year:", years[1])) %>% 
    addLayersControl(baseGroups = c("diff", paste(datacalls[2], "datacall year:", years[2]), paste(datacalls[1], "datacall year:", years[1])),
                     #overlayGroups = c("Quakes", "Outline"),
                     options = layersControlOptions(collapsed = FALSE))
  m
  saveWidget(m, file=paste0(gsub(" ", "_", label[i]), "_datacall_comparison.html"), selfcontained = TRUE, title = label[i])
  cp(paste0(gsub(" ", "_", label[i]), "_datacall_comparison.html"), "QC/reports_2018/leaflet", move = TRUE)
}