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
# also need sp- and raster-package, but needed function are called directly
# in the code below

# Read in the 2015 data from data call 2016 and 2017
# what shapes are there:
file2016 <- 
  dir("deliveries/_2016_ICES_VMS_Datacall_VMS", pattern = "shp", full.names = TRUE)
i <- str_detect(file2016, "PEL")
file2016 <- file2016[!i]
file2017 <- dir("deliveries/2015", pattern = "shp", full.names = TRUE)

# Get the total intensity file
i <- 17 # The total intensity file

# 2017 datacall data
thisyear <- 
  rgdal::readOGR(paste0("/home/einarhj/prj2/vms2/wgsfd2017/", file2017[i]))@data %>%
  select(c_square, lon = mid_lon, lat = mid_lat, sar = SurfSAR, ssar = SubsurfSAR) %>% 
  mutate(c_square = as.character(c_square)) %>% 
  gather(variable, thisyear, -c(c_square, lon, lat))
# 2016 datacall data
lastyear <- 
  rgdal::readOGR(paste0("/home/einarhj/prj2/vms2/wgsfd2017/", file2016[i]))@data %>% 
  select(c_square, lon = mid_lon, lat = mid_lat, sar = SurfSAR, ssar = SubsurfSAR) %>% 
  mutate(c_square = as.character(c_square)) %>% 
  gather(variable, lastyear, -c(c_square, lon, lat))

# Merge the data and calculate the difference in the sar value
d <-
  thisyear %>% 
  full_join(lastyear) %>% 
  mutate(diff = thisyear - lastyear) %>% 
  tbl_df()

# Trim the data excluding below 0.001 and above 0.999
p <- 0.999
q <-
  d %>% 
  group_by(variable) %>% 
  summarise(lower = quantile(diff, 1 - p, na.rm = TRUE),
            upper = quantile(diff, p, na.rm = TRUE))
d <-
  d %>% 
  left_join(q) %>% 
  mutate(diff = ifelse(diff < lower, lower, diff),
         diff = ifelse(diff > upper, upper, diff))

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
  #addTiles() %>% 
  addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>% 
  addRasterImage(r, colors = pal, opacity = 1, group = "Difference") %>%
  addLegend(pal = pal, values = c(raster::values(r), -mx, mx),
            title = "dsar")
m

# If you want to save the data as a html-file to be open in a browser
#saveWidget(m, file="leaflet_demo.html", selfcontained = TRUE)

# ------------------------------------------------------------------------------
# Add SAR values from the 2017 datacall
# the script is only a sligth modification from the above

d <-
  thisyear %>% 
  full_join(lastyear) %>% 
  mutate(value = thisyear) %>% 
  tbl_df()
# Cap the SAR values
p <- 0.98
q <-
  d %>% 
  group_by(variable) %>% 
  summarise(upper = quantile(value, p, na.rm = TRUE))
d <-
  d %>% 
  left_join(q) %>% 
  mutate(value = ifelse(value > upper, upper, value))
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
  addRasterImage(r, colors = pal, opacity = 1, group = "2017 datacall") %>% 
  addLegend(pal = pal, values = raster::values(r),
            title = "SAR")

# ------------------------------------------------------------------------------
# Add SAR values from the 2016 datacall

d <-
  thisyear %>% 
  full_join(lastyear) %>% 
  mutate(value = lastyear) %>% 
  tbl_df()
# Cap the SAR values
#p <- 0.98
#q <-
#  d %>% 
#  group_by(variable) %>% 
#  summarise(upper = quantile(value, p, na.rm = TRUE))
d <-
  d %>% 
  left_join(q) %>% 
  mutate(value = ifelse(value > upper, upper, value))
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
  addRasterImage(r, colors = pal, opacity = 1, group = "2016 datacall") %>% 
  addLayersControl(baseGroups = c("diff", "2017 datacall", "2016 datacall"),
                   #overlayGroups = c("Quakes", "Outline"),
                   options = layersControlOptions(collapsed = FALSE))
m
saveWidget(m, file="leaflet_demo.html", selfcontained = TRUE)