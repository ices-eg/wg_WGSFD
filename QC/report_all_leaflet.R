
library(data.table)
library(dplyr)
library(raster)
library(leaflet)
library(htmlwidgets)
library(icesTAF)
library(sf)

# read ospar regions
ospar <- read_sf("data/shapefiles", "ospar")


countries <- "2017"
years <- 2009:2017

layers <- expand.grid(year = years, country = countries)

rasts <- 
  lapply(1:nrow(layers), function(i) {
  
    country <- layers$country[i]; 
    year <- layers$year[i]
    
    vms <- fread(sprintf("data/QC_2018/ICES_VE_%s.csv", country)) %>% .[year == layers$year[i]] %>% as.data.frame
    msg(country, ":", nrow(vms), " rows")
    
    if (nrow(vms) == 0) return(NULL)
    
    # make raster
    resolution <- 0.05
    loc <- as.matrix(vmstools::CSquare2LonLat(vms$c_square, degrees = resolution))[,2:1]
    colnames(loc) <- c('X', 'Y')
    
    # set up an 'empty' raster, here via an extent object derived from your data
    r <- raster(extent(loc)  + resolution/2,
                resolution = resolution,
                crs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    r <- rasterize(loc, r, vms$kw_fishinghours, fun = "sum")
    
    r
  })
names(rasts) <- paste0(layers$country, ": ", layers$year)
rasts <- rasts[!sapply(rasts, is.null)]


msg("making totals map")
m <- 
  leaflet(ospar) %>% 
  addTiles() %>%
  addProviderTiles(providers$Esri.OceanBasemap)

# add layers
rnames <- names(rasts)
for (layer in rnames) {
  if (is.null(rasts[[layer]])) next
  m <- addRasterImage(m, rasts[[layer]], opacity = 0.8, group = layer)
}

# add ospar
m <- addPolygons(m, fill = NA, weight = 1)

# Layers control
m <- addLayersControl(m,
      baseGroups = rnames,
      options = layersControlOptions(collapsed = FALSE))

msg("saving map")
fname <- "fishing_intensity_by_year.html"
saveWidget(m, file = fname)
cp(fname, "QC/reports_2018", move = TRUE)
