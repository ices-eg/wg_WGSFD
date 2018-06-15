## Prepare plots/tables for report

## Before:
## After:

# libraries
library(icesTAF)
library(raster)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)
library(viridis)
library(data.table)

# utilities
source("wgsfd_2018_dataset/utilities.R")

# settings
year <- 2017
datacall_year <- 2018
gear <- "TBB_DEF_16-31_0_0"


# get layer
vms_data <- 
  sprintf("data/QC_2018/ICES_VE_%i.csv", datacall_year) %>%
  fread 

vms <- 
  vms_data %>% 
  as.data.frame %>%
  filter(LE_MET_level6 == gear, year == year) %>%
  group_by(c_square, country, year, LE_MET_level6) %>%
  summarise(kw_fishinghours = sum(kw_fishinghours)) %>%
  group_by(c_square, year, LE_MET_level6) %>%
  summarise(composition = paste(country, round(kw_fishinghours), collapse = ", "),
            kw_fishinghours = sum(kw_fishinghours)) %>%
  with(makesf(c_square, .))
  
# palette
pal <- colorNumeric(palette = viridis(20, option = "B", direction = 1),
                      domain = vms$kw_fishinghours,
                      reverse = TRUE,
                      na.color = "transparent")
# labels
labels <-
  sprintf(
    "<strong>%s</strong><br/>%s<br/>%s",
    vms$c_square,
    vms$composition,
    vms$LE_MET_level6
  ) %>%
  lapply(htmltools::HTML)

# make map
m <-
  leaflet(vms)%>%
  addTiles() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addPolygons(
    fillColor = ~pal(kw_fishinghours),
    weight = 0.1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) 
m

# save map
saveWidget(m, file = "c_square_polygons.html")
cp("c_square_polygons.html", "QC/reports_2018/leaflet", move = TRUE)
