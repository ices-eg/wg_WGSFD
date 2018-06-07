
devtools::install_github("ices-tools-prod/RODBC")

library(raster)
library(sf)
library(RODBC)

rm(list = ls())

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/shapefiles")) dir.create("data/shapefiles")

# download data ----

# open dB connection for shape files
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=GDB;Trusted_Connection=yes'
conn <- odbcDriverConnect(connection = dbConnection)

# get OSPAR region shapes
ospar <- sqlQuery(conn, "SELECT Region, Name, shape.STAsText() as wkt FROM QUERYREF_OSPAR_REGIONS_20091214", rows_at_time = 1)
ospar <- st_as_sf(ospar, wkt = "wkt", crs = 4326)
write_sf(ospar, "data/shapefiles", "ospar", driver = "ESRI Shapefile")


# get HELCOM region shapes
helcom <- sqlQuery(conn, "SELECT geom.STAsText() as wkt FROM QueryRef_Baltic_polygon", rows_at_time = 1)
helcom <- st_as_sf(helcom, wkt = "wkt", crs = 4326)
write_sf(helcom, "data/shapefiles", "helcom", driver = "ESRI Shapefile", update = TRUE)

odbcClose(conn)



# get european coastline shapefiles
download.file("http://data.openstreetmapdata.com/land-polygons-generalized-3857.zip",
              "data/land-polygons-generalized-3857.zip")

unzip(zipfile = "data/land-polygons-generalized-3857.zip",
      overwrite = TRUE,
      exdir = "data/shapefiles")

unlink("data/land-polygons-generalized-3857.zip")

# read coastline shapefiles and transform to wgs84
coast <- read_sf("data/shapefiles/land-polygons-generalized-3857", "land_polygons_z5")
#unlink("data/shapefiles/land-polygons-generalized-3857", recursive = TRUE)

coast <- st_transform(coast, 4326)

# trim coastline to extent for ploting
#bbox <- as(extent(st_union(ospar, helcom)), "SpatialPolygons")
#proj4string(bbox) <- CRS("+init=epsg:4326")
#coast <- rgeos::gIntersection(coast, bbox, byid = TRUE)
coast <- aggregate(coast, list(world = rep(1, nrow(coast))), mean)

# save coastline to shapefiles folder
write_sf(coast, "data/shapefiles", "coast", driver = "ESRI Shapefile", update = TRUE)


