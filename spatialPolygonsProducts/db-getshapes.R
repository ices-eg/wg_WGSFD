

library(raster)

rm(list = ls())

# create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/shapefiles")) dir.create("data/shapefiles")

# download data ----

# open dB connection for shape files
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=GDB;Trusted_Connection=yes'
conn <- RODBC::odbcDriverConnect(connection = dbConnection)

# get OSPAR region shapes
ospar <- RODBC::sqlQuery(conn, "SELECT shape.STAsText() as wkt FROM QUERYREF_OSPAR_REGIONS_20091214", rows_at_time = 1)$wkt
ospar <- do.call(bind, lapply(ospar, rgeos::readWKT))
ospar <- as(ospar, "SpatialPolygonsDataFrame")
proj4string(ospar) <- CRS("+init=epsg:4326")
rgdal::writeOGR(ospar, "data/shapefiles", "ospar", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# get HELCOM region shapes
helcom <- RODBC::sqlQuery(conn, "SELECT geom.STAsText() as wkt FROM QueryRef_Baltic_polygon", rows_at_time = 1)$wkt
helcom <- rgeos::readWKT(helcom)
helcom <- as(helcom, "SpatialPolygonsDataFrame")
proj4string(helcom) <- CRS("+init=epsg:4326")
rgdal::writeOGR(helcom, "data/shapefiles", "helcom", driver = "ESRI Shapefile", overwrite_layer = TRUE)

RODBC::odbcClose(conn)



# get european coastline shapefiles
download.file("http://data.openstreetmapdata.com/land-polygons-generalized-3857.zip",
              "data/land-polygons-generalized-3857.zip")

unzip(zipfile = "data/land-polygons-generalized-3857.zip",
      overwrite = TRUE,
      exdir = "data/shapefiles")

unlink("data/land-polygons-generalized-3857.zip")

# read coastline shapefiles and transform to wgs84
coast <- rgdal::readOGR("data/shapefiles/land-polygons-generalized-3857", "land_polygons_z5", verbose = FALSE)
unlink("data/shapefiles/land-polygons-generalized-3857", recursive = TRUE)

coast <- spTransform(coast, CRS("+init=epsg:4326"))

# trim coastline to extent for ploting
bbox <- as(extent(bind(ospar, helcom)), "SpatialPolygons")
proj4string(bbox) <- CRS("+init=epsg:4326")
coast <- rgeos::gIntersection(coast, bbox, byid = TRUE)
coast <- rgeos::gUnaryUnion(coast)
coast <- as(coast, "SpatialPolygonsDataFrame")

# save coastline to shapefiles folder
rgdal::writeOGR(coast, "data/shapefiles", "coast", driver = "ESRI Shapefile", overwrite_layer = TRUE)

