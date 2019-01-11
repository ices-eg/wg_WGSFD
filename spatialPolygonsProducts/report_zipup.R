
# HELCOM

setwd("spatialPolygonsProducts/maps/_ICES_VMS_Datacall_VMS/HELCOM")

zip(zipfile = "../../ICES.2017.Maps-HELCOM-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")


setwd("spatialPolygonsProducts/shapefiles/_ICES_VMS_Datacall_VMS/HELCOM/")

zip(zipfile = "../../ICES.2017.Shapefiles-HELCOM-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")


# OSPAR - 2017

setwd("spatialPolygonsProducts/maps/_ICES_VMS_Datacall_VMS/OSPAR")

zip(zipfile = "../../ICES.2017.Maps-OSPAR-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")


setwd("spatialPolygonsProducts/shapefiles/_ICES_VMS_Datacall_VMS/OSPAR/")

zip(zipfile = "../../ICES.2017.Shapefiles-OSPAR-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")


# OSPAR - 2018

setwd("spatialPolygonsProducts/maps/_ICES_VMS_Datacall_VMS/OSPAR")

zip(zipfile = "../../ICES.2018.Maps-OSPAR-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")


setwd("spatialPolygonsProducts/shapefiles/_ICES_VMS_Datacall_VMS/OSPAR/")

zip(zipfile = "../../ICES.2018.Shapefiles-OSPAR-spatial-data-fishing-intensity.zip", 
    files = list.files(".", recursive = TRUE))

setwd("../../../..")
