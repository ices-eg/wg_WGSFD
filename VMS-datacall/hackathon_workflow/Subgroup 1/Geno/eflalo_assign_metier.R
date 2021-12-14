
### Load libraries
library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(lubridate)
library(sp)
library(dplyr)
library(rgdal)

### Import necessary functions
path2fun <- "VMS-datacall\\hackathon_workflow\\Subgroup 1\\Geno"
source(file.path(path2fun, "./Scripts/Functions.R"))

### Create input data
input.data <- data.table(Country = eflalo$VE_COU,
                         year = year(as.POSIXct(eflalo$FT_DDAT[1],format="%d/%m/%Y")),
                         vessel_id = eflalo$VE_REF,
                         vessel_length = eflalo$VE_LEN,
                         trip_id = eflalo$FT_REF,
                         haul_id = eflalo$LE_ID,
                         fishing_day = eflalo$LE_CDAT,
                         area = NA,
                         ices_rectangle = eflalo$LE_RECT,
                         gear = eflalo$LE_GEAR,
                         gear_FR = NA,
                         mesh = eflalo$LE_MSZ,
                         selection = NA,
                         registered_target_assemblage = NA,
                         FAO_species = NA,
                         metier_level_6 = NA,
                         measure =NA,
                         KG = NA,
                         EUR = NA)

validateInputDataFormat(input.data) # the input needs to be a data.table!

### Load reference lists
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv"
area.list <- loadAreaList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202020.xlsx"
species.list <- loadSpeciesList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"
metier.list <- loadMetierList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Code-ERSGearType-v1.1.xlsx"
gear.list <- loadGearList(url)
assemblage.list <- unique(c(species.list$species_group, species.list$dws_group))
assemblage.list <- assemblage.list[!is.na(assemblage.list)]
rm(url)

### Input data codes validation
validateInputDataCodes(input.data, gear.list, area.list, species.list)

### Input data preparation
#### Basic conversions/recodings
input.data[,EUR:=as.numeric(EUR)]
input.data[,KG:=as.numeric(KG)]
input.data[,c("selection_type","selection_mesh"):=data.table(str_split_fixed(selection,"_",2))]
input.data[,selection_type:=ifelse(selection_type=="",NA,selection_type)]
input.data[,selection_mesh:=ifelse(selection_mesh=="",NA,selection_mesh)]

#### Assign RCG names to the input data
##### Get FAO area code for each fishing operation (use centroid)
lons <- eflalo %>% select(LE_SLON, LE_ELON) %>% rowMeans()
lats <- eflalo %>% select(LE_SLAT, LE_ELAT) %>% rowMeans()

eflalosp <- SpatialPoints(cbind(lons, lats))
proj4string(eflalosp)=CRS("+init=epsg:4326")

# need to write a wrapper for next three lines
fao <- readOGR(dsn = file.path(path2fun,"Shapefiles"), layer = "FAO_AREAS_SUBDIVISION")
proj4string(fao)=CRS("+init=epsg:4326")
x <- over(eflalosp,fao)

input.data <- input.data %>% mutate(F_CODE1 = as.character(x$F_SUBDIVIS))

fao <- readOGR(dsn = file.path(path2fun,"Shapefiles"), layer = "FAO_AREAS_DIVISION")
proj4string(fao)=CRS("+init=epsg:4326")
x <- over(eflalosp,fao)

input.data <- input.data %>% mutate(F_CODE2 = as.character(x$F_SUBDIVIS))

input.data <- input.data %>%
  transform(area = ifelse((is.na(F_CODE1) & !is.na(F_CODE2)), F_CODE2, F_CODE1))%>%
  select(-c(F_CODE1, F_CODE2))






