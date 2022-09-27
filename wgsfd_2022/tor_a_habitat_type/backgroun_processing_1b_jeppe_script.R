rm(list=ls())

#Download pacman, and let that install the needed packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, sf, dplyr)

# Create 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


#Download eunis habitat shapefiles from:
#https://www.emodnet-seabedhabitats.eu/access-data/download-data/
# (I am using the EUSeaMaps, atlantic and baltic)

#Read habiats into R and clean them up
hab <- st_read("//ait-phnas03.win.dtu.dk/aqua$/aqua-q/gis/Dynamisk/GEOdata/BasicLayers/Habitats/EMODnet_2021/C20211108_EUSeaMap_2021_Arctic_Atlantic.gdb")
hab <- st_transform(hab, 4326)
hab <- st_zm(hab)
hab <- st_make_valid(hab)
hab <- hab[,"EUNIScomb"]   

hab2 <- st_read("//ait-phnas03.win.dtu.dk/aqua$/aqua-q/gis/Dynamisk/GEOdata/BasicLayers/Habitats/EMODnet_2021/C20211007_EUSeaMap_2021_Baltic_Sea.gdb")
hab2 <- st_transform(hab2, 4326)
hab2 <- st_zm(hab2)
hab2 <- st_make_valid(hab2)
hab2 <- hab2[,"EUNIScomb"]   


#Load you tacsateflalo file
load("//ait-phnas03.win.dtu.dk/aqua$/aqua-q/dfad/users/jepol/home/22-02-03_WGSFD_DatalCall_2022/Results/tacsatEflalo2021.RData")
# Make it into data.table and add unique id to each row 
setDT(tacsatEflalo)[, rowID := 1:.N]

#Load habitats files
load("//ait-phnas03.win.dtu.dk/aqua$/aqua-q/dfad/users/jepol/home/22-09-18_WGSFD_2022/Data/habitats.rdata")

#Make tacsatEflalo spatial
pts <- tacsatEflalo %>%
  sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>%
  sf::st_set_crs(4326)

#Add habitats from the atlantic/north sea
  out1 <- st_join(pts, hab, join = st_intersects)
  setDT(out1)[,geometry := NULL]
  out1 <- out1[!is.na(EUNIScomb)]
  
#Add habitats from the baltic sea
  out2 <- st_join(pts[pts$rowID %!in% out1$rowID,],
                  hab2, join = st_intersects)
  
  setDT(out2)[,geometry := NULL]
  out2 <- out2[!is.na(EUNIScomb)]
  
  #Combine them into a new tacsateflalo file, 
  #now with eunis habitat (EUNIScomb)
  tacsatEflalo2 <- rbindlist(list(out1, out2, tacsatEflalo[rowID %!in% c(out1$rowID, out2$rowID)]),
                   fill = T)
  
  tacsatEflalo2[, rowID := NULL]
  
  #Make a new tacsatEflalo
  
  tacsatEflalo <- data.frame(tacsatEflalo2)
  
  #Maybe save it also
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
