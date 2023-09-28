36°N and 85°30'N and 44°W and 68°30'E




library(sf)
library(dplyr)
library(vmstools)


cell_resolution = csquare_resolution/  n_divisions  

## bounding box area limits definition


##ICES REctangles boundaries
lon = c(-44,68.5)
lat = c(36,85.5)

## Testing boudnaries 

lon = c(-2,2)
lat = c(40,50)


coord_bbox = data.frame(lon, lat)
bbox_aoi =  coord_bbox %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_bbox() %>% st_as_sfc() %>% 
  st_sf( id  = 1, label = 'bbox' ) %>%
  st_set_crs(4326)


## create the grid

grid_1  = st_make_grid( bbox_aoi,  cellsize = cell_resolution, square = TRUE, offset = c(min(lon),min(lat))  ) #%>% as(Class = "Spatial")



### Calcule grid centroi with no geomtry 

( 85.5 - 35 ) /0.01 

( -44 - 68.5 ) /0.01 

cell_resolution = csquare_resolution/  n_divisions  

 lon = seq(-20, 20, 0.01)
 lat = seq(35, 80, 0.01)  

 ices_sq_res= 0.5
 lon = seq(-2 + ices_sq_res / 2, 0,0.5)
 lat = seq(40 + ices_sq_res/2 , 45, 0.5)  

spatial_grid_0p01_ct = expand.grid( lon = lon, lat = lat   )  



csquare_resolution = c(0.01, 0.05)

for ( i in unique (csquare_resolution)) {
  print(i)
 cs_code  = CSquare(spatial_grid_0p01_ct$lon, spatial_grid_0p01_ct$lat,degrees =  i ) 
 spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( cs_code = cs_code   )
 colnames (spatial_grid_0p01_ct) [ ncol(spatial_grid_0p01_ct) ] = paste0("cs_code", i ) 
    
 }

library ( ggplot2)






### Plot it 

#spatial_grid_0p01 = spatial_grid_0p01 %>% mutate( f_effort =  runif( dim(spatial_grid_0p01 )[1], 10, 100) ) %>% slice( 1:100)



spatial_grid_0p01_geom = spatial_grid_0p01_ct %>% 
                         sf::st_as_sf(coords = c('lon', 'lat' ))%>%
                            st_set_crs(4326)
                          # %>% st_join ()


ggplot( spatial_grid_0p01_geom ) + geom_sf()
        
bbox = spatial_grid_0p01_geom %>% st_bbox()   %>% st_as_sfc() %>% 
  st_sf( id  = 1, label = 'bbox' )  %>%
  st_set_crs(4326)



gridp = st_make_grid(bbox, cellsize = 0.5, crs = 4326  , offset =  c( min(spatial_grid_0p01$lon) - 0.25, min(spatial_grid_0p01$lat)-  0.25)) %>% st_as_sf() 


ggplot( ) + geom_sf (data = gridp) +   geom_sf ( data = spatial_grid_0p01_geom)  


g1 = st_make_grid(bbox, cellsize = 0.5, crs = 4326   ) %>% st_as_sf() %>% st_centroid() 

g1_ct = g1 %>%  st_coordinates()
 
g1  %>% cbind(  CSquare(g1_ct[,1], g1_ct[,2], degrees = 0.05 )  )
  
 



area_csquare = st_area(st_transform(spatial_grid_0p01, 9822) ) /1000000
spatial_grid_0p01 %>% mutate( area_km2 = area_csquare)


%>%
ggplot(.) + geom_sf ( (aes(color = largeIntv)) + 


grid_1  = st_make_grid( bbox_aoi,  cellsize = cell_resolution, square = TRUE, offset = c(min(lon),min(lat))  ) #%>% as(Class = "Spatial")



### Dowsampling 


spatial_grid_effort_0p05 = spatial_grid_0p01 %>% group_by(cs_code0.05 ) %>% summarise(f_effort = sum ( f_effort))


### Multiresolucion 
