library(sf)
library(dplyr)
library(vmstools)


 

## bounding box area limits definition


##ICES REctangles boundaries
lon = c(-44,68.5)
lat = c(36,85.5)
 
 

 ices_sq_res= 1
 ices_sq_res_offset <- ices_sq_res/2
 
 lon_ct = seq(-2 + ices_sq_res_offset, 0,ices_sq_res)
 lat_ct = seq(40 + ices_sq_res_offset, 45, ices_sq_res)  

spatial_grid_0p01_ct = expand.grid( lon_ct = lon_ct, lat_ct = lat_ct   )  



csquare_resolution = c(0.01, 0.05, 0.5)

for ( i in unique (csquare_resolution)) {
  print(i)
 cs_code  = CSquare(spatial_grid_0p01_ct$lon_ct, spatial_grid_0p01_ct$lat_ct,degrees =  i ) 
 spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( cs_code = cs_code   )
 colnames (spatial_grid_0p01_ct) [ ncol(spatial_grid_0p01_ct) ] = paste0("cs_code", i ) 
    
 }

library ( ggplot2)


### Calculate the ICES grid cell area

spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_1 = lon_ct - ices_sq_res_offset, lat_cn_1 = lat_ct - ices_sq_res_offset)
spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_2 = lon_ct + ices_sq_res_offset, lat_cn_2 = lat_ct - ices_sq_res_offset)
spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_3 = lon_ct + ices_sq_res_offset, lat_cn_3 = lat_ct + ices_sq_res_offset)


ggplot( spatial_grid_0p01_ct) + 
geom_point( aes(x= lon_ct , y = lat_ct ), color = 'red') + 
geom_point( aes(x= lon_cn_1 , y = lat_cn_1 ), color = 'green') + 
geom_point( aes(x= lon_cn_2 , y = lat_cn_2 ), color = 'green') + 
geom_point( aes(x= lon_cn_3 , y = lat_cn_3 ), color = 'green')  


spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate(x_diff =  abs (lon_cn_1 - lon_cn_2  ), y_diff = abs (lat_cn_2 - lat_cn_3 ) ) %>%
                        mutate ( d =  sqrt( (x_diff^2 + y_diff^2)  ) , d_m =d *  111.139   )



outer = matrix(c(-2,40,-1,40),ncol=2, byrow=TRUE)



st_sf(id = 'L1', geom =  st_sfc(st_linestring(outer, dim = "XY"))) %>%
  st_set_crs(4326) %>% st_transform(9822) %>%
mutate ( line_dist = st_length(geom))
 
st_linestring(outer) %>% st_as_sf( wkt = "geometry" ) 

?st_linestring
  
  st_set_crs(4326) %>% st_transform(., 9822)


spatial_grid_0p01_ct %>% summary()


 




### Plot it 

#spatial_grid_0p01 = spatial_grid_0p01 %>% mutate( f_effort =  runif( dim(spatial_grid_0p01 )[1], 10, 100) ) %>% slice( 1:100)



spatial_grid_0p01_geom = spatial_grid_0p01_ct %>% 
                         sf::st_as_sf(coords = c('lon_ct', 'lat_ct' ))%>%
                            st_set_crs(4326)
                          # %>% st_join ()


ggplot( spatial_grid_0p01_geom ) + geom_sf()
        
bbox = spatial_grid_0p01_geom %>% st_bbox()   %>% st_as_sfc() %>% 
  st_sf( id  = 1, label = 'bbox' )  %>%
  st_set_crs(4326)


gridp = st_make_grid(bbox, cellsize = ices_sq_res, crs = 4326  , 
                    offset =  c( min(spatial_grid_0p01_ct$lon_ct) - ices_sq_res_offset,
                                 min(spatial_grid_0p01_ct$lat_ct)-  ices_sq_res_offset)
                    ) %>% st_as_sf() 

        
## Add table information to the grid 

spatial_grid_0p01_cells = gridp %>% cbind( spatial_grid_0p01_ct)

### Plot the grid   


ggplot(spatial_grid_0p01_cells )   + geom_sf () +  geom_sf ( data = spatial_grid_0p01_geom)  +
  geom_sf_label(aes(label = cs_code0.5) , size = 2)


 

### Dowsampling 


spatial_grid_effort_0p05 = spatial_grid_0p01 %>% group_by(cs_code0.05 ) %>% summarise(f_effort = sum ( f_effort))


### Multiresolucion 
