## Create nested ICES Grids using C-square Notation ##

## libraries 
library(sf)
library(dplyr)
library(vmstools)
library ( ggplot2)

## ICES Rectangles boundaries
lon = c(-44,68.5)
lat = c(36,85.5)

# limit boundaries for testing purposes
lon = c(-4, 2)
lat = c(40, 70)

# set grid cell resolution and calculate offset value
ices_sq_res= 0.05
ices_sq_res_offset <- ices_sq_res/2
 
# create grid centroids
lon_ct = seq(lon[1] + ices_sq_res_offset, lon[2],ices_sq_res)
lat_ct = seq(lat[1] + ices_sq_res_offset, lat[2], ices_sq_res)  

spatial_grid_0p01_ct = expand.grid( lon_ct = lon_ct, lat_ct = lat_ct)  

# Loop through VMS tools package to create csquare notation at a range of resolutions
csquare_resolution = c(0.01, 0.05, 0.5)

for ( i in unique (csquare_resolution)) {
  print(i)
 cs_code  = CSquare(spatial_grid_0p01_ct$lon_ct, spatial_grid_0p01_ct$lat_ct,degrees =  i ) 
 spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( cs_code = cs_code   )
 colnames (spatial_grid_0p01_ct) [ ncol(spatial_grid_0p01_ct) ] = paste0("cs_code", i ) 
    
 }

### Calculate the ICES grid cell area

# calculate centroids
spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_1 = lon_ct - ices_sq_res_offset, lat_cn_1 = lat_ct - ices_sq_res_offset)
spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_2 = lon_ct + ices_sq_res_offset, lat_cn_2 = lat_ct - ices_sq_res_offset)
spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate ( lon_cn_3 = lon_ct + ices_sq_res_offset, lat_cn_3 = lat_ct + ices_sq_res_offset)

# ggplot( spatial_grid_0p01_ct) + 
# geom_point( aes(x= lon_ct , y = lat_ct ), color = 'red') + 
# geom_point( aes(x= lon_cn_1 , y = lat_cn_1 ), color = 'green') + 
# geom_point( aes(x= lon_cn_2 , y = lat_cn_2 ), color = 'green') + 
# geom_point( aes(x= lon_cn_3 , y = lat_cn_3 ), color = 'green')  


# spatial_grid_0p01_ct = spatial_grid_0p01_ct %>% mutate(x_diff =  abs (lon_cn_1 - lon_cn_2  ), y_diff = abs (lat_cn_2 - lat_cn_3 ) ) %>%
#                         mutate ( d =  sqrt( (x_diff^2 + y_diff^2)  ) , d_m =d *  111.139   )


# spatial_grid_0p01_ct$distance_1_2 <- NA

# calculate the distance along the x and y axis using lines
# for (i in 1:nrow (spatial_grid_0p01_ct)) {
# 
#   # create points
#   point_1 = st_sfc (st_point (c(spatial_grid_0p01_ct$lon_cn_1[i], spatial_grid_0p01_ct$lat_cn_1[i])))
#   point_2 = st_sfc (st_point (c(spatial_grid_0p01_ct$lon_cn_2[i], spatial_grid_0p01_ct$lat_cn_2[i])))
#   point_3 = st_sfc (st_point (c(spatial_grid_0p01_ct$lon_cn_3[i], spatial_grid_0p01_ct$lat_cn_3[i])))
#   
#   point_1 = point_1 %>% st_set_crs(4326) %>% st_transform(9822)
#   point_2 = point_2 %>% st_set_crs(4326) %>% st_transform(9822)
#   point_3 = point_3 %>% st_set_crs(4326) %>% st_transform(9822)
#   
#   # create lines between points
#   line_1_2 = matrix (c(spatial_grid_0p01_ct$lon_cn_1[i], spatial_grid_0p01_ct$lat_cn_1[i], 
#                        spatial_grid_0p01_ct$lon_cn_2[i], spatial_grid_0p01_ct$lat_cn_2[i]), 
#                      ncol = 2, byrow = TRUE)
#   
#   line_2_3 = matrix (c(spatial_grid_0p01_ct$lon_cn_2[i], spatial_grid_0p01_ct$lat_cn_2[i], 
#                        spatial_grid_0p01_ct$lon_cn_3[i], spatial_grid_0p01_ct$lat_cn_3[i]), 
#                      ncol = 2, byrow = TRUE)
#   
#   # calculate distance between the points
#   spatial_grid_0p01_ct$distance_1_2_points[i] = as.numeric (st_distance(point_1,point_2))
#   spatial_grid_0p01_ct$distance_2_3_points[i] = as.numeric (st_distance(point_2,point_3))
#   
#   # calculate distance of the lines
#   line_1_2 = st_sf(id = 'L1_2', geom =  st_sfc(st_linestring(line_1_2, dim = "XY"))) %>%
#     st_set_crs(4326) %>% st_transform(9822) %>%
#     mutate ( line_dist = st_length(geom))
#   
#   line_2_3 = st_sf(id = 'L2_3', geom =  st_sfc(st_linestring(line_2_3, dim = "XY"))) %>%
#     st_set_crs(4326) %>% st_transform(9822) %>%
#     mutate ( line_dist = st_length(geom))
#   
#   # line_1_2$line_dist
#   # line_2_3$line_dist
#   
#   spatial_grid_0p01_ct$distance_1_2[i] = as.numeric (line_1_2$line_dist)
#   spatial_grid_0p01_ct$distance_2_3[i] = as.numeric (line_2_3$line_dist)
#   
# }

# # then multiply axes to calculate the area - BUT THESE ARE ACTUALLY TRAPEZIUMS NOT SQUARES
# spatial_grid_0p01_ct = spatial_grid_0p01_ct %>%
#   mutate (grid_area_lines_km2 = (distance_1_2 * distance_2_3)/1000000) 
# 
# spatial_grid_0p01_ct = spatial_grid_0p01_ct %>%
#   mutate (grid_area_points_km2 = (distance_1_2_points * distance_2_3_points)/1000000) 
# 
# # plot to check area over latitude
# ggplot( spatial_grid_0p01_ct) +
# geom_point( aes(x= lon_ct , y = lat_ct, size = grid_area_lines_km2), color = 'red') 
# # larger in lower latitudes, exactly as we expect

# outer = matrix(c(-2,40,-1,40),ncol=2, byrow=TRUE)

# st_sf(id = 'L1', geom =  st_sfc(st_linestring(outer, dim = "XY"))) %>%
#   st_set_crs(4326) %>% st_transform(9822) %>%
# mutate ( line_dist = st_length(geom))

# st_linestring(outer) %>% st_as_sf( wkt = "geometry" ) 

# ?st_linestring
  
  # st_set_crs(4326) %>% st_transform(., 9822)

## Add ICES rectangles using centroids
spatial_grid_0p01_ct$ICES_rectangle = mapplots::ices.rect2(lon = lon_ct, lat = lat_ct)

### Plot it 

#spatial_grid_0p01 = spatial_grid_0p01 %>% mutate( f_effort =  runif( dim(spatial_grid_0p01 )[1], 10, 100) ) %>% slice( 1:100)

## Transform to grid cells
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

### use st_area/st_geod_area to calculate the area of the cell
spatial_grid_0p01_cells$grid_area_st_area = as.numeric(st_area(spatial_grid_0p01_cells)/1000000)
# spatial_grid_0p01_cells$grid_area_st_geod_area = as.numeric(lwgeom::st_geod_area(spatial_grid_0p01_cells)/1000000)

# # difference between area using different methods
# spatial_grid_0p01_cells %>% 
#   mutate (grid_area_diff = (grid_area_lines_km2 - grid_area_st_area)) 
# USING CORNERS AND DISTANCE BETWEEN LINES/POINTS GIVES A LARGER AREA AND GETS LARGER AS YOU INCREASE LATITUDE
# there is also a difference between using st_area and st_geod_area but this is less marked

## plot the differences between grid cell by latitude using different methods
# plot = reshape2::melt (data.frame(spatial_grid_0p01_cells)[,c(1:2, 16:17, 20:21)], id.vars = c("lon_ct", "lat_ct"))

# ggplot (plot, aes (x = lat_ct, y = value, colour = variable)) + 
#   geom_point ()

### Plot the grid 

ggplot(spatial_grid_0p01_cells, aes (fill = grid_area_st_area))   + geom_sf () # +  
  # geom_sf ( data = spatial_grid_0p01_geom)  +
  # geom_sf_label(aes(label = cs_code0.5) , size = 2)

## Create plot to show points with an equal distribution (in km) of fishing points
# across a range of different latitudes to show how the 

### Dowsampling  ##############

spatial_grid_effort_0p05 = spatial_grid_0p01 %>% group_by(cs_code0.05 ) %>% summarise(f_effort = sum ( f_effort))


### Multiresolucion #############





### Areas base bias analysis #################



lat_south = runif( 100, 36, 36.1)
lon_south = runif( 100, 0, 0.1)



plot ( lon_south, lat_south)


df = data.frame( lon_south = lon_south, lat_south = lat_south)
df  %>% sf::st_as_sf(coords = c('lon_south' , 'lat_south' )) %>%st_set_crs(4326)
