## Use the stars pacakge to load the raster into R env

```r

library(stars)
library(sf)

eubbht = "C:\\Users\\RM12\\Downloads\\euseamap_2021_atlantic_100m.tif"
bbht_ras = read_stars(eubbht)


rp_sft = {your point dataset: random dummy point or eflalotacsat vms}
e <- st_extract(x = bbht_ras, rp_sft ) 


```
