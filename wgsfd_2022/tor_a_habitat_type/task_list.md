## 1(a) background task ( only once ) 

1. download the emodnet euseamap ( https://www.emodnet-seabedhabitats.eu/access-data/download-data/?linkid=eusm_2021_atlantoarctic,eusm_2021_baltic,eusm_2021_blacksea,eusm_2021_mediterranean ) 
2. rasterize to MSFDBHT ( QGIS/ ArcGIS ) 
	- torsten rasterize to 0.001
	- 
3.a - Pretiling dat ain QGIS/Arc at ICES rectangle tile size
3.b - Raster into to process with raster package 

4.Output provided into Renv raster format ( )

## 1 (b) backgroud task ( jeppe approach/  script available  ) 

1.Dowload the emodnet eusemaP ( https://www.emodnet-seabedhabitats.eu/access-data/download-data/?linkid=eusm_2021_atlantoarctic,eusm_2021_baltic,eusm_2021_blacksea,eusm_2021_mediterranean) 

2. load the eusemap to R . Convert into sf dataframe
3. clean up the dataset
4.Output provided into R polygon format (e.g. 



## workflow task (run by data submmiters) 


1. VMS point to habitat polygon spatial operation (overlay sf operation ) 
	- create dummy random VMS points dataset across north sea region
	  ( 500,000 , 1,000,000, 5,000,000 , 15,000,000 ) 

2. additional column in table 1 with habitat type 


