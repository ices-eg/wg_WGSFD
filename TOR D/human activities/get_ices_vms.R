
# years is 2009 - 2018
# ices ecoregions https://www.ices.dk/community/advisory-process/Pages/ICES-ecosystems-and-advisory-areas.aspx

library(devtools)
#install_github("ices-tools-prod/icesVMS")
library(sf)



library(icesVMS)
icesVMS::update_token()

years <- 2009:2018
f_cats <- c("Beam", "Otter", "Dredge", "Seine")
#f_cat <- "Beam"
year<- 2014

for ( year in years ) { 
  print(year)
  for (f_cat in f_cats) { 
    print(f_cat)
    celtic_fa <- icesVMS::get_wgfbit_data1("Celtic Seas", year, fishing_category = f_cat)
    table_id <- Id(schema = "fishing_activity_ices" , table = tolower( paste0("celtic_", f_cat, "_",year) ) )
    #DBI::dbWriteTable(con, name=  table_id,value = bargrid,  overwrite= T, row.names = FALSE)
    celtic_fa<-st_as_sf(celtic_fa, wkt = "wkt")
    sf::st_write_db(conn = con, table = table_id, obj = celtic_fa , drop =)
    print(paste0("celtic_", f_cat, "_",year, " it have been succesful uploaded!"))
    
  } 
  
  
} 



DG-ENV habitat impact , intermediat stage. Fisingh , main fishign gorunds and looks and . How much GES you would get , advice request very detailed. 
What is the main fishign grounds. 









# total surface / subsurface swept 
northSea_total <- icesVMS::get_wgfbit_data2("Greater North Sea", 2018)
# total surface / subsurface swept 
celticseas_total <- icesVMS::get_wgfbit_data2("Celtic Seas", 2018)

year<- 2018 
table_id <- Id(schema = "fishing_activity_ices" , table = tolower( paste0("celticseas_total_",year) ) )
#DBI::dbWriteTable(con, name=  table_id,value = bargrid,  overwrite= T, row.names = FALSE)
celticseas_total<-st_as_sf(celticseas_total, wkt = "wkt")
sf::st_write_db(conn = con, table = table_id, obj = celticseas_total )



# individual metiers
northSea_otcru <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_CRU")
northSea_otdmf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_DMF")
northSea_otmix <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_MIX")
northSea_otmixcrudmf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_MIX_CRU_DMF")
northSea_otmixdmfben <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_MIX_DMF_BEN")
northSea_drbmol <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "DRB_MOL")
northSea_otspf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "OT_SPF")
northSea_sdndmf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "SDN_DMF")
northSea_sscdmf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "SSC_DMF")
northSea_tbbcru <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "TBB_CRU")
northSea_tbbdmf <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "TBB_DMF")
northSea_tbbmol <- icesVMS::get_wgfbit_data1("Greater North Sea", 2018, benthis_metier = "TBB_MOL")


