### this bit deals with the new field added for the 2020 datacall
# construct a table of annonymous vessel ids accross all years
VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!



 

# join onto data tables
table1 <- left_join(table1, VE_lut)
table2 <- left_join(table2, VE_lut)


# summarise output and save
table1Save <-
  table1 %>%separate(col = LE_MET ,   c("met4", "met5", "mesh" ), sep = '_', remove = FALSE)%>%separate(mesh , c("min", "max"))%>%
    group_by(RT,VE_COU,Year,Month,Csquare,LE_GEAR, met5, min, max, LE_MET,LENGTHCAT) %>%
    summarise(
      mean_si_sp = mean(SI_SP),
      sum_intv =sum(INTV),
      mean_ve_len = mean(VE_LEN),
      mean_ve_kf = mean(VE_KW),
      sum_kwHour = sum(kwHour),
      sum_le_kg_tot = sum(LE_KG_TOT),
      sum_le_euro_tot  = sum(LE_EURO_TOT),      
      n_vessels = n_distinct(VE_ID),
      vessel_ids =
        ifelse (
          n_distinct(VE_ID) < 3,
          paste(unique(VE_ID), collapse = ";"),
          NA_character_
        )
      ) %>%  relocate( n_vessels,vessel_ids, .before = Csquare)%>%
      mutate (AverageGearWidth = NA  )%>% ## If this information is available modify this line of the script. By default is assumed not existing gear width information
      as.data.frame()

colnames(table1Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
    "C-square","MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6",  "VesselLengthRange",
    "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
    "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth"
  )

table2Save <-
  table2 %>%separate(col = LE_MET ,   c("met4", "met5", "mesh" ), sep = '_', remove = FALSE)%>%separate(mesh , c("min", "max"))%>%
  group_by(
    RT, VE_COU, Year, Month, LE_RECT,LE_GEAR, met5, min, max, LE_MET, 
    LENGTHCAT, tripInTacsat
  ) %>%  
  summarise(
    sum_intv = sum(INTV, na.rm = TRUE),
    sum_kwDays = sum(kwDays, na.rm = TRUE),
    sum_le_kg_tot = sum(LE_KG_TOT, na.rm = TRUE),
    sum_le_euro_tot = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids =
      ifelse (
        n_distinct(VE_ID) < 3,
        paste(
          unique(VE_ID), collapse = ";"),
          NA_character_
        )
  ) %>%  relocate( n_vessels,vessel_ids, .before = LE_RECT)%>%
as.data.frame()

colnames(table2Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID", "ICESrectangle",
    "MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6", "VesselLengthRange", "VMSEnabled", "FishingDays",
    "kWFishingDays", "TotWeight", "TotValue" 
  )

## Save the final table 1 and table 2 . Headers and quotes have been removed to be compatible with required submission format.  
write.table(table1Save, file.path(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)
write.table(table2Save, file.path(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)
