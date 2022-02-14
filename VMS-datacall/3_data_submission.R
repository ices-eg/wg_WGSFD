

# 3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK) --------------------------------------------

load(file = paste0(outPath, "table1.RData")  )
load( file = paste0(outPath, "table2.RData")  )


# 3.2 Replace vessel id by an anonymized id column  --------------------------------------------


# New field added for the 2020 datacall including unique vessels id's  #
# This vessel id is used to calculate unique vessels in a c-square and  #
VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!


# join onto data tables
table1 <- left_join(table1, VE_lut)
table2 <- left_join(table2, VE_lut)




# 3.3 Assign the vessel length category based in DATSU vocabulary   ----------------------



#  Use of the "icesVocab" ICES developed R package that fetch the DATSU vocabulary values for a given vocabulary theme #
# install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")

library(icesVocab)

# Get the values accepted in this vocabulary dataset

vlen_ices <- getCodeList("VesselLengthClass") ### Get DATSU Vocabulary list for selected dataset


# Filter values that aren't deprecated, overlapped  or not accepted by data call requirements

vlen_icesc <-  vlen_ices%>%slice(2, 4, 6, 7, 8, 10, 11, 12, 13 )%>%select(Key)

# TABLE 1. Add the vessel length category using  LENGTHCAT field



table1$LENGTHCAT <-  table1$VE_LEN%>%cut(    breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                             right = FALSE    ,include.lowest = TRUE,
                                             labels =  vlen_icesc$Key 
)


# TABLE 2. Add the vessel length category using  LENGTHCAT field

table2$LENGTHCAT <-  table2$VE_LEN%>%cut(   breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                            right = FALSE    ,include.lowest = TRUE,
                                            labels =  vlen_icesc$Key 
)


# 3.4 Aggregate and summarise TABLE 1 and TABLE2   ----------------------

# Table 1

table1Save <-
  table1 %>%
  separate(col = LE_MET ,   c("met4", "met5" ), sep = '_', remove = FALSE)%>%
    group_by(RT,VE_COU,Year,Month,Csquare,LE_GEAR, met5,  LE_MET,LENGTHCAT) %>%
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
          'no_required'
        )
      ) %>%  relocate( n_vessels,vessel_ids, .before = Csquare)%>%
      mutate (AverageGearWidth = NA%>%as.numeric()  )%>% ## If this information is available modify this line of the script. By default is assumed not existing gear width information
      as.data.frame()

colnames(table1Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
    "C-square","MetierL4", "MetierL5",  "MetierL6",  "VesselLengthRange",
    "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
    "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth"
  )




# Table 2

table2Save <-
  table2 %>%
  separate(col = LE_MET ,   c("met4", "met5"  ), sep = '_', remove = FALSE)%>%
  group_by(
    RT, VE_COU, Year, Month, LE_RECT,LE_GEAR, met5,  LE_MET,
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
          ''
        )
  ) %>%  relocate( n_vessels,vessel_ids, .before = LE_RECT)%>%
as.data.frame()

colnames(table2Save) <-
  c(
    "RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID", "ICESrectangle",
    "MetierL4", "MetierL5",   "MetierL6", "VesselLengthRange", "VMSEnabled", "FishingDays",
    "kWFishingDays", "TotWeight", "TotValue"
  )





# 3.5   ICES DATSU VOCABULARY CHECKS BEFORE DATA SUBMISSION  ------------------------------------------



##Get vocabulary for mandatory and fields with associated vocabulary using the DATSU API
# install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")
library(icesVocab)


# TABLE 1 =============================================================


### 3.5.1 Check if C-Squares are within ICES Ecoregions =====================

  csquares_d      <-  table1Save%>%
                      select('C-square')%>%
                      distinct( )

  csquares_dcoord <-  cbind ( csquares_d ,  CSquare2LonLat (csqr = csquares_d$`C-square` ,degrees =  0.05)   )
  valid_csquare   <-  csquares_dcoord%>%
                      filter(SI_LATI >= 30 & SI_LATI <= 90  )%>%
                      select('C-square')%>%
                      pull()



  table1Save      <-  table1Save%>%filter(`C-square` %in% valid_csquare)


### 3.5.2 Check Vessel Lengths categories are accepted ==================================


  vlen_ices       <-  getCodeList("VesselLengthClass")
  table ( table1Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table1Save [ !table1Save$VesselLengthRange %in%vlen_ices$Key,]%>%group_by(VesselLengthRange)%>%select(VesselLengthRange)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table1Save      <-  table1Save%>%filter(VesselLengthRange %in% vlen_ices$Key)

### 3.5.3 Check Metier L4 (Gear) categories are accepted =================================

  m4_ices         <-  getCodeList("GearTypeL4")
  table (table1Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table1Save [ !table1Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%select(MetierL4)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table1Save      <-  table1Save%>%filter(MetierL4 %in% m4_ices$Key)


### 3.5.4 Check Metier L5 (Target Assemblage) categories are accepted =====================

  m5_ices         <-  getCodeList("TargetAssemblage")

  table (table1Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table1Save [ !table1Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%select(MetierL5)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table1Save      <-  table1Save%>%filter(MetierL5 %in% m5_ices$Key)



# TABLE 2  =============================================================


### 3.5.5 Check ICES rect are valid  =====================

  statrect_ices <- getCodeList("StatRec")

  table (table2Save$ICESrectangle %in%statrect_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table2Save [ !table2Save$ICESrectangle %in%statrect_ices$Key,]%>%group_by(ICESrectangle)%>%select(ICESrectangle)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table2Save      <-  table2Save%>%filter(ICESrectangle %in% statrect_ices$Key)



### 3.5.6 Check Vessel Lengths categories are accepted ==================================


  vlen_ices       <-  getCodeList("VesselLengthClass")
  table ( table2Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table2Save [ !table2Save$VesselLengthRange %in%vlen_ices$Key,]%>%group_by(VesselLengthRange)%>%select(VesselLengthRange)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table2Save      <-  table2Save%>%filter(VesselLengthRange %in% vlen_ices$Key)


### 3.5.7 Check Metier L4 (Gear) categories are accepted =================================

  m4_ices         <-  getCodeList("GearTypeL4")
  table (table2Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table2Save [ !table2Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%select(MetierL4)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table2Save      <-  table2Save%>%filter(MetierL4 %in% m4_ices$Key)


### 3.5.8 Check Metier L5 (Target Assemblage) categories are accepted =====================

  m5_ices         <-  getCodeList("TargetAssemblage")

  table (table2Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table2Save [ !table2Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%select(MetierL5)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table2Save      <-  table2Save%>%filter(MetierL5 %in% m5_ices$Key)


### 3.5.9 Check Metier L5 (Target Assemblage) categories are accepted =====================


  yn <- getCodeList("YesNoFields")

  table (table2Save$VMSEnabled %in%yn$Key )   # TRUE records accepted in DATSU, FALSE aren't

  # Get summary  of   DATSU valid/not valid records
  table2Save [ !table2Save$VMSEnabled %in%yn$Key,]%>%group_by(VMSEnabled)%>%select(VMSEnabled)%>%tally()

  # Correct them if any not valid and filter only valid ones
  table2Save      <-  table2Save%>%filter(VMSEnabled %in% yn$Key)


# DATSU Vocabulary check finish




# 3.6 DATA QC REPORT (OPTIONAL)   ------------------------------------------



# Null values are only accepted for NON MANDATORY fields

# TABLE 1 =============================================================

# Create the table to check fields formats and number of NA's

table_nas <- NULL
for ( nn in colnames(table1Save)) {
  table_na <- table(table1Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table1Save[, nn]%>%length(), field_type =class(  table1Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer

gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 1**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c( 'TotValue', 'AverageGearWidth')  )
  )


# TABLE 2 =============================================================

# Create the table to check fields formats and number of NA's

table_nas <- NULL
for ( nn in colnames(table2Save)) {
  table_na <- table(table2Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table2Save[, nn]%>%length(), field_type =class(  table2Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer

gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 2**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c( 'TotValue')  )
  )


# Check if TABLE 1 fishing hours > 0

table( table1$INTV > 0  )

# Check if TABLE 2 fishing days  > 0

table( table2$INTV > 0  )

# End of QC checks





# 3.7 Save the final TABLE 1 and TABLE 2 for datacall submission --------------------------------------------

## Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.

write.table(table1Save, file.path(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)
write.table(table2Save, file.path(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)




############### DATACALL SUBMISSION USING ICESVMS R PACKAGE (OPTIONAL)  ##################

# R packages required to be installed:
# install.packages(c("icesVMS", "icesConnect"), repos = "https://ices-tools-prod.r-universe.dev")  

library(icesVMS)

# Replace with your ICES user name and you will be requested with your password
icesConnect::set_username('submitter_ices_user_id') 

# icesConnect::ices_token(refresh = TRUE)
# icesConnect::decode_token()$UserEmail # Check the email associated to your ices user name is the correct one

screen_vms_file(file.path(outPath, "table1Save.csv"))  # Submit for screening Table 1
screen_vms_file(file.path(outPath, "table2Save.csv"))  # Submit for screening Table 2

 

######################################################################
