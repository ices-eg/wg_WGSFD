
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
                         FAO_species = eflalo$LE_Species,
                         metier_level_6 = NA,
                         measure =NA,
                         KG = eflalo$LE_KG_TOT,
                         EUR = eflalo$LE_EUR_TOT)

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

### Get FAO area code
input.data <- input.data %>%
  ExtractFaoCode()

#### Merge by area
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")

# Assign gear group and re-coded gear name to the input data
input.data<-merge(input.data, gear.list, all.x = T, by.x = "gear", by.y = "gear_code")

# Process input data
#In the variable called sequence.def please include all columns that will constitute a fishing sequence
#This variable will be used as a key for grouping operations
sequence.def <- c("Country","year","vessel_id","vessel_length","trip_id","haul_id",
                  "fishing_day","area","ices_rectangle","gear_level6","mesh","selection",
                  "registered_target_assemblage")
sequence.def <- sequence.def[6] # NORWAY

# Calculate group totals for each sequence
input.data[,":="(seq_group_KG = sum(KG, na.rm = T),
                 seq_group_EUR = sum(EUR, na.rm = T)),
           by=c(sequence.def,"species_group")]

# Select a measure to determine the dominant group at a sequence level. If at least one species in a sequence has "value" in a measure column then 
measure = "weight" # NORWAY

# all species in that sequence get the same measure.
input.data[,":="(seq_measure = getMeasure(measure)),
           by=sequence.def]

# Determine the dominant group for each sequence
input.data[seq_measure == "weight",":="(seq_dom_group = species_group[which.max(seq_group_KG)]),
           by=sequence.def]
input.data[seq_measure == "value",":="(seq_dom_group = species_group[which.max(seq_group_EUR)]),
           by=sequence.def]
input.data[,":="(seq_group_KG=NULL,seq_group_EUR=NULL,seq_measure=NULL)]
# Apply DWS rules
input.data[dws_group=="DWS",seq_DWS_kg:=sum(KG, na.rm = T),
           by=c(sequence.def, "dws_group")]
input.data[,seq_total_kg:=sum(KG, na.rm = T),
           by=sequence.def]
input.data[,seq_DWS_perc:=ifelse(is.na(seq_DWS_kg),0,seq_DWS_kg/seq_total_kg)*100]
input.data[,seq_DWS_perc:=max(seq_DWS_perc),by=sequence.def]
input.data[,DWS_gear_applicable:=grepl(RCG,DWS_for_RCG),by=.(RCG)]
input.data[seq_DWS_perc>8 & DWS_gear_applicable,seq_dom_group:="DWS"]
input.data[,":="(dws_group=NULL,DWS_for_RCG=NULL,seq_DWS_kg=NULL,seq_total_kg=NULL,seq_DWS_perc=NULL,
                 DWS_gear_applicable=NULL)]
# Assign metier level 6
input.data$metier_level_6<-NA
input.data$metier_level_5<-NA

### Add target species if available

depdata <- read.csv(file.choose(),                       
                    sep = ";",
                    fileEncoding="UTF-16LE")

depdata$STARTTIDSPUNKT <- as.POSIXct(depdata$STARTTIDSPUNKT,format="%d.%m.%Y %H:%M:%S")
depdata$STOPPTIDSPUNKT <- as.POSIXct(depdata$STOPPTIDSPUNKT,format="%d.%m.%Y %H:%M:%S")

# if start point is null, use stop time date and set time to midnight

idx <- which(is.na(depdata$STARTTIDSPUNKT)&!is.na(depdata$STOPPTIDSPUNKT))
depdata$STARTTIDSPUNKT[idx] <- paste(as.character(date(depdata$STOPPTIDSPUNKT[idx])),"00:00:00", sep = " ")
depdata$STARTTIDSPUNKT[idx] <- as.POSIXct(depdata$STARTTIDSPUNKT[idx],format="%d.%m.%Y %H:%M:%S")
depdata$FT_REF <- paste(depdata$RC,year(depdata$STARTTIDSPUNKT), month(depdata$STARTTIDSPUNKT),
                     day(depdata$STARTTIDSPUNKT), hour(depdata$STARTTIDSPUNKT),
                     minute(depdata$STARTTIDSPUNKT),as.character(depdata$VARIGHET),
                     sep = "-")

input.data <- input.data %>%
  mutate(target_species = depdata$AVGANG_INTENDERT_ART_FAO[match(as.character(eflalo$FT_REF),as.character(depdata$FT_REF))])%>%
  mutate(registered_target_assemblage=species.list$species_group[match(target_species,species.list$FAO_species)])

input.data[,c("metier_level_6","metier_level_5"):=pmap_dfr(list(RCG,
                                                                year,
                                                                gear_level6, 
                                                                registered_target_assemblage,
                                                                seq_dom_group, 
                                                                mesh, 
                                                                selection_type,
                                                                selection_mesh), getMetier)]

# Missing metier. Step 1: Search levels based on a dominant group of species
input.data[,":="(month=month(dmy(fishing_day)),
                 quarter=quarter(dmy(fishing_day)))]
step.levels<-list(c("vessel_id","month","area","seq_dom_group","gear_group"),
                  c("vessel_id","month","area","seq_dom_group"),
                  c("vessel_id","quarter","area","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","area","seq_dom_group"),
                  c("vessel_id","year","area","seq_dom_group","gear_group"),
                  c("vessel_id","year","area","seq_dom_group"),
                  c("vessel_id","month","seq_dom_group","gear_group"),
                  c("vessel_id","month","seq_dom_group"),
                  c("vessel_id","quarter","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","seq_dom_group"),
                  c("vessel_id","year","seq_dom_group","gear_group"),
                  c("vessel_id","year","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 2: Search levels based on gear/gear group
step.levels<-list(c("vessel_id","month","area","gear_level6"),
                  c("vessel_id","quarter","area","gear_level6"),
                  c("vessel_id","year","area","gear_level6"),
                  c("vessel_id","month","gear_level6"),
                  c("vessel_id","quarter","gear_level6"),
                  c("vessel_id","year","gear_level6"),
                  c("vessel_id","month","area","gear_group"),
                  c("vessel_id","quarter","area","gear_group"),
                  c("vessel_id","year","area","gear_group"),
                  c("vessel_id","month","gear_group"),
                  c("vessel_id","quarter","gear_group"),
                  c("vessel_id","year","gear_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 3: Search levels based on fleet register gear, vessel length group
# and species group
input.data[,vessel_length_group:=cut(vessel_length,breaks=c(0,10,12,18,24,40,Inf),right=F)]
step.levels<-list(c("month","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("month","gear_FR","area","seq_dom_group"),
                  c("quarter","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("quarter","gear_FR","area","seq_dom_group"),
                  c("year","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("year","gear_FR","area","seq_dom_group"),
                  c("month","vessel_length_group","gear_FR","seq_dom_group"),
                  c("month","gear_FR","seq_dom_group"),
                  c("quarter","vessel_length_group","gear_FR","seq_dom_group"),
                  c("quarter","gear_FR","seq_dom_group"),
                  c("year","vessel_length_group","gear_FR","seq_dom_group"),
                  c("year","gear_FR","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 4: Search levels based on fleet register gear, vessel length group
step.levels<-list(c("month","vessel_length_group","gear_FR","area","gear_level6"),
                  c("month","gear_FR","area","gear_level6"),
                  c("quarter","vessel_length_group","gear_FR","area","gear_level6"),
                  c("quarter","gear_FR","area","gear_level6"),
                  c("year","vessel_length_group","gear_FR","area","gear_level6"),
                  c("year","gear_FR","area","gear_level6"),
                  c("month","vessel_length_group","gear_FR","gear_level6"),
                  c("month","gear_FR","gear_level6"),
                  c("quarter","vessel_length_group","gear_FR","gear_level6"),
                  c("quarter","gear_FR","gear_level6"),
                  c("year","vessel_length_group","gear_FR","gear_level6"),
                  c("year","gear_FR","gear_level6"),
                  c("month","vessel_length_group","gear_FR","area","gear_group"),
                  c("month","gear_FR","area","gear_group"),
                  c("quarter","vessel_length_group","gear_FR","area","gear_group"),
                  c("quarter","gear_FR","area","gear_group"),
                  c("year","vessel_length_group","gear_FR","area","gear_group"),
                  c("year","gear_FR","area","gear_group"),
                  c("month","vessel_length_group","gear_FR","gear_group"),
                  c("month","gear_FR","gear_group"),
                  c("quarter","vessel_length_group","gear_FR","gear_group"),
                  c("quarter","gear_FR","gear_group"),
                  c("year","vessel_length_group","gear_FR","gear_group"),
                  c("year","gear_FR","gear_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}

# Analyze vessel patterns
# Specify the percentage threshold of the number of sequences below which 
# a metier will be considered rare
rare.threshold <- 15
# Version 1 of the vessel pattern algorithm
# input.data <- vesselPatterns(input.data,sequence.def,rare.threshold,gear.list)
# Version 2 of the vessel pattern algorithm
input.data<-rareMetiersLvl5(input.data,sequence.def,rare.threshold)
# Vessel patterns. Step 1.
step.levels<-list(c("vessel_id","month","area","seq_dom_group","gear_group"),
                  c("vessel_id","month","area","seq_dom_group"),
                  c("vessel_id","quarter","area","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","area","seq_dom_group"),
                  c("vessel_id","year","area","seq_dom_group","gear_group"),
                  c("vessel_id","year","area","seq_dom_group"),
                  c("vessel_id","month","seq_dom_group","gear_group"),
                  c("vessel_id","month","seq_dom_group"),
                  c("vessel_id","quarter","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","seq_dom_group"),
                  c("vessel_id","year","seq_dom_group","gear_group"),
                  c("vessel_id","year","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)])>0){
    input.data <- vesselPatternsByLevel(input.data,level,sequence.def)
  } else {break}
}
# Vessel patterns. Step 2.
step.levels<-list(c("vessel_id","month","area","gear_level6"),
                  c("vessel_id","quarter","area","gear_level6"),
                  c("vessel_id","year","area","gear_level6"),
                  c("vessel_id","month","gear_level6"),
                  c("vessel_id","quarter","gear_level6"),
                  c("vessel_id","year","gear_level6"),
                  c("vessel_id","month","area","gear_group"),
                  c("vessel_id","quarter","area","gear_group"),
                  c("vessel_id","year","area","gear_group"),
                  c("vessel_id","month","gear_group"),
                  c("vessel_id","quarter","gear_group"),
                  c("vessel_id","year","gear_group"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)])>0){
    input.data <- vesselPatternsByLevel(input.data,level,sequence.def)
  } else {break}
}

# Metier level 6 assignment to metier level 5 which was assigned from pattern.
input.data[,metier_level_6_pattern:=NA]
step.levels<-list(c("vessel_id","month","area","metier_level_5"),
                  c("vessel_id","quarter","area","metier_level_5"),
                  c("vessel_id","year","area","metier_level_5"),
                  c("vessel_id","month","metier_level_5"),
                  c("vessel_id","quarter","metier_level_5"),
                  c("vessel_id","year","metier_level_5"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & 
                     !is.na(metier_level_5_pattern) &
                     is.na(metier_level_6_pattern)])>0){
    input.data <- metiersLvl6ForLvl5pattern(input.data,level,sequence.def)
  } else {break}
}

# Create new metier columns where rare metiers are replaced with the ones found in the pattern.
input.data[,":="(metier_level_5_new=ifelse(is.na(metier_level_5_pattern),
                                           metier_level_5,
                                           metier_level_5_pattern),
                 metier_level_6_new=ifelse(is.na(metier_level_6_pattern),
                                           metier_level_6,
                                           metier_level_6_pattern))]

# Detailed metier level 6 assignment to general >0_0_0 cases.
input.data[,detailed_metier_level_6:=ifelse(grepl("_>0_0_0",metier_level_6_new),NA,metier_level_6_new)]
step.levels<-list(c("vessel_id","month","area","metier_level_5_new"),
                  c("vessel_id","quarter","area","metier_level_5_new"),
                  c("vessel_id","year","area","metier_level_5_new"),
                  c("vessel_id","month","metier_level_5_new"),
                  c("vessel_id","quarter","metier_level_5_new"),
                  c("vessel_id","year","metier_level_5_new"))
for(level in step.levels){
  if(nrow(input.data[is.na(detailed_metier_level_6)])>0){
    input.data <- detailedMetiersLvl6ForLvl5(input.data,level,sequence.def)
  } else {break}
}

# Save results
print("Saving results ...")
result<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
                   .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,gear_FR,mesh,selection,FAO_species,
                     registered_target_assemblage,KG,EUR,metier_level_6,mis_met_level,mis_met_number_of_seq,
                     metier_level_5,metier_level_5_status,
                     metier_level_5_pattern,ves_pat_level,ves_pat_number_of_seq,
                     metier_level_6_pattern,ves_pat_met6_level,ves_pat_met6_number_of_seq,
                     metier_level_5_new,metier_level_6_new,
                     detailed_metier_level_6,det_met6_level,det_met6_number_of_seq)]
write.csv(result,"metier_results.csv", na = "")
write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
                                                          KG_sum=sum(KG, na.rm=T),
                                                          EUR_sum=sum(EUR, na.rm=T)),
                                                       by=.(Country, RCG, metier_level_6_new)][order(Country, RCG, metier_level_6_new)])


