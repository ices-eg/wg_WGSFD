detailedMetiersLvl6ForLvl5 <-function(input.data,level,sequence.def){
  input.data.to.process<-input.data[is.na(detailed_metier_level_6)]
  input.data.processed<-input.data[!is.na(detailed_metier_level_6)]
  
  data.with.metiers<-input.data.processed[,.(det_met6_number_of_seq=uniqueN(.SD)),
                                          by=c(level,"detailed_metier_level_6"),
                                          .SDcols=sequence.def][,.SD[which.max(det_met6_number_of_seq)],
                                                                by=level,
                                                                .SDcols=c("detailed_metier_level_6",
                                                                          "det_met6_number_of_seq")][,det_met6_level:=paste(level,collapse = "|")]
  cols.to.remove<-c("detailed_metier_level_6","det_met6_number_of_seq","det_met6_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.to.process)){
      input.data.to.process[,c(c):=NULL]
    }
  }
  input.data.to.process<-merge(input.data.to.process,data.with.metiers,all.x=T,by=level)
  return(rbind(input.data.to.process,input.data.processed,fill=T))
}

# Function that returns a measure to be used on a sequence level to determine dominant species/group 
getMeasure<-function(p.measure){
  idx<-which(p.measure=="value")
  if(length(idx)>0) return("value")
  else return("weight")
}

# Function that finds a metier code based on the given parameters
getMetier<-function(p.rcg, p.year, p.gear, p.reg_target, p.dom_group, p.mesh, 
                    p.selection_type, p.selection_mesh){
  #Registered target assemblage has priority over the calculated dominating assemlage
  p.target <- ifelse(is.na(p.reg_target),p.dom_group,p.reg_target)
  #First step - assign metier based on rcg, gear, target assemblage, mesh size, selection dev.
  #Ignore >0 metiers, they will be included at the end when no other metier was assigned
  metier<-metier.list[!mesh %chin% c("0", ">0") & RCG==p.rcg & 
                        gear==p.gear & 
                        target==p.target & 
                        #data.table::between(p.mesh,m_size_from,m_size_to) &
                        p.mesh>=m_size_from & p.mesh<=m_size_to &
                        (p.selection_type == sd & p.selection_mesh == sd_mesh) &
                        ((is.na(Start_year) & is.na(End_year)) | 
                           (p.year<=End_year & is.na(Start_year)) | 
                           (p.year>=Start_year & is.na(End_year)) |
                           (p.year>=Start_year & p.year<=End_year)),
                      .(metier_level_6,metier_level_5)]
  #Second step - if metier is not assigned then try without the info on selection dev.
  #Still ignore >0 metiers
  if(nrow(metier)==0){
    metier<-metier.list[!mesh %chin% c("0", ">0") & RCG==p.rcg &
                          gear==p.gear &
                          target==p.target &
                          #data.table::between(p.mesh,m_size_from,m_size_to) &
                          p.mesh>=m_size_from & p.mesh<=m_size_to &
                          ((is.na(Start_year) & is.na(End_year)) |
                             (p.year<=End_year & is.na(Start_year)) |
                             (p.year>=Start_year & is.na(End_year)) |
                             (p.year>=Start_year & p.year<=End_year)),
                        .(metier_level_6,metier_level_5)]
  }
  #Third step - if metier is not assigned then try to assign a metier with >0 mesh size range
  if(nrow(metier)==0){
    metier<-metier.list[mesh %chin% c("0", ">0") & RCG==p.rcg &
                          gear==p.gear &
                          target==p.target &
                          ((is.na(Start_year) & is.na(End_year)) |
                             (p.year<=End_year & is.na(Start_year)) |
                             (p.year>=Start_year & is.na(End_year)) |
                             (p.year>=Start_year & p.year<=End_year)),
                        .(metier_level_6,metier_level_5)]
  }
  if(nrow(metier)==0){
    metier<-data.table(metier_level_6="MIS_MIS_0_0_0",metier_level_5="MIS_MIS")
  }
  return(metier[1])
}

getMetierLvl5FromPattern<-function(p.vessel_id, p.year, p.gear, p.gear_group, p.reg_target, p.dom_group){
  p.target <- ifelse(is.na(p.reg_target),p.dom_group,p.reg_target)
  
  # Search for metier lvl5 in the pattern having the same year, vessel id and target assemblage.
  metiers<-pattern[year==p.year & vessel_id == p.vessel_id & target_assemblage == p.target]
  result<-NA
  # if exactly one metier lvl5 was found then save it in the result for the sequence. 
  if(nrow(metiers)==1){
    result<-as.character(metiers[,metier_level_5])
  }
  # if more than one metier lvl5 were found then take the one with the highest 
  # number of sequences in the same gear group.
  else if(nrow(metiers)>1){
    result<-as.character(metiers[gear_group == p.gear_group, metier_level_5[which.max(seq_no_lvl5)]])
  }
  # if none of the above returned any metier lvl5 then search for a metier lvl5 
  # in the pattern having the same year, vessel id and gear group. If more than one
  # were found, take the one with the highest number of sequences
  else{
    metiers<-pattern[year==p.year & vessel_id == p.vessel_id & gear_group == p.gear_group]
    result<-as.character(metiers[, metier_level_5[which.max(seq_no_lvl5)]])
  }
  # if metier lvl 5 was found in the pattern then return it with a prefix "pattern"
  if(length(result)>0){
    return(paste("pattern",result,sep="_"))
  }
  else{
    # if metier lvl5 was not found in the pattern but gear code and target assemblage
    # have valid values then return them with a prefix "rare"
    if(p.gear %in% gear.list$gear_code & p.target %in% assemblage.list){
      return(paste("rare",p.gear,p.target,sep="_"))
    }
    # if metier lvl5 was not found in the pattern and gear code and target assemblage
    # do not have valid values then return NA
    else{
      return(as.character(NA))
    }
  }
}

getMissingMetier<-function(vessel_id,month,area,seq_dom_group,
                           quarter,year,vessel_length_group,
                           gear_FR){
  input.data.row <- data.table(vessel_id,month,area,seq_dom_group,
                               quarter,year,vessel_length_group,
                               gear_FR)
  search.levels <- list(c("vessel_id","month","area","seq_dom_group"),
                        c("vessel_id","quarter","area","seq_dom_group"),
                        c("vessel_id","year","area","seq_dom_group"),
                        c("vessel_length_group","gear_FR","month","area","seq_dom_group"),
                        c("gear_FR","month","area","seq_dom_group"))
  result<-NA
  for(l in search.levels){
    result<-merge(input.data.row, input.data.sequances, by=l)
    result<-result[,.(n=.N), by="metier_level_6"]
    result<-as.character(result[which.max(n),.(metier_level_6)][1])
    if(!is.na(result)){
      break
    }
  }
  
  if(is.na(result)){
    return("MIS_MIS_0_0_0*")
  }
  else{
    return(paste0(result,"*"))
  }
}

loadAreaList <- function(url){
  message("Loading area list ...")
  x <- data.table(read.csv(url, sep = ",", stringsAsFactors = F))
  setnames(x, old = c("Code","AreaCode"), new = c("RCG","area"))
  x <- x[,map(.SD,trimws)]
  # area 21.0.A is present twice for NA and NSEA. In the next step we merge by area so we must get rid of duplicated areas. Question is how to handle 21.0.A.
  if(any(duplicated(x$area))){
    x<-x[!duplicated(x$area)]
    print("Warning! Duplicated area codes were found and were removed.")
  }
  return(x[,.(area,RCG)])
}

loadGearList <- function(url){
  message("Loading gear list ...")
  x <- data.table(read.xlsx(url, sheet = "formatted"))
  x[,Group:=tolower(Group)]
  setnames(x, old = c("Code","Group","GEAR.Level6"), 
           new = c("gear_code", "gear_group","gear_level6"))
  if(any(duplicated(x$gear_code))){
    x<-x[!duplicated(x$gear_code)]
    print("Warning! Duplicated gear codes were found and were removed.")
  }
  
  return(x[,.(gear_code,gear_group,gear_level6,DWS_for_RCG)])
}

loadInputData <- function(fileName){
  message("Loading input data file ...")
  x <- fread(fileName, stringsAsFactors = F, na.strings = "")
  return(x)
}

loadMetierList <- function(url){
  message("Loading metier list ...")
  x <- data.table(read.csv(url, sep = ",", stringsAsFactors = F, na.strings = ""))
  setnames(x, old = c("Metier_level6","Metier_level5"), 
           new = c("metier_level_6","metier_level_5"))
  
  #Split metier by parts
  x[,c("gear","target","mesh","sd","sd_mesh") := data.table(str_split_fixed(metier_level_6,"_",5))]
  x[str_detect(mesh,"-") == T, c("m_size_from","m_size_to"):=data.table(str_split_fixed(mesh,"-",2))]
  x[,":="(m_size_from=as.integer(m_size_from),m_size_to=as.integer(m_size_to),sd=as.integer(sd),sd_mesh=as.integer(sd_mesh))]
  
  x[substr(mesh,1,2) == ">=", ":="(m_size_from=as.integer(gsub("[[:punct:]]", " ",mesh)),
                                   m_size_to=as.integer(999))]
  x[substr(mesh,1,1) == ">" & substr(mesh,2,2) != "=", ":="(m_size_from=as.integer(1)+as.integer(gsub("[[:punct:]]", " ",mesh)),
                                                            m_size_to=as.integer(999))]
  x[substr(mesh,1,1) == "<" & substr(mesh,2,2) != "=", ":="(m_size_to=as.integer(gsub("[[:punct:]]", " ",mesh))-as.integer(1),
                                                            m_size_from=as.integer(1))]
  x[mesh == "0", ":="(m_size_from=as.integer(0),
                      m_size_to=as.integer(999))]
  
  return(x[,.(RCG,metier_level_6,Start_year,End_year,metier_level_5,gear,target,
              mesh,sd,sd_mesh,m_size_from,m_size_to)])
}

loadSpeciesList <- function(url){
  message("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List", startRow = 2, check.names = T))
  x <- unique(x[,.(FAOcode, Grouping.2.1, Grouping.3.DWS.Reg..DWS.1)])
  setnames(x, old = c("FAOcode","Grouping.2.1","Grouping.3.DWS.Reg..DWS.1"), new = c("FAO_species", "species_group","dws_group"))
  if(any(duplicated(x$FAO_species))){
    x<-x[!duplicated(x$FAO_species)]
    print("Warning! Duplicated species codes were found and were removed.")
  }
  return(x[,.(FAO_species,species_group,dws_group)])
}

metiersLvl6ForLvl5pattern <-function(input.data,level,sequence.def){
  if(!"metier_level_6_pattern" %in% colnames(input.data)) input.data[,metier_level_6_pattern:=NA]
  input.data.to.process<-input.data[metier_level_5_status=="rare" & 
                                      !is.na(metier_level_5_pattern) &
                                      is.na(metier_level_6_pattern)]
  input.data.processed<-input.data[metier_level_5_status=="rare" & !is.na(metier_level_6_pattern)]
  input.data.not.considered<-input.data[metier_level_5=="MIS_MIS" | (metier_level_5_status=="rare" & 
                                                                       is.na(metier_level_5_pattern))]
  input.data<-input.data[metier_level_5_status=="common"]
  data.with.metiers<-input.data[,.(ves_pat_met6_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_6"),
                                .SDcols=sequence.def][,.SD[which.max(ves_pat_met6_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_6",
                                                                "ves_pat_met6_number_of_seq")][,ves_pat_met6_level:=paste(level,collapse = "|")]
  setnames(data.with.metiers,old="metier_level_6",new="metier_level_6_pattern")
  cols.to.remove<-c("metier_level_6_pattern","ves_pat_met6_number_of_seq","ves_pat_met6_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.to.process)){
      input.data.to.process[,c(c):=NULL]
    }
  }
  level.to.process<-replace(level,level=="metier_level_5","metier_level_5_pattern")
  input.data.to.process<-merge(input.data.to.process,data.with.metiers,all.x=T,
                               by.x=level.to.process, by.y = level)
  return(rbind(input.data, input.data.to.process,input.data.processed, 
               input.data.not.considered, fill=T))
}

missingMetiersByLevel<-function(input.data, level, sequence.def){
  input.data.metiers.missing<-input.data[substr(metier_level_6,1,3)=="MIS"]
  input.data.metiers.missing[,":="(metier_level_6_backup=metier_level_6,
                                   metier_level_5_backup=metier_level_5)]
  n.rows.initial<-nrow(input.data)
  data.with.metiers<-input.data[substr(metier_level_6,1,3)!="MIS",
                                .(mis_met_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_6","metier_level_5"),
                                .SDcols=sequence.def][,.SD[which.max(mis_met_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_6",
                                                                "metier_level_5",
                                                                "mis_met_number_of_seq")][,mis_met_level:=paste(level,collapse = "|")]
  cols.to.remove<-c("metier_level_6","metier_level_5","mis_met_number_of_seq","mis_met_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.metiers.missing)){
      input.data.metiers.missing[,c(c):=NULL]
    }
  }
  input.data.metiers.missing<-merge(input.data.metiers.missing,
                                    data.with.metiers,all.x=T,by=level)
  input.data.metiers.missing[is.na(metier_level_6),
                             ":="(metier_level_6=metier_level_6_backup, metier_level_5=metier_level_5_backup)]
  input.data.metiers.missing[,c("metier_level_6_backup","metier_level_5_backup"):=NULL]
  
  input.data<-input.data[substr(metier_level_6,1,3)!="MIS"]
  input.data<-rbind(input.data, input.data.metiers.missing, fill=T)
  if(nrow(input.data)!=n.rows.initial){ 
    stop("Processed dataset has more rows than input dataset.
         Check your input data and search levels.")
  }
  return(input.data)
}

rareMetiersLvl5 <- function(input.data,sequence.def,rare.threshold){
  pattern <- input.data[,unique(.SD),.SDcols=c(sequence.def,"metier_level_5")]
  pattern<-pattern[metier_level_5!="MIS_MIS"]
  pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
  pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
  input.data <- merge(input.data, pattern,all.x = T ,
                      by=c("year", "vessel_id", "metier_level_5"))
  input.data[,metier_level_5_status:=ifelse(seq_perc_lvl5<rare.threshold,"rare","common")]
  input.data[,":="(seq_no_lvl5=NULL, seq_perc_lvl5=NULL, 
                   metier_level_5_pattern=NA, ves_pat_number_of_seq=NA, ves_pat_level=NA)]
  return(input.data)
}

validateInputDataCodes <- function(input.data, gear.list, area.list, species.list){
  message("Validation of input data codes...")
  assemblage.list <- unique(c(species.list$species_group, species.list$dws_group))
  assemblage.list <- assemblage.list[!is.na(assemblage.list)]
  assemblage.list <- c(assemblage.list,c("MCD","FIF"))
  
  invalid.area<-setdiff(unique(input.data$area), area.list$area)
  invalid.gear<-setdiff(unique(input.data$gear), gear.list$gear_code)
  gearFR<-unique(input.data$gear_FR)
  gearFR<-gearFR[!is.na(gearFR)]
  invalid.gearFR<-setdiff(gearFR, gear.list$gear_code)
  selection<-unique(input.data$selection)
  selection<-selection[!is.na(selection)]
  invalid.selection<-selection[grep("[01234]_\\d{1,3}",selection,invert=TRUE)]
  reg.tar.assemblage<-unique(input.data$registered_target_assemblage)
  reg.tar.assemblage<-reg.tar.assemblage[!is.na(reg.tar.assemblage)]
  invalid.reg.target.assemblage<-setdiff(reg.tar.assemblage,assemblage.list)
  invalid.species<-setdiff(unique(input.data$FAO_species), species.list$FAO_species)
  invalid.measure<-setdiff(unique(input.data$measure),c("weight","value"))
  if(length(invalid.area)>0) 
    warning(paste0("Invalid area codes found: ",invalid.area))
  if(length(invalid.gear)>0) 
    warning(paste0("Invalid gear codes found: ",invalid.gear))
  if(length(invalid.gearFR)>0) 
    warning(paste0("Invalid gear_FR codes found: ",invalid.gearFR))
  if(length(invalid.selection)>0) 
    warning(paste0("Invalid selection device coding found: ",invalid.selection))
  if(length(invalid.reg.target.assemblage)>0) 
    warning(paste0("Invalid reg target assemblage codes found: ",invalid.reg.target.assemblage))
  if(length(invalid.species)>0) 
    warning(paste0("Invalid species codes found: ",invalid.species))
  if(length(invalid.measure)>0) 
    warning(paste0("Invalid measure codes found: ",invalid.measure))
  
  return(TRUE)
}

validateInputDataFormat <- function(input.data){
  message("Validation of input data format...")
  valid.colnames<-c("Country","year","vessel_id","vessel_length","trip_id",
                    "haul_id","fishing_day","area","ices_rectangle","gear",
                    "gear_FR","mesh","selection","registered_target_assemblage",
                    "FAO_species","metier_level_6","measure","KG","EUR")
  valid.coltypes<-c("character","integer","character","double","character",
                    "character","character","character","character","character",
                    "character","integer","character","character",
                    "character","character","character","double","double")
  validation<-data.table(column_name=valid.colnames,column_type=valid.coltypes)
  for(i in 1:nrow(validation)){
    column.name<-validation[i,]$column_name
    if(column.name %in% colnames(input.data)){
      if(is.factor(input.data[,unlist(.SD),.SDcols=column.name]))
        stop(paste0("Column ",column.name," is a factor."))
      column.type<-validation[i,]$column_type
      if(column.type==typeof(input.data[,unlist(.SD),.SDcols=column.name]))
        print(paste0("Column ",column.name,": validation passed."))
      else 
        warning(paste0("Column ",column.name," should be of type ",column.type,"."))
    }
    else stop(paste0("The ",validation[i,]$column_name," column is missing."))
  }
  if(ncol(input.data) > nrow(validation)) 
    warning(paste0("Input data has additional columns that are not recognized by the script."))
  return(TRUE)
}


vesselPatterns<-function(input.data,sequence.def,rare.threshold,gear.list){
  pattern <- input.data[,unique(.SD),.SDcols=c(sequence.def,"metier_level_5")]
  pattern<-pattern[metier_level_5!="MIS_MIS"]
  pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
  pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
  input.data <- merge(input.data, pattern,all.x = T ,
                      by=c("year", "vessel_id", "metier_level_5"))
  input.data[,metier_level_5_status:=ifelse(seq_perc_lvl5<rare.threshold,"rare","common")]
  input.data.rare<-input.data[seq_perc_lvl5<rare.threshold]
  input.data.no.metier<-input.data[metier_level_5=="MIS_MIS" & is.na(seq_perc_lvl5)]
  input.data<-input.data[seq_perc_lvl5>=rare.threshold]
  pattern<-pattern[seq_perc_lvl5>=rare.threshold]
  pattern[,c("gear","target_assemblage"):=data.table(str_split_fixed(metier_level_5,"_",2))]
  pattern<-merge(pattern, gear.list[,.(gear_code,gear_group)], 
                 all.x = T, by.x = "gear", by.y = "gear_code")
  
  pattern[,count_year_vessel_target:=.N,by=.(year, vessel_id, target_assemblage)]
  pattern[,rank_year_vessel_target_geargr:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, target_assemblage, gear_group)]
  pattern[,rank_year_vessel_target:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, target_assemblage)]
  pattern[,rank_year_vessel_geargr:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, gear_group)]
  setnames(pattern, old="metier_level_5", new="metier_level_5_pattern")
  
  input.data.rare.merged<-data.table()
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, pattern[count_year_vessel_target==1,
                                                    .(year,vessel_id,target_assemblage,
                                                      metier_level_5_pattern)],
                           by.x=c("year","vessel_id","seq_dom_group"),
                           by.y=c("year","vessel_id","target_assemblage"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[count_year_vessel_target>1 & rank_year_vessel_target_geargr==1,
                                   .(year,vessel_id,target_assemblage,gear_group,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","seq_dom_group","gear_group"),
                           by.y=c("year","vessel_id","target_assemblage","gear_group"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[count_year_vessel_target>1 & rank_year_vessel_target==1,
                                   .(year,vessel_id,target_assemblage,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","seq_dom_group"),
                           by.y=c("year","vessel_id","target_assemblage"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[rank_year_vessel_geargr==1,
                                   .(year,vessel_id,gear_group,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","gear_group"),
                           by.y=c("year","vessel_id","gear_group"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  return(rbind(input.data,input.data.no.metier,input.data.rare,input.data.rare.merged,fill=T))
}

vesselPatternsByLevel <- function(input.data,level,sequence.def){
  input.data.rare.to.process<-input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)]
  input.data.rare.processed<-input.data[metier_level_5_status=="rare" & !is.na(metier_level_5_pattern)]
  input.data.no.metier<-input.data[metier_level_5=="MIS_MIS"]
  input.data<-input.data[metier_level_5_status=="common"]
  data.with.metiers<-input.data[,.(ves_pat_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_5"),
                                .SDcols=sequence.def][,.SD[which.max(ves_pat_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_5",
                                                                "ves_pat_number_of_seq")][,ves_pat_level:=paste(level,collapse = "|")]
  setnames(data.with.metiers,old="metier_level_5",new="metier_level_5_pattern")
  cols.to.remove<-c("metier_level_5_pattern","ves_pat_number_of_seq","ves_pat_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.rare.to.process)){
      input.data.rare.to.process[,c(c):=NULL]
    }
  }
  input.data.rare.to.process<-merge(input.data.rare.to.process,data.with.metiers,all.x=T,by=level)
  return(rbind(input.data, input.data.rare.to.process,input.data.rare.processed, 
               input.data.no.metier, fill=T))
}

## Added by Genoveva Gonzalez Mirelis
# Function to spatially extract FAO area code for the centroid of each fishing operation
ExtractFaoCode <- function(input.data){
  fao <- readOGR(dsn = file.path(path2fun,"Shapefiles"), layer = "FAO_AREAS_SUBDIVISION")
  proj4string(fao)=CRS("+init=epsg:4326")
  x <- over(eflalosp,fao)
  input.data <- input.data %>% mutate(F_CODE1 = as.character(x$F_SUBDIVIS))
  fao <- readOGR(dsn = file.path(path2fun,"Shapefiles"), layer = "FAO_AREAS_DIVISION")
  proj4string(fao)=CRS("+init=epsg:4326")
  x <- over(eflalosp,fao)
  input.data <- input.data %>% mutate(F_CODE2 = as.character(x$F_DIVISION))
  input.data <- input.data %>%
    transform(area = ifelse((is.na(F_CODE1) & !is.na(F_CODE2)), F_CODE2, F_CODE1))
  #data.frame()%>%
  #  select(-c(F_CODE1,F_CODE2))
  input.data <- input.data[, grep("F_CODE", colnames(input.data)):=NULL]
}
  

