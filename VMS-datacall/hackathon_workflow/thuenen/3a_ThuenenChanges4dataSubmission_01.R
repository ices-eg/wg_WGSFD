# correction of German data to fit the test of proposed workflow

# to run after reading table1 and table2 in code 3_data_submission



# 3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK) --------------------------------------------

load(file = paste0(outPath, "table1.RData")  )
load(file = paste0(outPath, "table2.RData")  )


# sort(unique(table1$LE_GEAR))
# sort(unique(table1$LE_MET))


# change TBC to TBB
table1$LE_GEAR[table1$LE_GEAR=="TBC"]<-"TBB"  
table2$LE_GEAR[table2$LE_GEAR=="TBC"]<-"TBB"  



# change PUL & PUK to TBB
  # LE_GEAR  ; until 2019 PUL only in log_eflalo (from MIS and lincence-list)
  # ? LE_MET ; maybe in LE_GEAR only, maybe also in LE_MET in 2020 (in 2020 PUL and PUK included in BLE logbook)
   ##  CHECK in which years is PUL/PUK in BLE-Logbook


table1$LE_GEAR[table1$LE_GEAR=="PUL"]<-"TBB"  
table2$LE_GEAR[table2$LE_GEAR=="PUL"]<-"TBB" 

table1$LE_GEAR[table1$LE_GEAR=="PUK"]<-"TBB"  
table2$LE_GEAR[table2$LE_GEAR=="PUK"]<-"TBB" 




# change DEU to DE  in VE_COUNTRY
table1$VE_COU<-"DE"
table2$VE_COU<-"DE"


