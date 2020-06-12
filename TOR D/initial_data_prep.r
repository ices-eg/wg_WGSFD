#read and prep data
setwd("C:\\Users\\ik01\\OneDrive - CEFAS\\IK\\wgsfd")
df <- read.csv("data_modelling.csv")
head(df)

#delete NAs for spatial explanatory variables; 
explvarssp <- c("distance_coast_avg", "bpi5","bpi10","bpi30","bpi50","bpi75","mud_percent","sand_percent","gravel_percent","total_d50" ,"tidalvelmean","sea_bottom_temp")

df1 <- df[complete.cases(df[ ,explvarssp]),]

#double check NAs for lat and long and delete cases where NAs in lat long and c-square
df2 <- df1[complete.cases(df1[ ,c("c_square", "lat", "lon" )]),]
summary(df2)

#if there are any c-squares that never get fished by any metier, change those NAs with something like 'no fishing'
df2$metier_benth <- as.character(df2$metier_benth)
df2$metier_benth[is.na(df2$metier_benth)] <- "no_fishing"
df2$metier_benth <- as.factor(df2$metier_benth)

#### 0 fishing derived from c-squares where we have env variables, these could have fishing > 0 from other metiers or not, and fishing from the selected metier is 0 or NA ####
#add quarter if it does not exist
if ( !("quarter" %in% colnames(df2))) df2$quarter <- as.factor(lubridate::quarter(zoo::as.yearmon(paste(df2$month, df2$year), "%m %Y")))

#some variables need to be factors
for (i in c("c_square","year","month", "metier_benth")) df2[,paste0(i,"f")] <- as.factor(df2[,i])
summary(df2)

#set 0 fishing effort
#function
set0s <- function(Metier = "TBB_DMF", timestep = "quarter"){
  df2$YTS <- as.factor(paste(df2[,timestep],df2$year))
  df3<-list()
  for (i in levels(df2$YTS)){
    #doing it in a loop in case we need to add any sampling if too many 0s and 0 inflated distributions struggle
    df3[[i]] <- df2[df2$YTS == i,]
    ind <- which(df3[[i]]$metier_benthf != Metier)
    df3[[i]]$kw_fishinghours[ind]<- 0
    df3[[i]]$fishing_hours[ind]<- 0
  }
  df4 <- do.call("rbind", df3)
  return(df4)
}

#example run
dftbbdfm <- set0s(Metier = "TBB_DMF", timestep = "quarter")
summary(dftbbdfm)

