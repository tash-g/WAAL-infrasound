
## ---------------------------
##
## Script name: PROCESS_GPS_DATA
##
## Purpose of script: This script defines decision points, splits possible trajectories
## into 'segments', and matches these to modelled microbarom infrasound
##
## Author: Dr. Lucia Martina Martin Lopez (with editing by Dr Natasha Gillies)
##
## Date Created: 2021-03-16
##
## Email: gilliesne@gmail.com
##
## ---------------------------


# 0.0 Load the packages ---------------------------------------------------

# Packages
packages <- c()

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


# 0.1 Load the data -----------------------------------------------------

gps_2013 <- read.csv("Data_inputs/WAAL_GPS_2013.csv", stringsAsFactors = F)


# 1.0 Create travel bouts -----------------------------------------------

### NOTE: CHECK THIS SECTION

### Travelling bouts defined as periods of commuting flight lasting >20km

for (i in 1:(length(unique(gps_2013$BirdId)))) {
  
  GPS <- subset(gps_2013, BirdId == test[i], )
  GPS$idx <- seq(1:nrow(GPS))
  
  ## 1.1 Filter fixes labelled as 'travel'
  gps_2013Trav <- GPS %>%
    filter(State == "Travel")
  
  ## 1.2 Give bouts of travel a unique ID
  idxdiff <- diff(gps_2013Trav$idx)
  a <- c(which(diff(gps_2013Trav$idx) > 1), nrow(gps_2013Trav))
  travbout = c()
  for (j in 1:length(a)) {
    if (j == 1) {
      travbout <-
        c(rep(j, a[j]))
    } else{
      travbout <- c(travbout, rep(j, a[j] - a[j - 1]))
    }
  }
  gps_2013Trav$travbout <- travbout
  gps_2013Trav$travbout <-
    as.factor(as.character(gps_2013Trav$travbout))
  
  ## 1.3 Estimate the distance travelled in each bout
  Trav20 <- gps_2013Trav %>%
    group_by(travbout) %>%
    mutate(TotdisttravBout = sum(DistTrav)) %>%
    group_by(travbout) %>% filter(TotdisttravBout > 20)
  
  Boutn <- length(unique(Trav20$travbout))
  Bouttot <- length(levels(Trav20$travbout))
  
  c <- data.frame(BirdId = test[i],
                  Boutn = Boutn,
                  Bouttot = Bouttot)
  
  ## 1.4 Export the travel bouts to a new dataframe 
  if (exists ("newdata")) {
    newdata <- rbind(newdata, Trav20)
    fulldata <- rbind(fulldata, gps_2013Trav)
    Boutanal <- rbind(Boutanal, c)
    
  } else {
    newdata <- Trav20
    fulldata <- gps_2013Trav
    Boutanal <- c
  }
}

gps_2013Trav20 <- newdata




## 3.2 Identify the first point of each traveling period and the associated ####
## soundscape map.
library(ncdf4) # to get what's needed from ncdf files
library(lubridate)# working with dates and times
library(birk)# to find closest 
library(dplyr)
library(ggplot2)
gps_2013Trav20 <- read.csv('data/gps_2013Trav20.csv', stringsAsFactors=F)
gps_2013Trav20_1stpoint<-gps_2013Trav20%>%group_by(BirdId, travbout)%>%
  slice_head( n=1)%>%ungroup()


## 3.3 and 3.4 ####
#Soundscapes from Ollie can be found in https://drive.google.com/drive/folders/1t1vWtpbgFv_kNWn7fXSzBjGQeicu4Jdq?usp=sharing

path_to_IS_maps <- "D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/SPL_maps_2013/"
IS_folder_maps <- dir(path_to_IS_maps, pattern = "2013")
datalist = list()
#IS_folder_maps<-c("201301_1")
### These names are the same as TripID but with _ rather than . 
####Important. My computer was crashing when I attempted to run the loop for 
### the length of the IS_folder_maps. Thus, I had to run this loop twice, one for 
### x in 1:39 and the other one for x in 40:length(IS_folder_maps

for (x in 1:39){
  # for (x in 40:length(IS_folder_maps)){
    
  
  birdmapid<-IS_folder_maps[x]
  path_to_IS_files <- paste0(path_to_IS_maps, birdmapid, "/")
  
  
  # Load IS files 
  IS_files<-list.files(path= path_to_IS_files)
  
if (nchar (birdmapid)==8){
  IDbirdmap<-paste0(substr(IS_folder_maps[x], 1, 6),".",substr(IS_folder_maps[x], 8,8))
}else{
  IDbirdmap<-paste0(substr(IS_folder_maps[x], 1, 7),".",substr(IS_folder_maps[x], 9,10))
}
  
  ###change name of TripID 201377.3 to 201377.1
  if (IDbirdmap=="201377.1"){
    gps_2013Trav20_1stpoint$TripID<-as.factor(gps_2013Trav20_1stpoint$TripID)
    levels(gps_2013Trav20_1stpoint$TripID)[levels(gps_2013Trav20_1stpoint$TripID)=="201377.3"] <- "201377.1"
  }
  ##select the bird trip for wich the soundmaps are being open. 
  gps_2013_ID1<-gps_2013Trav20_1stpoint%>% 
    filter(TripID == IDbirdmap)
  
  gps_2013_ID1$TripID <- as.factor(as.character(gps_2013_ID1$TripID))
  gps_2013_ID1$DateTime <- as.POSIXct(gps_2013_ID1$DateTime, tz = "UTC")# which is the same as UTC. 
  
  
  ### add maptime to GPS database that will be associated with that particular GPS
  ### the mpatime should be within 30 minutes from the GPS datetime. 
  
  ## Variables: 
  ## ID (1 for focal cone, 0 for non focal cone) 
  ## Cone number (0 for focal, 1 to 11 for non focal)
  ## Absolute total spl in each cone
  ## Standarized SPL in each cone
  ## Absolute gdist to the 45dB contour 
  ## Standarized gdist to the 45dB contour 
  gps_2013_ID1$maptobe<-format(round(gps_2013_ID1$DateTime, units="hours"), format="%Y-%m-%d %H:%M")   
  
  

  
  for (i in 1:nrow(gps_2013_ID1)){
    
    
    #for (i in 2:4){
      
    ##find the maptobe for each GPS point in the IS_files
    ## if not map is found then fill it with NAs. 
    TG<-gps_2013_ID1$maptobe[i]
    Maptobe<-paste0(sub("\\_Soundscape_.*", "",IS_files[i]),"_Soundscape_",substr(TG, 1, 4),substr(TG, 6, 7),substr(TG, 9, 10),"_",substr(TG, 12, 13),".nc")
    IDX<-which(IS_files==Maptobe)
    
    if (length(IDX)==0){
      
      Cones=data.frame(cone_ID=as.numeric(NA),
                       cone_n=as.numeric(NA), 
                       cone_vert_lef=as.numeric(NA),
                       cone_vert_rig=as.numeric(NA),
                       meanGdist_45dB=as.numeric(NA),
                       maxGdist=as.numeric(NA),
                       abs_SPL_2000dB=as.numeric(NA),
                       abs_SPL_2000=as.numeric(NA), 
                       abs_SPL_2000dB_std=as.numeric(NA),
                       birdID=as.character(birdmapid),
                       TripID=as.character(NA),
                       mapID=as.character(TG),
                       counter=as.numeric(NA),
                       Sex=as.character(gps_2013_ID1$Sex),
                       x_lon=as.numeric(NA),
                       y_lat=as.numeric(NA),
                       State=as.character(NA),
                       Dist_cro_shelf=as.numeric(NA),
                       max_dist=as.numeric(NA),
                       per_trav_dist=as.numeric(NA),
                       per_trip_time=as.numeric(NA),                   
                       Trip_state=as.character(NA),
                       WindSp= as.numeric(NA), # wind speed
                       WindDir= as.numeric(NA), # wind direction 
                       Dev.wind=as.numeric(NA), # wind direction relative to track direction
                       Dev.wind2=as.numeric(NA), # wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
                       tw_sup_d=as.numeric(NA),# tail wind support
                       cw_sup_d=as.numeric(NA),# cross wind support
                       tw_sup_nod=as.numeric(NA),# tail  wind support with no directionality
                       cw_sup_nod=as.numeric(NA),# cross wind support with no directionality
                       stringsAsFactors=FALSE) 
      
      colnames(Cones)<-c("cone_ID", "cone_n","cone_vert_lef","cone_vert_rig",
                         "meanGdist_45dB", "maxGdist","abs_SPL_2000dB",
                         "abs_SPL_2000","abs_SPL_2000dB_std","birdID","TripID","mapID", "counter",
                         "Sex","x_lon","y_lat","State","Dist_cro_shelf",
                         "max_dist","per_trav_dist","per_trip_time","Trip_state","WindSp","WindDir",
                         "Dev.wind","Dev.wind2","tw_sup_d","cw_sup_d","tw_sup_nod","cw_sup_nod")
      

      
    }   else {
      
      ID=IS_files[IDX]
      ##extract the DateTime of SPL map 
      a<-paste0(substr(ID, 19, 22),":",substr(ID, 23, 24),":",substr(ID, 25, 26),":",substr(ID, 28, 29),":00:00")
      mapDateTime<-parse_date_time(a, 'ymd HMS')
      mapDateTime <- as.POSIXct(mapDateTime, tz = "GMT")# which is the same as UTC. 
      
      ##open nc file
      nc_file <- nc_open(paste0(path_to_IS_files, ID))
      latitude<-ncvar_get(nc_file,"lat")#deg
      longitude<-ncvar_get(nc_file,"lon")# deg
      OSWIPa<-ncvar_get(nc_file,"OSWI_interp_spatial_time")#Interpolated OSWI + TLloss + NormGrid + Time in Pa
      Pref=20e-6 
      OSWIdB<-10 * log10(OSWIPa/(Pref^2))
      latitude=as.vector(latitude)
      longitude=as.vector(longitude)
      SPL_dB=as.vector(OSWIdB)
      SPL_Pa=as.vector(OSWIPa)
      baz<-ncvar_get(nc_file,"baz")# back azimuth angle, clockwise to north
      baz<-as.vector(baz)
      Gdist<-ncvar_get(nc_file,"dist")# Geodesic distance in degrees
      X=as.data.frame(cbind(latitude,longitude,SPL_dB,SPL_Pa, Gdist,baz))
      X2<-X[complete.cases(X * 0), , drop=FALSE]
      
      
      ###Set Back zenith angles in accordance with the GPS Bearing
      
      # ### Check back zenith angle goes as follows ####
      # ##180 is North
      # X2180<-X2 %>% filter(baz==180)
      # ggplot(data=X2180,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X2180,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##-90 goes east
      # X2m90<-X2 %>% filter(round(baz)== -90)
      # ggplot(data=X2m90,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X2m90,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##+90 goes west 
      # X290<-X2 %>% filter(round(baz)== +90)
      # ggplot(data=X290,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X290,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##and 0 corresponds to south
      # X20<-X2 %>% filter((baz)== 0)
      # ggplot(data=X20,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X20,aes(x = longitude, y = latitude, color=SPL_dB))
      # 
      # ### Check that bearing in GPS data goes as follows 
      # gps_2013_ID_c_f <- read.csv('./gps_2013_ID_cleandatabase_full.csv', stringsAsFactors=F)
      # gps_2013_ID_c_f $TripID <- as.factor(as.character(gps_2013_ID_c_f $TripID))
      # gps_2013_ID_c_f $BirdId <- as.factor(as.character(gps_2013_ID_c_f $BirdId))
      # ID1<-gps_2013_ID_c_f  %>% filter(TripID=="201301.1")
      # 
      # ####GPS 90 is in the East
      # ID1_b0<-ID1%>% filter(Bearing > 80 & Bearing < 120)
      # ggplot()+
      #   geom_point(data=ID1,aes(x = x, y = y))+
      #   geom_point(data=ID1_b0,aes(x = x, y = y, color='red'))+
      #   ggtitle("Bearing >80 <120")
      # ###GPS with bearing 180 is at the south 
      # ID1_b90<-ID1%>% filter(Bearing > 150 & Bearing > -150)
      # ggplot()+
      #   geom_point(data=ID1,aes(x = x, y = y))+
      #   geom_point(data=ID1_b90,aes(x = x, y = y, color='black'))+
      #   ggtitle("Bearing >90 <180")
      # ###GPS with bearing 0 is at the Norh
      # ID1_s0<-ID1%>% filter(Bearing <20 & Bearing > -20)
      # ggplot()+
      #   geom_point(data=ID1,aes(x = x, y = y))+
      #   geom_point(data=ID1_s0,aes(x = x, y = y, color='black'))+
      #   ggtitle("Bearing <0 >-90")
      # ###GPS with bearing -90 is at the West
      # ID1_s180<-ID1%>% filter(Bearing < -80 & Bearing > -120)
      # ggplot()+
      #   geom_point(data=ID1,aes(x = x, y = y))+
      #   geom_point(data=ID1_s180,aes(x = x, y = y, color='black'))+
      #   ggtitle("Bearing <-90 >-180")
      
      
      
      ###Set Back zenith angles in accordance with the GPS Bearing####
      
      X2$baz_converted<-ifelse(X2$baz<=90 & X2$baz>=0,X2$baz-180,
                               ifelse (X2$baz>90 & X2$baz<=180,X2$baz-180,
                                       ifelse (X2$baz>=-180 & X2$baz<=-90,(X2$baz+180),
                                               ifelse (X2$baz>-90 & X2$baz<0,(X2$baz+180),NA))))
      
      # ###Check that the conversion is as it should be. ####
      # ##180 is south
      # X2180<-X2 %>% filter(baz_converted ==-180)
      # ggplot(data=X2180,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X2180,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##-90 goes west
      # X2m90<-X2 %>% filter(round(baz_converted)== -90)
      # ggplot(data=X2m90,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X2m90,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##+90 goes east
      # X290<-X2 %>% filter(round(baz_converted)== +90)
      # ggplot(data=X290,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X290,aes(x = longitude, y = latitude, color=SPL_dB))
      # ##and 0 corresponds to North
      # X20<-X2 %>% filter((baz_converted)== 0)
      # ggplot(data=X20,aes(x = longitude, y = latitude, color=SPL_dB))+
      #   geom_point(data=X20,aes(x = longitude, y = latitude, color=SPL_dB))
      
      #################
      
  ###3.3 within each soundscape map divide the area in 12 cone of 30 deg each####
      ##define parameters
      coneaperture<-30
      tansectlength<-2025
      tansectlength_f<-3000
      
      
      ##3.3.1 Get the baz angles for the 12 cones starting from the left side of 
      ## the focal cone, and then clockwise. 
      ##always the same relative angles from the ontrack one! 
      

      focal_cone_deg<-c(round(gps_2013_ID1$Bearing[i])-coneaperture/2, round(gps_2013_ID1$Bearing[i])+coneaperture/2)
      
      if (any(abs(focal_cone_deg)>180)){
        a<-which(abs(focal_cone_deg)>180)
        if (focal_cone_deg[a]<0){
          focal_cone_deg[a]<-360+focal_cone_deg[a]
        }else{
          focal_cone_deg[a]<-360-focal_cone_deg[a]
        }
      }
      
      

      
    
      if (focal_cone_deg[1]<= -150){
        cone_vert_lef<-(seq(from=focal_cone_deg[1], to= 180, by=30))
        cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
      }else{
        cone_vert_lef<-c(seq(from=focal_cone_deg[1], to= 180, by=30),rev(seq(from=focal_cone_deg[1]-30, to= -180, by=-30)))
        cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
      }
      
      
      ###add the exception to remove the cone that will be created with0
      ## cone_vert_lef=180 and cone_vert_rig=-180
      if (any(abs(diff(cone_vert_lef))==360)) {
        cone_vert_lef<-cone_vert_lef[- which(diff(cone_vert_lef)!=30)]
        cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
      }
      
      if (length(cone_vert_lef)>12) {
        cone_vert_lef<-c(cone_vert_lef[c(1:12)])
        cone_vert_rig<-c(cone_vert_rig[c(1:12)])
      }



      
      
      
      cone_ID<-c(0,1,1,1,1,1,1,1,1,1,1,1)# it it is focal or non focal
      cone_n<-seq(1,12)# cone number, being 1 the focal and from 2 to 12 the non
      ##focal ones in a clockwise direction

      Cones=as.data.frame(cbind(cone_ID,cone_n,cone_vert_lef,cone_vert_rig))
      colnames(Cones)<-c("cone_ID", "cone_n","cone_vert_lef","cone_vert_rig")

      ###For each cone estimate the abs and standarized SPL and gdist to 45dB. 
      ###need to calculate the max Gdist in each baz. So as to set the transectlength 
      ## to that. 
      
      # If Cones$gdist is smaller than 1950 that map should not be analyzed! 
      ## Open to discuss with the rest!!
      maxGdist<-aggregate(.~round(baz_converted), X2, FUN=max)
      #plot(maxGdist$Gdist,maxGdist$baz_converted )
      
      Cones$abs_SPL_2000=Cones$abs_SPL_2000dB=Cones$maxGdist=Cones$meanGdist_45dB=NA
     
      
      for (c in 1:nrow(Cones)){
        

       if(cone_vert_lef[c]>0 &cone_vert_rig[c]<0){
         X2_1<-X2%>%
           filter(X2$baz_converted>=cone_vert_lef[c]  & X2$baz_converted<=180)
         X2_2<-X2%>%
           filter(X2$baz_converted<=cone_vert_rig[c]  & X2$baz_converted>=-180)
         newX<-rbind(X2_1,X2_2)
         abs_SPL_2000<- newX%>%
           filter(Gdist<= tansectlength)%>%
           summarise(x = sum(SPL_Pa))
         Cones$abs_SPL_2000[c]<-as.numeric(abs_SPL_2000)
         
         abs_SPL_2000dB<- newX%>%
           filter(Gdist<= tansectlength)%>%
           summarise(x = 10 * log10(sum(SPL_Pa)/(Pref^2)))
         Cones$abs_SPL_2000dB[c]<-as.numeric(abs_SPL_2000dB)
         
         maxGdistbaz<- newX%>%
           filter(Gdist<= tansectlength)%>%
           group_by(round(baz_converted))%>%
           mutate(x = max(Gdist))
         distang <- data.frame(maxGdistbaz$baz, maxGdistbaz$x)
         #plot(distang)
         Cones$maxGdist[c]<-min(as.numeric(maxGdistbaz$x))
         
         
         Gdist_45dB<-newX%>%
           filter(Gdist<= tansectlength)%>%
           group_by(round(baz_converted))%>%
           filter(abs(SPL_dB -45)==min(abs(SPL_dB -45)))
         Cones$meanGdist_45dB[c] <-mean(as.numeric(Gdist_45dB$Gdist))
           

       }else{
         abs_SPL_2000<- X2%>%
           filter(X2$baz_converted>=cone_vert_lef[c]  & X2$baz_converted<=cone_vert_rig[c] & Gdist<= tansectlength)%>%
           summarise(x = sum(SPL_Pa))
         Cones$abs_SPL_2000[c]<-as.numeric(abs_SPL_2000)
         
        
         abs_SPL_2000dB<- X2%>%
           filter(X2$baz_converted>=cone_vert_lef[c]  & X2$baz_converted<=cone_vert_rig[c] & Gdist<= tansectlength)%>%
         summarise(x = 10 * log10(sum(SPL_Pa)/(Pref^2)))
         Cones$abs_SPL_2000dB[c]<-as.numeric(abs_SPL_2000dB)
         
         maxGdistbaz<- X2%>%
           filter(X2$baz_converted>=cone_vert_lef[c]  & X2$baz_converted<=cone_vert_rig[c] & Gdist<= tansectlength)%>%
           group_by(round(baz_converted))%>%
           mutate(x = max(Gdist))
         distang <- data.frame(maxGdistbaz$baz, maxGdistbaz$x)
         #plot(distang)
         Cones$maxGdist[c]<-min(as.numeric(maxGdistbaz$x))
         
         Gdist_45dB<-X2%>%
           filter(X2$baz_converted>=cone_vert_lef[c]  & X2$baz_converted<=cone_vert_rig[c] & Gdist<= tansectlength 
                  & Gdist>= 50)%>%
           group_by(round(baz_converted))%>%
           filter(abs(SPL_dB -45)==min(abs(SPL_dB -45)))
         Cones$meanGdist_45dB[c] <-mean(as.numeric(Gdist_45dB$Gdist))
         
         # ggplot(Gdist_45dB)+
         #   geom_point(aes(x=baz_converted, y=Gdist))
         # plot.name <- paste0(plot.direx, "Contour_45dB_Gdistbigger50_",i,"_201301_1.png")
         # ggsave(plot.name)
         
         
         
       }
      }
 
        
        Cones$abs_SPL_2000dB_std<-scale(Cones$abs_SPL_2000dB)
        Cones$birdID<-birdmapid
        Cones$TripID<-gps_2013_ID1$TripID[i]
        Cones$mapID<-TG 
        Cones$counter=gps_2013_ID1$counter[i]
        Cones$Sex=gps_2013_ID1$Sex[i]
        Cones$x_lon=gps_2013_ID1$x_lon[i]
        Cones$y_lat=gps_2013_ID1$y_lat[i]
        Cones$State=gps_2013_ID1$State[i]
        Cones$Dist_cro_shelf=gps_2013_ID1$Dist_cro_shelf[i]
        Cones$max_dist=gps_2013_ID1$max_dist[i]
        Cones$per_trav_dist=gps_2013_ID1$per_trav_dist[i]
        Cones$per_trip_time=gps_2013_ID1$Trip_time[i]*100
        Cones$Trip_state=gps_2013_ID1$Trip_state[i]
        Cones$WindSp=gps_2013_ID1$WindSp[i] # wind speed
        Cones$WindDir=gps_2013_ID1$WindDir[i]# wind direction 
        Cones$Dev.wind=gps_2013_ID1$Dev.wind[i]# wind direction relative to track direction
        Cones$Dev.wind2=gps_2013_ID1$Dev.wind2[i]# wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
        
        deg2rad <- function(deg) {(deg * pi) / (180)}
        Cones$tw_sup_d=gps_2013_ID1$WindSp[i]*cos(deg2rad(gps_2013_ID1$Dev.wind[i]))  # tail wind support with directionality 
        Cones$cw_sup_d=gps_2013_ID1$WindSp[i]*sin(deg2rad(gps_2013_ID1$Dev.wind[i]))  # cross wind support with directionality 
        Cones$tw_sup_nod=gps_2013_ID1$WindSp[i]*cos(deg2rad(gps_2013_ID1$Dev.wind2[i]))  # tail wind support with no directionality 
        Cones$cw_sup_nod=gps_2013_ID1$WindSp[i]*sin(deg2rad(gps_2013_ID1$Dev.wind2[i]))  # cross wind support with no directionality 
        
            ###scale standarize with mean zero and sd 1. So that all positive numbers
      ## mean that the SPL are above the mean and all negative numbers are below
      ## the mean. 


       # test<- X2%>%
       #   filter(X2$baz_converted>=cone_vert_lef[c]  & baz_converted<=cone_vert_rig[c] & Gdist<= tansectlength)
       # 
       # p1<-ggplot(NULL)+
       #   geom_point(data=X2,aes(x = longitude, y = latitude, color=SPL_dB))+
       #   geom_point(data=test,aes(x = longitude, y = latitude))
       # p1
       
    }
    # if (x==40 & i==1){GPS_ID_cones=Cones
    if (x==1 & i==1){GPS_ID_cones=Cones
    
    }else{
      GPS_ID_cones=rbind(GPS_ID_cones,Cones)
    }
      
  }
  print(x)
  nc_close( nc_file )
  }



write.csv(GPS_ID_cones, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones1_39.csv")
write.csv(GPS_ID_cones, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones40_89.csv")

GPS_ID_cones40_89 <- read.csv('data/GPS_ID_cones40_89.csv', stringsAsFactors=F)
GPS_ID_cones1_39 <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones1_39.csv', stringsAsFactors=F)

GPS_ID_conesfull=rbind(GPS_ID_cones1_39,GPS_ID_cones40_89)
write.csv(GPS_ID_conesfull, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database.csv")




################figures  exploratory analysis###################################


library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(sf)
#GPS_ID_conesfull <- read.csv('D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database.csv', stringsAsFactors=F)
GPS_ID_conesfull <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database.csv', stringsAsFactors=F)

levels(as.factor(GPS_ID_conesfull$birdID))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

GPS_ID_conesfull <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database.csv', stringsAsFactors=F)
gps_2013Trav20 <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013Trav20.csv', stringsAsFactors=F)
gps_2013Trav20_1stpoint<-gps_2013Trav20%>%group_by(BirdId, travbout)%>%
  slice_head( n=1)%>%ungroup()
####Plot GPS track with the points for which we have estimated the SPL#########
str(gps_2013Trav20_1stpoint)

ID1<-gps_2013Trav20_1stpoint%>%
  filter(TripID=="201301.1")

ID1_cones<-GPS_ID_conesfull%>%
  filter(birdID=="201301_1")
ggplot()+
  geom_point(data=ID1,aes(x=x_lon, y=y_lat))+
  geom_point(data=ID1_cones, aes(x=x_lon, y=y_lat),color="red")


###plot in black all gps points associated with travelling. 
### IN red all of those starting travelling periods with a distance larger than 20 km 
## which is the threshold below which birds may use olfactory cues. 
plot.direx <- "C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/GPS_decission_points_IS/"

for (i in unique(gps_2013Trav20$TripID)){
  
  data_cones<-GPS_ID_conesfull[GPS_ID_conesfull[,"TripID"]==i,]
  data_full<-gps_2013Trav20[gps_2013Trav20[,"TripID"]==i,]
  TripID<-data_full$TripID[1]
  if (any(!is.na(data_cones))==F){
  }else{
  p <-
    ggplot(data = world)+
    geom_sf() +
    geom_point(data=data_full, aes(x = x_lon, y = y_lat))+
    geom_point(data=data_cones, aes(x=x_lon, y=y_lat, color=as.factor(Trip_state)))+
  
    #plot crozet island. 
    geom_point(x=51.706972, y=-46.358639, color="yellow")+
    coord_sf(xlim = c(min (data_full$x_lon)-3,max (data_full$x_lon))+2, ylim =c(min (data_full$y_lat)-2,max (data_full$y_lat)+2), expand = FALSE)
  p<-p+labs(title=paste0("ID_",TripID))
  plot.name <- paste0(plot.direx, "GPStrack_analysed_decission_points_IS",TripID,".png")
  ggsave(plot.name)
  }
}





#####Violin plots with the std SPL of focal vs. non focal for all BirdID#######
## Separate them by sex
## separate them by trip State

ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,abs_SPL_2000dB_std))+
 geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_std_SPL",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,abs_SPL_2000dB_std, color=as.factor(Trip_state)))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_std_SPL_byTripState",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")


ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,abs_SPL_2000dB))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_abs_SPL",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,abs_SPL_2000dB, color=as.factor(Trip_state)))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_abs_SPL_byTripState",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")



GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=abs_SPL_2000dB_std, x=xaxis, color=as.factor(Trip_state)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_std_SPL_byTripState_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=abs_SPL_2000dB_std, x=xaxis, color=as.factor(Sex)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_std_SPL_bySex_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")


GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=abs_SPL_2000dB, x=xaxis, color=as.factor(Trip_state)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_abs_SPL_byTripState_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=abs_SPL_2000dB, x=xaxis, color=as.factor(Sex)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_abs_SPL_bySex_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")


GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=meanGdist_45dB, x=xaxis, color=as.factor(Sex)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_abs_Gdist45_bySex_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")


GPS_ID_conesfull%>%
  filter(cone_ID==1)%>%
  group_by(birdID)%>%
  mutate(xaxis=1:n())%>%
  ggplot()+
  geom_point(aes(y=meanGdist_45dB, x=xaxis, color=as.factor(Trip_state)))+
  facet_wrap(~birdID)
plot.name <- paste0(plot.direx, "IS_cone_abs_Gdist45_byTripState_perTrip",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")





ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,meanGdist_45dB))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_Gdist45",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,meanGdist_45dB, color=as.factor(Trip_state)))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_Gdist45_byTripState",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")

ggplot()+
  geom_violin(data=GPS_ID_conesfull, aes(factor(cone_n) ,meanGdist_45dB, color=as.factor(Sex)))+
  geom_jitter(height = 0, width = 0.1)
plot.name <- paste0(plot.direx, "Violin_IS_cone_Gdist45_bySex",".png")
ggsave(plot.name, width = 40, height = 20, units = "cm")



###Create a table with 
### how many female and how mnay male trips 
###       how many travel bouts are analysed per TripID,
###       percentage of GPS points analysed over the total GPS points. 
####  how many decission bouts per trip state?


library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(sf)
GPS_ID_conesfull <- read.csv('D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database.csv', stringsAsFactors=F)

GPS_ID_conesfull<-GPS_ID_conesfull%>%
  filter(! birdID=="201377_1")


females<-GPS_ID_conesfull%>%
  filter(Sex=="F")
unique(females$birdID)

females<-GPS_ID_conesfull%>%
  filter(Sex=="F")%>%
  group_by(birdID)%>%
    summarize(tot_count=sum(length(unique(counter))))
sum(females$tot_count)

males<-GPS_ID_conesfull%>%
  filter(Sex=="M")%>%
  group_by(birdID)%>%
  summarize(tot_count=sum(length(unique(counter))))
sum(males$tot_count)

in_summary<-GPS_ID_conesfull%>%
  filter(Trip_state=="in")%>%
  group_by(birdID)%>%
  summarize(tot_count=sum(length(unique(counter))))
sum(in_summary$tot_count)


test<-GPS_ID_conesfull%>%
  group_by(birdID)%>%
  summarize(tot_count=max(counter, na.rm = TRUE),
            analysed=n_distinct(counter, na.rm = TRUE))


test<-as.data.frame(test)
test$per_anal<-test$analysed*100/test$tot_count
mean(test$per_anal)#[1] 3.718353
sd(test$per_anal)# 1.078406

(sum(test$analysed)*100)/sum(test$tot_count)

sumario<-test%>%
summarize(mean=mean(per_anal),
          sd=sd(per_anal),
          max=max(per_anal),
          min=min(per_anal))

#mean       sd      max min
#1 3.718353 1.078406 5.952381   0

write.csv(test, "D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Created_databses_from_Full_script_data2013GPS_IS_modelR/statistics_analysedbouts_pertrip.csv")










