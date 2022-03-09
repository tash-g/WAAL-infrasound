############################Full analysis albatros movement in relation to IS###
#Important notes 
##TravDist and bearing are from t to t+1 as the last setplength & angle are NAs. 





##1. Prepare GPS data base##############################==########################
#select only 2013 data, only including one trip (the longest) per bird

###1.1 LOAD IN TRACKS ####
##THis contains all trips short and long from 2010-2016 provided and processed by Tommy
##This data is speed-filtered with all trips interpolated to 15 mins.
##The columns "WindSp" and "Dev.wind2" refer to wind speed and direction 
##relative to the bird bearing, respectively and "LoD" to whether day ("L") or night ("D"). 

path.cro <- "./data/Mar21_Crozet_Inc_GPS2010-16_all_RemIncSpFilLinInter_Wind_LoD_woShortTrips.csv"
cro <- data.table::fread(file = path.cro, na.strings = "NA")
cro$ID <- as.factor(as.character(cro$TripID))
nlevels(cro$ID) # 291
###variables meaning from database###############
#X4:   ??  X3 and X4 seems to be the same
#X3:   ??
#X2:   ??
#X1:   ?? X1 and X seems to be the same
#X:   ??    
#BirdID: contains the year and the bird id from 01 to 120
#TripID: contains the BirdID and the trip number, normally .1 but not all. Ask Tommy
#Ring: 
#Sex: F or M
#Year: year
#Longitude: longitude
#Latitude: latitude
#DateTime: year-month-day hour:minutes:seconds
#DistTrav: I guess is distanc etravelled betwee two consecutive points in km?
#WindU:   ??
#WindV:   ??
#WindSp: wind speed relative to the bird bearing
#WindDir:   ??
#SpeedKmH: speed of the bird in km/h
#Flying: yer or no. Yes if speed is bigger than 10 km/h. 
#Bearing: estimated as ....????
#Dev.wind
#Dev.wind2:wind direction relative to the bird bearing
#Wind_sp_f:
#LoD: to whether day ("L") or night ("D") based on ...???
#Dist_cro_shelf: distance to crozet shelf (distance from colony)
#Dist_bout
#Above_30
#Trip_bout



###1.2 Check and remove if more than one trip from the same bird, remove the shorter one. ####
### 2013 data

library(dplyr)
gps_2013<-cro%>% 
  filter(Year == 2013)
#89 IDS
#Trip ID 101

test<-gps_2013%>%group_by(BirdId)%>%
  filter(n_distinct(TripID)>1)

##plot all GPS data 
library(ggplot2)
library(plotly)

pGPS1<-ggplot(data = test, aes(x = Longitude, y =Latitude)) +  
  geom_point(aes( colour = TripID)) +
  geom_path(data = test, aes(x = Longitude, y = Latitude, shape=factor(TripID)))+
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")
gg_pGPS1 <- highlight(ggplotly(pGPS1), "plotly_selected")
gg_pGPS1

#####select by hand. 
### select 201313.3 vs 201313.2
### select either 201325.1, 201325.2 
### select 201329.2 vs 201329.1 
### select 201332.1 vs 201332.2 
### select 201333.1 vs 201333.2 
### select 201336.1 vs 201336.2 
### select 201343.2 vs 201343.1 
### select 201355.2 vs 201355.1 
### select either 201360.1, 201360.2 
### select either 201363.1, 201363.2 
### select 201377.3 vs 201377.1 & 201377.2

test2<-gps_2013%>%group_by(BirdId)%>%
  filter(n_distinct(TripID)>1)
test2<-gps_2013%>%group_by(BirdId)%>%
  filter(n_distinct(TripID)>1 )%>%group_by(TripID)%>%
  summarise(max_dist=max(Dist_col))

####confirmed that the ones to keep are the ones with bigger distance to colony. 
testea<-gps_2013%>%group_by(TripID)%>%
  summarise(max_dist=max(Dist_col))
ea=left_join(gps_2013, testea,by="TripID")

####select the shorter trips of those birds that have more than one trip. 
gps_2013_ID_short<-ea%>%group_by(BirdId)%>%
  filter((n_distinct(TripID)>1  & max_dist<max(max_dist)) )# then select those with longer range!

#12 trips and 11 birds
length(unique(gps_2013_ID_short$TripID))
length(unique(gps_2013_ID_short$BirdId))
write.csv(gps_2013_ID_short, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_short.csv")

####select the longer trips of those birds that have more than one trip. 
gps_2013_ID<-ea%>%group_by(BirdId)%>%
  filter(!(n_distinct(TripID)>1  & max_dist<max(max_dist)) )# then select those with longer range!
length(unique(gps_2013_ID$TripID))
length(unique(gps_2013_ID$BirdId))
### same length of trips and IDs!!!

##save database
write.csv(gps_2013_ID, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID.csv")





####2013Database only including the largest trip per bird. second and third trips 
##have been reomoved

cro.13 <- read.csv('data/gps_2013_ID.csv', stringsAsFactors=F)





###2. Do HMM###################################################################
##There were some problems with the residuals of the HMM and so here we are 
##trying to filter the data slightly to remove those problems. 

#We did keep only crozet island on the shape file to later remove all points at
# the start and end of each trip of less than 10 km from the Crozed island. 
# Keeping however all of those point less than 10km from Crozet at the middle of
# each trip. 
# We also removed all extra points and small trips before and after the main trip. 




###2.1 Prepare data and estimate the distance of all points to the shape file of Crozet#####
library(raster)
library(dplyr)
proj.dec <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.utm <- "+proj=laea +lat_0=-46.4 +lon_0=51.76 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
world.direx <- "C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/HMM files_GPS_2013/Re _Redo_models_to_extract_GPS2013_Crozet_data/south_country_merged.shp"
world2 <- maptools::readShapeSpatial(world.direx)
proj4string(world2) <- proj.dec
world3 <- spTransform(world2, CRS(proj.utm))

#find the poloygon for the Crozet island
x=y=c()
for (i in 1:length(world2@polygons[[1]]@Polygons)){
  x[i]= mean(world2@polygons[[1]]@Polygons[[i]]@coords[,1])#longitude
  y[i]= mean(world2@polygons[[1]]@Polygons[[i]]@coords[,2])#latitude
}

#colony<-c(51.706972,-46.358639)
Croz_idx<-which(x<51.9 & x>51.5 & y>-46.5 & y<(-46.2))
Croz<-world3@polygons[[1]]@Polygons[[Croz_idx]] #this creates a formal class polygon
#create a SpatialPolintsDataframe
Croz1<-Polygons(list(Croz), ID="island")
Croz2<-SpatialPolygons(list(Croz1),1L,proj4string = CRS(proj.utm))
df<-data.frame(value=1, row.names="island")
Croz3<-SpatialPolygonsDataFrame(Croz2, data=df)

cro.13$Longitude <- cro.13$x
cro.13$Latitude <- cro.13$y
cro.13$TripID <- as.factor(as.character(cro.13$TripID))
TripID<-nlevels(cro.13$TripID) # 89

# projecting to utm
coordinates(cro.13) <- ~ Longitude+Latitude
proj4string(cro.13) <- proj.dec
cro.13.sp <- spTransform(cro.13, CRS(proj.utm))
cro.13.df <- as.data.frame(cro.13.sp)

# this code below takes AGES!
library(rgeos)
library(gdistance)
library(sf)

cro.13.sp$Dist_world <- NA
for (i in 1:nrow(cro.13.df)) {
  cro.13.sp$Dist_world[i] <- rgeos::gDistance(Croz3, cro.13.sp[i,])
}
cro.13.sp$Dist_world <- cro.13.sp$Dist_world/1000
hist(cro.13.sp$Dist_world)
# project back
cro.13.sp2 <- spTransform(cro.13.sp, CRS(proj.dec))
cro.13.df <- as.data.frame(cro.13.sp2)
#save cro.13.df 
write.csv(cro.13.df, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/cro.13.dist_col.csv")
#database including only one trip per ID with the distance of each point to the Crozet shelf





###2.2 Remove small trips within each trip#######
### it seems that more than one tripID is formed by more trips and so I need
### to check them and to divide or filter the non needed data
library(plotly)
Trips2check<-c(201311.1,201313.3,201321.1,201323.1,201325.2,201328.1,201329.2,201331.1,
               201342.1,201370.1,201381.2,2013104.1,2013110.1,2013112.1 )

Trips2check2<-c(2013114.1, 2013116.1,201302.1,201334.1,201343.2,201358.1,201368.1)


for (i in unique(Trips2check2)){
  cro.13.df$Time_stamp <- as.POSIXct(cro.13.df$DateTime)  #This is UTC time
  data_ID<-cro.13.df[cro.13.df[,"TripID"]==i,]
  
  p <-
    ggplot(data = world)+
    geom_sf() +
    geom_point(data=data_ID, aes(x = Longitude, y = Latitude, color=DateTime))+
    #plot crozet island. 
    geom_point(x=51.706972, y=-46.358639, color="red")+
    coord_sf(xlim = c(min (cro.13.df$Longitude)-3,max (cro.13.df$Longitude))+2, ylim =c(min (cro.13.df$Latitude)-2,max (cro.13.df$Latitude)+2), expand = FALSE)
  gg_2 <- highlight(ggplotly(p), "plotly_selected")
  htmlwidgets::saveWidget(gg_2,paste0(unique(data_ID$TripID[1]),"need to divide in subtrips"))
  dev.off()
}

cro.13.dc <- read.csv('./cro.13.dist_col.csv', stringsAsFactors=F)

cro.13.dc.filt<-cro.13.dc%>% 
filter(!(TripID == 201311.1 & DateTime > "2013-02-26 16:00:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201313.3  & DateTime > "2013-02-22 22:50:00"))##!!!!

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201321.1   & DateTime > "2013-02-02 09:40:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201323.1   & DateTime > "2013-03-08 05:00:00"))##

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201325.2   & DateTime > "2013-03-26 00:50:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201328.1   & DateTime > "2013-03-18 14:00:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201331.1   & DateTime > "2013-03-20 11:50:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201342.1   & DateTime > "2013-02-28 08:20:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201370.1   & DateTime > "2013-02-12 09:50:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201370.1   & DateTime < "2013-02-01 03:00:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 2013112.1   & DateTime < "2013-02-01 16:00:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 2013116.1   & DateTime > "2013-03-30 19:10:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201334.1   & DateTime > "2013-03-20 19:00:00"))####

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201302.1   & DateTime < "2013-02-16 17:00:00"))###

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201329.2   & DateTime > "2013-03-18 08:00:00"))# this trip also has a lot of points at the begining of the trip near the colony but further than 20 km that I did not filtered! hope its not a problem for the HMM

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 2013110.1   & DateTime < "2013-02-02 14:30:00"))### this trip has a lot of points near the colony then it goes away 

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201381.2   & DateTime > "2013-02-12 08:00:00"))##

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 201381.2   & DateTime < "2013-02-05 11:30:00"))##

cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!(TripID == 2013114.1   & DateTime < "2013-02-18 07:50:00"))##

#2013104.1 has 2 trips. one before 01/02/2013 12:50  and the other after it. 
cro.13.dc.filt$TripID[which(cro.13.dc.filt$TripID== 2013104.1   & cro.13.dc.filt$DateTime > "2013-02-01 12:50:00")]=2013104.2 ##


#2013104.1  is the longest trip and the one we are going to keep. 
cro.13.dc.filt<-cro.13.dc.filt%>% 
  filter(!TripID == 2013104.2)##


write.csv(cro.13.dc.filt, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/cro.13.dist_col_clean.csv")
###This database contains the longest trip for each birID with the distance of each point 
## to the Crozet shelf and with the trips cleaned so as not to include small trips 
## near the colony. 








###2.3 remove all data that is 10km from the crozet shelf#### 
## it make sense to remove the data-points at the beginning and end of the trip 
### that are consecutive and less than 10km and like this points that are not 
## consecutive will be thought to be happening in the middle of the trip

###First select all datapoints with less than 10km X7 is the index 
cro.13.dc.filt <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/cro.13.dist_col_clean.csv', stringsAsFactors=F)


library(dplyr)
test<-cro.13.dc.filt%>%group_by(TripID)%>%
  filter(Dist_world>10)%>%
  summarise(first_more10km = first(X.8),
            last_more10km = last(X.8))

idx=c()
for (i in 1:nrow(test)){
  idx<-c(idx,test$first_more10km[i]:test$last_more10km[i])
  
}
cro.13.dc.filt.10kmbegend<-cro.13.dc.filt[idx,]

plot.direx <- "C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/HMM files_GPS_2013/Re _Redo_models_to_extract_GPS2013_Crozet_data/GPS plot data near colony filtering datapoints 10km from shore/"

cro.13.dc.filt.new<-cro.13.dc.filt.10kmbegend
colony <- data.frame(Lon = 51.76666, Lat = -46.4)

library(ggplot2)
library(rnaturalearth)
cro.13.dc.filt.new$TripID<-as.factor(cro.13.dc.filt.new$TripID)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
for (i in 1:nlevels(cro.13.dc.filt.new$TripID)){
  p1 <- ggplot() + geom_path(data = cro.13.dc.filt.new[cro.13.dc.filt.new$TripID == levels(cro.13.dc.filt.new$TripID)[i],], aes(Longitude, Latitude)) +
    geom_point(data = cro.13.dc.filt.new[cro.13.dc.filt.new$TripID== levels(cro.13.dc.filt.new$TripID)[i],], aes(Longitude, Latitude,fill = Dist_world), pch = 21,size = 1.5) +
    scale_fill_distiller(palette = "RdYlGn",  limits = c(0, 20)) +
    #geom_point(data = colony, aes(Lon, Lat), pch = 24, size = 2, fill = "yellow")+
    #theme(legend.position = "none")+
    geom_sf(data = world) +
    ggtitle(levels(cro.13.dc.filt.new$TripID)[i]) + #borders(fill = "gray") + 
    coord_sf(xlim = c(colony$Lon-2, colony$Lon+2),
             ylim =c(colony$Lat-2, colony$Lat+2))
  print(p1)
  
  plot.name <- paste0(plot.direx, levels(cro.13.dc.filt.new$TripID)[i], "clean_morethan10km.png")
  ggsave(plot.name)
  dev.off()
}




write.csv(cro.13.dc.filt.10kmbegend, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/cro.13.dist_col_10kmclean_final_to_use.csv")
##This database contains the longest trip for each birID without the short trips
## in and out of the colony at the beginning and at the end. and we have also 
## filtered those points that are within 10km from the Crozet shelf at the beginning 
## and at the end of the trip but keeps those that occur at the mid of the trip. 


###2.4  Do HMM of this final database#############
library(momentuHMM)
library(ggplot2)

### LOAD IN TRACKS 
path.cro <- "./Created_databses_from_Full_script_data2013GPS_IS_modelR/cro.13.dist_col_10kmclean_final_to_use.csv"
cro10km <- read.csv(file = path.cro, na.strings = "NA")
cro10km$ID <- as.factor(as.character(cro10km$TripID))
nlevels(cro10km$ID) # 89

cro10km<-cro10km%>%rename( st_new = step, ang_new = angle, y_lat=y, x_lon=x)
cro10km$ID <- as.factor(as.character(cro10km$TripID))
IDs<-nlevels(cro10km$ID) # 101

###   2.4.1PREPARE DATA FOR HMMS ####
dat <-prepData(cro10km, type= "LL", # longs and lats
               coordNames = c("Longitude", "Latitude")) ## these are our names


head(dat)

# assigning step lengths based on values associated with "best" models in the first code "1_run_simple_hmm_test_initial_values.r"
shape_0 <- c(12.46,3.95, 0.34) 
scale_0 <- c(3.734, 4.44, 	0.19) 

length(table(which(dat$step == 0))) # 38///0 for short trips 
nrow(dat[dat$step == 0,])/nrow(dat)*100 # 0.14//////0.2 for short trips


# zero parameter
zero_0 <- c(0,0,0) # basically saying that 4% of rest locations are zeros
stepPar0 <- c(shape_0,scale_0, zero_0)


# Also, here is some code to set zero values to really small numbers
ind_zero <- which(dat$step == 0)
if (length(ind_zero)>0){
  dat$step[ind_zero] <- runif(length(ind_zero))/10000
}
ind_zero <- which(dat$step == "NA")
if (length(ind_zero)>0){
  dat$step[ind_zero] <- runif(length(ind_zero))/10000
}

stepPar0 <- c(shape_0,scale_0)

# assigning turning angles based on first code
location_0 <- c(0.0033,  -0.016, 0.03) 
concentration_0 <- c(47.15,  1.16,  39.00)
anglePar0 <- c(location_0,concentration_0)


###   2.4.2 RUNNING HM MODEL ######
stateNames<-c("travel","search", "rest")

m1 <- fitHMM(data=dat, nbStates=3,
             dist=list(step="gamma",angle="vm"),
             Par0=list(step=stepPar0, angle=anglePar0),
             estAngleMean = list(angle=TRUE),
             stateNames = stateNames)


m1
plot(m1,select=1)
# trips look good.
AIC(m1)

#   379419.8
# plotting pseudo-residuals
plotPR(m1) # look a bit weird, but not sure what to do ...

####try sub-setting to only 10-20 trips and seeing if the residuals still 
## look bad - it might be that there's a few weird trips in the dataset that are skewing the models. 



# store model as an .rdata object so you don't have to run from scratch each time
file.out <- paste0("./HMM files_GPS_2013/Cro_10km_longtrips_2013_nozero.RData")

save(m1, file = file.out)



###save all plots from the plot space. 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/HMM files_GPS_2013/Output HMM plots_nozero/")



###   2.4.3 ASSIGN STATES AND OUTPUT DATAFRAME and check data #######


dat.out <- m1$data
###this are 
write.csv(dat.out, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase.csv")
dat.out$TripID <- as.factor(as.character(dat.out$TripID))
unique((dat.out$TripID))# 89 levels
dat.out$BirdID <- as.factor(as.character(dat.out$BirdId))
unique((dat.out$BirdID))# 89 levels


hist(dat.out$step)
hist(dat.out$angle)

dat.out$State <- viterbi(m1)
dat.out$State[dat.out$State == 1] <- "Travel"
dat.out$State[dat.out$State == 2] <- "Search"
dat.out$State[dat.out$State == 3] <- "Rest"
dat.out$State <- as.factor(as.character(dat.out$State))
table(dat.out$State)

##calculate percentage of each state. 
library(dplyr)
dat.out%>%
  group_by(State) %>%
  summarize(counts=n())%>%
  mutate(per=counts/sum(counts)*100)%>%
  collect()

name.out <- "."
write.csv(dat.out, name.out)
# this percentages are similar to Tash's ones!


gps <- read.csv('C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase.csv', stringsAsFactors=F)
####need to remove points that have a step length bigger than 25km. Will cite Tommys for that. 
##in total 45 out of 85500 will be filtered. 

datacleanwithnoerrors<-gps%>%
  filter(!step>25)

write.csv(datacleanwithnoerrors, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase_removesteplengtherrors.csv")
plot(datacleanwithnoerrors$step)
acf(dat.out$step[!is.na(dat.out$step)], lag.max = 300)









###3. Do script to generate the database##################
## 3.0 Add other imp variables to the database. 
## 3.1 create traveling bouts defined as traveling periods longer than 20 km. 
## 3.2 Identify the first point of each traveling period and the associated 
## soundscape map.
## 3.3 within each soundscape map divide the area in 12 cone of 30 deg each. 
## 3.4 Create a database for each soundscape maps with 12 rows, 1 for the focal 
## cone and the other 11 for the 11 non focal cones. The circle will start with 
## the focal cone being centered with north. 
## Variables: 
## ID (1 for focal cone, 0 for non focal cone) 
## Cone number (0 for focal, 1 to 11 for non focal)
## Absolute total spl in each cone
## Standarized SPL in each cone
## Absolute gdist to the 45dB contour 
## Standarized gdist to the 45dB contour 
## Distance to the colony,
## Distance travelled since the bird left the colony, 
## Total Distance in km for that particular travelling bout, 
## % of traveling time, since the bird left the colony 
## % of foraging time, since the bird left the colony


####3.0 ####
gps_2013 <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase.csv', stringsAsFactors=F)
library(dplyr)

gps_2013$TripID <- as.factor(as.character(gps_2013$TripID))
gps_2013$BirdId <- as.factor(as.character(gps_2013$BirdId))
test<-levels(gps_2013$BirdId)
##need to add to the database the following 
## Distance to the colony= Dist_cro_shelf already in the database. 

## Distance traveled since the bird left the colony, 
gps_2013<-gps_2013 %>%
  group_by(BirdId) %>%
  mutate(cum_trav_dist = cumsum(DistTrav))


## % of traveling time, since the bird left the colony 
gps_2013<-gps_2013 %>%
  group_by(BirdId) %>%
  mutate(per_trav_dist = cum_trav_dist*100/max(cum_trav_dist))

############need to add if the animal is heading out of the colony. other or 
####heading into the colony. Follow Wakefield et al 2009 to divide the trip in 
##outward, middle and inward. This is done at a population level rather than at 
## the individual level. 
## 1. Calculate distance from the colony as the proportion of the max distance 
##    reached druing that trip (dcol/dmax) and the time elapsed as a proportion 
##    of the total trip time (t/tmax)
gps_2013<-gps_2013 %>%
  group_by(BirdId) %>%
  mutate(dcol_dmax = Dist_cro_shelf/max(Dist_cro_shelf))

## create an index for each birdID
gps_2013<-gps_2013 %>%
  group_by(BirdId) %>%
  mutate(counter = row_number(BirdId))

gps_2013<-gps_2013 %>%
  group_by(BirdId) %>%
  mutate(Trip_time =  counter*15/max( counter*15))
## 2. The total variance in dcol/dmax for all locations occurring before t/tmax 
##    was then plotted against t/tmax
## The rate of change dcol/dmax with t/tmax is a measure of the rate at which
## birds move relative to the colony. 
## Hence, by graphically examining the variance of dcol/dmax with t/tmax we were
## able to identify the value of t/tmax at which the birds typically ceased 
## commuting rapidly away from the colony and that at which they began commuting 
## rapidly back again. Locations lying between these two values were categorized 
## as the middle stage, and the remainder as the outward or return stages, as appropriate.
library(ggplot2)
library(plotly)
pGPS1<-ggplot(data = gps_2013, aes(y = dcol_dmax, x =Trip_time)) +  
  geom_point(aes( colour = TripID))+  
  xlab("Prop_Trip_time") +
  ylab("Prop_max_dist") 
pGPS1
pGPS2<-ggplot(data = gps_2013) +  
  geom_point(aes(y = dcol_dmax, x =Trip_time))+ 
  xlab("Prop_Trip_time") +
  ylab("Prop_max_dist") 
pGPS2



var_asc=var_desc=c()

for (i in 1:length(seq(0,1,length.out=50))){
  
  var_asc[i]<-gps_2013%>%ungroup()%>%filter(Trip_time<seq(0,1,length.out=50)[i])%>%
    summarise(variance_dist=var(dcol_dmax))
  var_desc[i]<-gps_2013%>%ungroup()%>%filter(Trip_time>seq(0,1,length.out=50)[i])%>%
    summarise(variance_dist=var(dcol_dmax))
  
}
var_asc<-unlist(var_asc)
var_desc<-unlist(var_desc)
idx<-seq(0,1,length.out=50)

pGPS2<-ggplot() +  
  geom_point(aes(y = var_asc, x =idx))+ 
  geom_point(aes(y = var_desc, x =idx, color="red"))+ 
  xlab("Prop_Trip_time") +
  ylab("Var_dist") +
  geom_vline(xintercept=idx[17], linetype="dashed")+
  geom_vline(xintercept=idx[35], linetype="dashed", color="red")+
  theme(legend.position="none")
pGPS2

###classify trips in out middle and in and plot trips to visualize the result of it. 
gps_2013<-gps_2013 %>%
  mutate(Trip_state = case_when(Trip_time < idx[17] ~ 'out',
                                Trip_time >idx[17] & Trip_time < idx[35] ~ 'mid',
                                Trip_time > idx[35] ~ 'in'))




colony <- data.frame(Lon = 51.76666, Lat = -46.4)

plot.direx <- "C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Trip_state_threshold_population/"
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

for (i in 1:nlevels(gps_2013$TripID)){
  p1 <- ggplot() + 
    geom_point(data = gps_2013[gps_2013$TripID== levels(gps_2013$TripID)[i],], aes(x=x, y=y, colour= factor(Trip_state)), pch = 21,size = 1.5) +
    geom_sf(data = world) +
    geom_point(x=51.706972, y=-46.358639)+
    coord_sf(xlim = c(min(gps_2013$x), max(gps_2013$x)),
             ylim =c(min(gps_2013$y), max(gps_2013$y)))+
    ggtitle(levels(gps_2013$TripID)[i]) #borders(fill = "gray") + 
  plot.name <- paste0(plot.direx, levels(gps_2013$TripID)[i], ".png")
  ggsave(plot.name)
}


write.csv(gps_2013, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase_full.csv")
 
gps_2013_ID_c_f <- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013_ID_cleandatabase_full.csv', stringsAsFactors=F)
## 3.1 create traveling bouts defined as traveling periods longer than 20 km#### 
# load in data #

######select traveling bouts periods that have more than 20km. 

for (i in 1:(length(levels(gps_2013$BirdId)))){
  GPS <- subset(gps_2013, BirdId == test[i],)
  GPS$idx <- seq(1:nrow(GPS))
  gps_2013Trav<-GPS %>%
    filter(State=="Travel")
  ###create different travelling bouts and provide them with a different index
  idxdiff<-diff(gps_2013Trav$idx)
  a<-c(which(diff(gps_2013Trav$idx)>1), nrow(gps_2013Trav))
  travbout=c()
  for(j in 1:length(a)){
    if(j==1){travbout<-c(rep(j,a[j]))}else{travbout<-c(travbout,rep(j,a[j]-a[j-1]))}
    
  }
  gps_2013Trav$travbout<-travbout
  gps_2013Trav$travbout<-as.factor(as.character(gps_2013Trav$travbout))
  
  ###estimate the km travelled in each bout
  Trav20<-gps_2013Trav%>%
    group_by(travbout)%>%
    mutate(TotdisttravBout= sum(DistTrav))%>%
    group_by(travbout)%>% filter(TotdisttravBout >20)
  
  Boutn<-length(unique(Trav20$travbout))
  Bouttot<-length(levels(Trav20$travbout))
  
  c<- data.frame(BirdId=test[i],Boutn=Boutn, Bouttot=Bouttot )
  
  ###create new database with all birdID which travel distance is larger than 20km
  if (exists ("newdata")){
    newdata<-rbind(newdata,Trav20)
    fulldata<-rbind(fulldata,gps_2013Trav)
    Boutanal<- rbind(Boutanal,c)
    
  }else {
    newdata<-Trav20
    fulldata<-gps_2013Trav
    Boutanal<- c
  }
}

gps_2013Trav20<-newdata
###full data contains the newdatabase with all travelling bouts longer than 20km
### while newdatabase contains the database with all travelling bouts independently of 
write.csv(gps_2013Trav20, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/gps_2013Trav20.csv")






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










