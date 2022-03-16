#'*AIM: Adjust the duration of the travel bout denoting a decision point to see if this influences*
#'*our results.*

## Load the part-processed data set (dataset prior to calculating decision points)
gps_2013 <- read.csv('./Data_inputs/WAAL_GPS_2013-no_SPL.csv', stringsAsFactors=F)

## TO DO: Remove unnecessary columns

## Load packages
packages <- c("dplyr", "ncdf4", "lubridate", "birk", "ggplot2", "survival")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


### Specify deg2rad function
deg2rad <- function(deg) {(deg * pi) / (180)}


# 1. Filter traveling bouts with varying distance thresholds ----------------------------------------------

## Set bout distances to iterate through
bout_dist <- c(20, 50, 100, 200, 400)

## Extract bouts of each distance for each bird
birds <- unique(gps_2013$BirdId)

for (d in 1:length(bout_dist)) {
  
  for (i in 1:(length(birds))) {
    
    GPS <- subset(gps_2013, BirdId == birds[i], )
    GPS$idx <- seq(1:nrow(GPS))
    
    ## Select travelling periods only
    gps_2013Trav <- GPS %>%
      filter(State == "Travel")
    
    ## Separate each bout of travelling by looking for consecutive fixes and give them an index ID
    idxdiff <- diff(gps_2013Trav$idx)
    a <- c(which(diff(gps_2013Trav$idx) > 1), nrow(gps_2013Trav))
    
    travbout <- c()
    for (j in 1:length(a)) {
      if (j == 1) {
        travbout <- c(rep(j, a[j]))
      } else {
        travbout <- c(travbout, rep(j, a[j] - a[j - 1]))
      }
    }
    
    ## Assign travbout ID
    gps_2013Trav$travbout <- travbout
    gps_2013Trav$travbout <-
      as.factor(as.character(gps_2013Trav$travbout))
    
    ## Group each travel bout - calculate total distance travelled - filter bouts according
    ## to distance threshold
    Trav20 <- gps_2013Trav %>%
      group_by(travbout) %>%
      mutate(TotdisttravBout = sum(DistTrav), distThresh = bout_dist[d]) %>%
      group_by(travbout) %>% filter(TotdisttravBout > bout_dist[d]) %>% data.frame()
    
    ## NOTE - NOT SURE THESE LINES DO ANYTHING
    #Boutn <- length(unique(Trav20$travbout))
    #Bouttot <- length(levels(Trav20$travbout))
    
    #c <- data.frame(BirdId = birds[i],
                  #  Boutn = Boutn,
                   # Bouttot = Bouttot)
    
    ## Create new database with all birdID which travel distance is larger than 20km
    if (exists ("newdata")) {
      newdata <- rbind(newdata, Trav20)
      #fulldata <- rbind(fulldata, gps_2013Trav)
      #Boutanal <- rbind(Boutanal, c)
      
    } else {  newdata <- Trav20
      #fulldata <- gps_2013Trav
      #Boutanal <- c
    }
    
  }
  
  ## Output dataframe 
  print(paste("Dist", bout_dist[d], nrow(newdata)))
  
  write.csv(newdata, 
            paste0("Data_inputs/Decision_points/gps_2013_travBout", bout_dist[d], ".csv"), 
            row.names = F)
  
  newdata <- NULL
  
}



# 2. Identify first point of each period and match to associated SPL map --------

### Iterate through each data file, identify SPL maps, match to data

## Point to distance data
path_to_data <- "Data_inputs/Decision_points/"
dist_data_files <- dir(path_to_data, pattern = ".csv")

## Point to soundscape data
path_to_IS_maps <- "E:/Soundscapes/"
IS_folder_maps <- dir(path_to_IS_maps, pattern = "2013")
GPS_ID_cones <- list()

# Define cone parameters 
coneaperture<- 30
coneno <- 360/coneaperture
tansectlength<-2025
tansectlength_f<-3000

for (f in 1:length(dist_data_files)) {
  print(f)
  
  ## Load dataset
  gps_2013Trav20 <- read.csv(paste0(path_to_data, dist_data_files[f]), stringsAsFactors=F)
  
  ## Change name of TripID 201377.3 to 201377.1
  gps_2013Trav20$TripID[gps_2013Trav20$TripID=="201377.3"] <- "201377.1"
  
  ## Select the first point of travel for each travel bout
  gps_2013Trav20_1stpoint <-
    gps_2013Trav20 %>% group_by(BirdId, travbout) %>%
    slice_head(n = 1) %>% ungroup()
  
  
  #' * The next section of code matches GPS data to soundscape maps. It may take several hours to run.*
  
  tok <- Sys.time()
  for (x in 1:length(IS_folder_maps)){
    
    ## Point to IS map relevant to bird
    birdmapid <- IS_folder_maps[x]
    path_to_IS_files <- paste0(path_to_IS_maps, birdmapid, "/Interp/")
    
    # Load IS files 
    IS_files <- list.files(path = path_to_IS_files)
    
    # Extract bird ID
    IDbirdmap <- sapply(strsplit(IS_folder_maps[x], "_", fixed = TRUE),
                        function(i) paste(head(i, -1), collapse = "_"))
    
    if (grepl("_", IDbirdmap, fixed = TRUE) == FALSE) { IDbirdmap <- paste0(IDbirdmap, "_1")}
    
    # Change underscore to '.'
    IDbirdmap <- gsub("_", ".", IDbirdmap)
    
    ## Select the bird trip for which the soundscape is being loaded
    gps_2013_ID1 <- gps_2013Trav20_1stpoint %>% 
      filter(TripID == IDbirdmap)
    
    # If there are no GPS data for that map, skip
    if(nrow(gps_2013_ID1) == 0) next
    
    gps_2013_ID1$TripID <- as.factor(as.character(gps_2013_ID1$TripID))
    gps_2013_ID1$DateTime <- as.POSIXct(gps_2013_ID1$DateTime, tz = "UTC")
    
    
    ### Get closest hour for each GPS fix to match with soundscapes 
    
    ## Variables: 
    ## ID (1 for focal cone, 0 for non focal cone) 
    ## Cone number (0 for focal, 1 to 11 for non focal)
    ## Absolute total spl in each cone
    ## Standarized SPL in each cone
    ## Absolute gdist to the 45dB contour 
    ## Standarized gdist to the 45dB contour 
    
    gps_2013_ID1$maptobe <- format(round(gps_2013_ID1$DateTime, units="hours"), format="%Y-%m-%d %H:%M")   
    
    for (i in 1:nrow(gps_2013_ID1)){
      
      ## Find the maptobe for each GPS point in the IS_files
      ## If no map is found then fill it with NAs. 
      
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
                         pointID=as.character(NA),
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
                         relDir_adj.bearing=as.numeric(NA),
                         stringsAsFactors=FALSE) 
        
        
      }   else {
        
        ID = IS_files[IDX]
        
        ## Extract the dateTime from the SPL map 
        a<-paste0(substr(ID, 19, 22),":",substr(ID, 23, 24),":",substr(ID, 25, 26),":",substr(ID, 28, 29),":00:00")
        mapDateTime<-parse_date_time(a, 'ymd HMS')
        mapDateTime <- as.POSIXct(mapDateTime, tz = "UTC")
        
        ## Load the nc file - extract SPL for each latitude/longitude 
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
        
        nc_close(nc_file)
        
        ## Set Back zenith angles in accordance with the GPS Bearing
        X2$baz_converted<-ifelse(X2$baz<=90 & X2$baz>=0,X2$baz-180,
                                 ifelse (X2$baz>90 & X2$baz<=180,X2$baz-180,
                                         ifelse (X2$baz>=-180 & X2$baz<=-90,(X2$baz+180),
                                                 ifelse (X2$baz>-90 & X2$baz<0,(X2$baz+180),NA))))
        

        ### Within each soundscape map divide the area in 12 cone of 30 deg each
        
        ##Get the baz angles for the 12 cones starting from the left side of 
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
        
        
        
        if (focal_cone_deg[1]<= (-180+coneaperture)){
          cone_vert_lef<-(seq(from=focal_cone_deg[1], to= 180, by=coneaperture))
          cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
        }else{
          cone_vert_lef<-c(seq(from=focal_cone_deg[1], to= 180, by=coneaperture),rev(seq(from=focal_cone_deg[1]-coneaperture, to= -180, by=-coneaperture)))
          cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
        }
        
        
        ###add the exception to remove the cone that will be created with0
        ## cone_vert_lef=180 and cone_vert_rig=-180
        if (any(abs(diff(cone_vert_lef))==360)) {
          cone_vert_lef<-cone_vert_lef[- which(diff(cone_vert_lef)!=coneaperture)]
          cone_vert_rig<-c(cone_vert_lef[c(2:length(cone_vert_lef),1)])
        }
        
        if (length(cone_vert_lef)>coneno) {
          cone_vert_lef<-c(cone_vert_lef[c(1:coneno)])
          cone_vert_rig<-c(cone_vert_rig[c(1:coneno)])
        }
        
        
        
        cone_ID<-c(0,rep(1, coneno-1))# it it is focal or non focal
        cone_n<-seq(1,coneno)# cone number, being 1 the focal and from 2 to coneno the non
        ##focal ones in a clockwise direction
        
        Cones=as.data.frame(cbind(cone_ID,cone_n,cone_vert_lef,cone_vert_rig))
        Cones$pointID <- paste0(gps_2013_ID1$TripID[1], "_", i)
        
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
        
        Cones$tw_sup_d=gps_2013_ID1$WindSp[i]*cos(deg2rad(gps_2013_ID1$Dev.wind[i]))  # tail wind support with directionality 
        Cones$cw_sup_d=gps_2013_ID1$WindSp[i]*sin(deg2rad(gps_2013_ID1$Dev.wind[i]))  # cross wind support with directionality 
        Cones$tw_sup_nod=gps_2013_ID1$WindSp[i]*cos(deg2rad(gps_2013_ID1$Dev.wind2[i]))  # tail wind support with no directionality 
        Cones$cw_sup_nod=gps_2013_ID1$WindSp[i]*sin(deg2rad(gps_2013_ID1$Dev.wind2[i]))  # cross wind support with no directionality 
        
        
        #' *Find wind direction for each cone*
        
        cone_no <- 360/coneaperture
        angDiffs <- c(seq(0, 180, by = coneaperture), seq((-180+coneaperture), -coneaperture, coneaperture))
        Cones$cone_vert_lef.DIFF <- rep(angDiffs, nrow(Cones)/coneno)
        
        ## Calculate relative wind direction adjusted for cone and convert to c(-180, 180)
        Cones$relDir_adj <- Cones$Dev.wind2 + Cones$cone_vert_lef.DIFF
        Cones$relDir_adj.bearing <-  abs(ifelse(Cones$relDir_adj > 180, -360 + Cones$relDir_adj, 
                                                Cones$relDir_adj))
        
        ## Remove unnecessary columns
        Cones$cone_vert_lef.DIFF <- NULL
        Cones$relDir_adj <- NULL
        Cones$relDir <- NULL
        
        
      }
      
      if (x==1 & i==1){GPS_ID_cones=Cones
      
      }else{
        GPS_ID_cones=rbind(GPS_ID_cones,Cones)
      }
      
    }
    print(x)
    
  }
  tik <- Sys.time()
  
  tik-tok
 
  ## Process output 
  GPS_ID_cones <- subset(GPS_ID_cones, !is.na(cone_ID))
  names(GPS_ID_cones)[32] <- "relDir"
  GPS_ID_cones$pointID <- paste(GPS_ID_cones$TripID, GPS_ID_cones$counter, sep = ".")
  GPS_ID_cones$cone_ID <- abs(GPS_ID_cones$cone_ID-1)
  
  ## Write to csv
  write.csv(GPS_ID_cones, paste0("./Data_inputs/GPS_ID_cones", coneaperture, "deg_", bout_dist[f], "dist_full_database.csv"), row.names = F)
  
}


# 3. Run the model for each distance threshold -----------------------------------------

## Point to distance data
path_to_data <- "Data_inputs/"
dist_data_files <- dir(path_to_data, pattern = "0dist")

# Get distances to include in output
dists <- dist_data_files %>%
  strsplit( "_" ) %>%
  sapply( "[", 4)

## Loop through each dataset, fitting congitional logit
model_list <- vector(mode = "list", length = length(dist_data_files))

for (i in 1:length(dist_data_files)){
  
  print(dists[i])
  modDat <- data.table::fread(paste0(path_to_data, dist_data_files[i]), data.table = F)
  nrow(modDat)
  
  modDat <- subset(modDat, !is.na(abs_SPL_2000dB))
  modDat <- subset(modDat, !is.infinite(abs_SPL_2000dB))
  
  # Rename and process variables
  names(modDat)[1] <- "case"
  
  factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
  modDat[factor_vars] <- lapply(modDat[factor_vars], factor)
  
  ## Scale continuous variables
  cont_vars <- c("abs_SPL_2000dB", "WindSp", "relDir")
  modDat[cont_vars] <- lapply(modDat[cont_vars], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 
  
  
  ## Fit the model (NOTE: this is not the final model)
  RSF_mod <- clogit(case ~ strata(pointID) + abs_SPL_2000dB*relDir + 
                           cluster(birdID), method = 'approximate', data = modDat)

  model_list[[i]] <- summary(RSF_mod)
  
}
