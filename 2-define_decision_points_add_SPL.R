
## ---------------------------
##
## Script name: 2-define_decision_points_add_SPL
##
## Purpose of script: This script defines decision points, splits possible trajectories
## into 'segments', and matches these to modelled microbarom infrasound
##
## Author: Dr. Lucia Martina Martin Lopez (with support by Dr Natasha Gillies)
##
## Email: gilliesne@gmail.com
##
## ---------------------------


### 0.0 Load the packages ------------------------------------------------------

# 0.0.0 Define the packages
packages <- c("dplyr", "ncdf4", "lubridate", "birk", "pracma")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))

# 0.0.2 Load boutFinder custom function
source("FUNCTION_boutFinder.R")

## 0.1 Load the data -----------------------------------------------------------

gps_2013 <- read.csv("Data_inputs/WAAL_2013_gps_labelled.csv", stringsAsFactors = F)



### 1.0 Create travel bouts ----------------------------------------------------

# 1.0.0 Index all rows in each trip for each bird
gps_2013Trav <- gps_2013 %>% 
            group_by(TripID) %>%
            mutate(idx = seq(1:n())) %>%
# 1.0.1 Filter fixes labelled as 'travel'
            filter(State == "Travel") %>% 
# 1.0.2 Label individual bouts
            mutate(travbout = boutFinder(idx)) %>% 
            data.frame()

gps_2013Trav$travbout <- as.factor(as.character(gps_2013Trav$travbout))
            

## 1.2 Estimate distance travelled in each bout & filter for travel bouts > 20km ----
gps_2013Trav20 <- gps_2013Trav %>%
          group_by(TripID, travbout) %>%
          mutate(TotdisttravBout = sum(DistTrav)) %>%
          filter(TotdisttravBout > 20) %>%
          data.frame()

# 1.2.0 Output dataframe - used for sensitivity analysis
#write.csv(gps_2013Trav20, "Data_inputs/WAAL_2013_gps_Trav20.csv", row.names = F)


## 1.3 Identify the first point of each travelling period (decision point) -----
gps_2013Trav20_1stpoint <- gps_2013Trav20 %>%
                           group_by(TripID, travbout) %>%
                           slice_head(n = 1) %>%
                           ungroup()


### 2.0 Match each travel bout to SPL map --------------------------------------

#### NOTE: This script may take several hours to run depending on computing power.

## 2.1 Point to soundscape data ------------------------------------------------
path_to_IS_maps <- "E:/Soundscapes/"
IS_folder_maps <- dir(path_to_IS_maps, pattern = "2013")
GPS_ID_segments <- list()

## 2.2 Define segment parameters -----------------------------------------------
aperture <- 60
transectlength <- 2025
segmentno <- 360/aperture
angDiffs <-  c(seq(0, 180, by = aperture),
               seq((-180 + aperture),-aperture, aperture))
  
for (x in 1:length(IS_folder_maps)) {
    
  ## 2.3 Point to maps and isolate bird data -----------------------------------
  # 2.3.1 Point to IS map relevant to bird
  birdmapid <- IS_folder_maps[x]
  path_to_IS_files <- paste0(path_to_IS_maps, birdmapid, "/Interp/")
    
  # 2.3.2 Load infrasound map files 
  IS_files <- list.files(path = path_to_IS_files)
    
  # 2.3.3 Extract bird ID from the folder name
  IDbirdmap <- sapply(strsplit(IS_folder_maps[x], "_", fixed = TRUE),
                        function(i) paste(head(i, -1), collapse = "_"))
    
  if (grepl("_", IDbirdmap, fixed = TRUE) == FALSE) { IDbirdmap <- paste0(IDbirdmap, "_1")}
    
  # Change underscore to '.'
  IDbirdmap <- gsub("_", ".", IDbirdmap)
    
  # 2.3.4 Select the bird trip for which the soundscape is being loaded
  gps_2013_ID1 <- gps_2013Trav20_1stpoint %>% 
                  filter(TripID == IDbirdmap)
    
  # If there are no GPS data for that map, skip
  if (nrow(gps_2013_ID1) == 0) next
    
  # Check data formatting
  gps_2013_ID1$TripID <- as.factor(as.character(gps_2013_ID1$TripID))
  gps_2013_ID1$DateTime <- as.POSIXct(gps_2013_ID1$DateTime, tz = "UTC")
    
    
  # 2.3.5 Get closest hour for each GPS fix to match with soundscapes 
  gps_2013_ID1$maptobe <-
    format(round(gps_2013_ID1$DateTime, units = "hours"), format = "%Y-%m-%d %H:%M")   
  
  ## 2.4 Match each GPS fix to the closest SPL map in time and compute segments ----
  
  for (i in 1:nrow(gps_2013_ID1)) {
      
    # 2.4.1 Find the closest hour for each GPS point in the IS_files
     
    TG <- gps_2013_ID1$maptobe[i]
    Maptobe <- paste0(sub("\\_Soundscape_.*", 
                          "",
                          IS_files[i]),
                      "_Soundscape_",
                      substr(TG, 1, 4),
                      substr(TG, 6, 7),
                      substr(TG, 9, 10),
                      "_",
                      substr(TG, 12, 13),
                      ".nc")
    IDX <- which(IS_files == Maptobe)
      
    # If no map is found then fill it with NAs.
    if (length(IDX) == 0) {
      
      segments = data.frame(
        segment_ID = as.numeric(NA),
        segment_n = as.numeric(NA),
        segment_vert_lef = as.numeric(NA),
        segment_vert_rig = as.numeric(NA),
        abs_SPL_2000dB = as.numeric(NA),
        abs_SPL_2000 = as.numeric(NA),
        abs_SPL_2000_std = as.numeric(NA),
        abs_SPL_2000dB_std = as.numeric(NA),
        birdID = as.character(birdmapid),
        TripID = as.character(NA),
        mapID = as.character(TG),
        counter = as.numeric(NA),
        Sex = as.character(gps_2013_ID1$Sex),
        x_lon = as.numeric(NA),
        y_lat = as.numeric(NA),
        State = as.character(NA),
        Dist_cro_shelf = as.numeric(NA),
        WindSp = as.numeric(NA), # wind speed
        Dev.wind2 = as.numeric(NA), # wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
        relDir_adj.bearing = as.numeric(NA),
        stringsAsFactors = FALSE
      ) 
        
        
      }   else {
        
    ID = IS_files[IDX]
        
    # 2.4.2 Extract the dateTime from the SPL map 
    a <- paste0(substr(ID, 19, 22),
                ":",
                substr(ID, 23, 24),
                ":",
                substr(ID, 25, 26),
                ":",
                substr(ID, 28, 29),
                ":00:00")
    mapDateTime <- parse_date_time(a, 'ymd HMS')
    mapDateTime <- as.POSIXct(mapDateTime, tz = "UTC")
        
    # 2.4.3 Load the nc file - extract SPL for each latitude/longitude 
    nc_file <- nc_open(paste0(path_to_IS_files, ID))
    latitude <- ncvar_get(nc_file, "lat") #deg
    longitude <- ncvar_get(nc_file, "lon") # deg
    OSWIPa <- ncvar_get(nc_file, "OSWI_interp_spatial_time") #Interpolated OSWI + TLloss + NormGrid + Time in Pa
    Pref <- 20e-6 
    OSWIdB <- 10 * log10(OSWIPa/(Pref^2))
    latitude <- as.vector(latitude)
    longitude <- as.vector(longitude)
    SPL_dB <- as.vector(OSWIdB)
    SPL_Pa <- as.vector(OSWIPa)
    baz <- ncvar_get(nc_file,"baz") # back azimuth angle, clockwise to north
    baz <- as.vector(baz)
    Gdist <- ncvar_get(nc_file,"dist") # Geodesic distance in degrees
    X <- as.data.frame(cbind(latitude,longitude,SPL_dB,SPL_Pa, Gdist,baz))
    X2 <- X[complete.cases(X * 0), , drop = FALSE]
        
    nc_close(nc_file)
        
    # 2.4.4 Set Back zenith angles (BAZ) in accordance with the GPS Bearing
    X2$baz_converted <- ifelse(
      X2$baz <= 90 & X2$baz >= 0,
      X2$baz - 180,
      ifelse(
        X2$baz > 90 & X2$baz <= 180,
        X2$baz - 180,
        ifelse(
          X2$baz >= -180 & X2$baz <= -90,
          (X2$baz + 180),
          ifelse(X2$baz > -90 &
                    X2$baz < 0, (X2$baz + 180), NA)
        )
      )
    )
    
        
    ## 2.5 Within each soundscape map divide the area into 6 segments of 60 deg each  ----
    ## Get the baz angles for the 12 segments starting from the left side of
    ## the focal segment, and then clockwise. 
    ## Always the same relative angles from the ontrack one! 
    
    # 2.5.1 Set the focal segment as +/- 30 degrees from bird's bearing
    focal_segment_deg <-
      c( round(gps_2013_ID1$Bearing[i]) - aperture / 2,
        round(gps_2013_ID1$Bearing[i]) + aperture / 2 )
    
    if (any(abs(focal_segment_deg) > 180)) {
      a <- which(abs(focal_segment_deg) > 180)
      if (focal_segment_deg[a] < 0) {
        focal_segment_deg[a] <- 360 + focal_segment_deg[a]
      } else{
        focal_segment_deg[a] <- 360 - focal_segment_deg[a]
      }
    }
    
    # 2.5.2 Create the other segments from the focal segment
    if (focal_segment_deg[1] <= (-180 + aperture)) {
      segment_vert_lef <-
        (seq(from = focal_segment_deg[1], to = 180, by = aperture))
      segment_vert_rig <-
        c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
    } else {
      segment_vert_lef <-
        c(seq(from = focal_segment_deg[1], to = 180, by = aperture),
          rev(
            seq(
              from = focal_segment_deg[1] - aperture,
              to = -180,
              by = -aperture
            )
          ))
      segment_vert_rig <-
        c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
    }
    
        
    # 2.5.3 Add the exception to remove the 0 degree segment that will be created 
    # when segment_vert_lef = 180 and segment_vert_rig = -180
    if (any(abs(diff(segment_vert_lef)) == 360)) {
      segment_vert_lef <-
        segment_vert_lef[-which(diff(segment_vert_lef) != aperture)]
      segment_vert_rig <-
        c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
    }
    
    if (length(segment_vert_lef) > segmentno) {
      segment_vert_lef <- c(segment_vert_lef[c(1:segmentno)])
      segment_vert_rig <- c(segment_vert_rig[c(1:segmentno)])
    }
    
    # 2.5.4 Label the segments as focal vs non-focal and numeric ID (in 
    # clockwise direction)
    segment_ID <- c(0,rep(1, segmentno - 1))
    segment_n <- seq(1,segmentno)
    
    # 2.5.5 Make a dataframe containing all the segments    
    segments <- as.data.frame(cbind(segment_ID, # segment identity (focal vs non)
                                    segment_n, # segment number
                                    segment_vert_lef, # left bound
                                    segment_vert_rig)) # right bound

        
    ## 2.6 For each segment estimate the abs & standarized SPL and gdist to 45dB ----

    # 2.6.0 Add new empty column to dataframe to be filled
    segments$abs_SPL_2000dB <- NA
    segments$abs_SPL_2000 <- NA
   
    # 2.6.1 Loop through segments and calculate integrated SPL in each
    for (c in 1:nrow(segments)) {
          
      if (segment_vert_lef[c] > 0 & segment_vert_rig[c] < 0) {
        X2_1 <- X2 %>%
          filter(X2$baz_converted >= segment_vert_lef[c]  &
                   X2$baz_converted <= 180)
        X2_2 <- X2 %>%
          filter(X2$baz_converted <= segment_vert_rig[c]  &
                   X2$baz_converted >= -180)
       
        newX <- rbind(X2_1, X2_2)
        
        abs_SPL_2000 <- newX %>%
          filter(Gdist <= transectlength) %>%
          summarise(x = sum(SPL_Pa))
        
        segments$abs_SPL_2000[c] <- as.numeric(abs_SPL_2000)
        
        abs_SPL_2000dB <- newX %>%
          filter(Gdist <= transectlength) %>%
          summarise(x = 10 * log10(sum(SPL_Pa) / (Pref ^ 2)))
        
        segments$abs_SPL_2000dB[c] <- as.numeric(abs_SPL_2000dB)
        
      } else {
       
        abs_SPL_2000<- X2 %>%
          filter(X2$baz_converted >= segment_vert_lef[c]  & X2$baz_converted <= segment_vert_rig[c] & Gdist <= transectlength) %>%
          summarise(x = sum(SPL_Pa))
        
        segments$abs_SPL_2000[c]<-as.numeric(abs_SPL_2000)
        
        abs_SPL_2000dB <- X2 %>%
          filter( X2$baz_converted >= segment_vert_lef[c]  &
              X2$baz_converted <= segment_vert_rig[c] &
              Gdist <= transectlength ) %>%
          summarise(x = 10 * log10(sum(SPL_Pa) / (Pref ^ 2)))
        
        segments$abs_SPL_2000dB[c] <- as.numeric(abs_SPL_2000dB)
        
          }
        }
        
    segments$abs_SPL_2000_std <- scale(segments$abs_SPL_2000)
    segments$abs_SPL_2000dB_std <- scale(segments$abs_SPL_2000dB)
    segments$birdID <- birdmapid
    segments$TripID <- gps_2013_ID1$TripID[i]
    segments$mapID <- TG
    segments$counter = gps_2013_ID1$counter[i]
    segments$Sex = gps_2013_ID1$Sex[i]
    segments$x_lon = gps_2013_ID1$x_lon[i]
    segments$y_lat = gps_2013_ID1$y_lat[i]
    segments$State = gps_2013_ID1$State[i]
    segments$Dist_cro_shelf = gps_2013_ID1$Dist_cro_shelf[i]
    segments$WindSp = gps_2013_ID1$WindSp[i] # wind speed
    segments$Dev.wind2 = gps_2013_ID1$Dev.wind2[i] # wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
   
    ## 2.7 Find wind direction for each segment  -------------------------------
    
    # 2.7.1 Find angular differences between each segment 
    segments$segment_vert_lef.DIFF <- rep(angDiffs, nrow(segments) / segmentno)
    
    # 2.7.2 Calculate relative wind direction adjusted for segment and convert to c(-180, 180)
    segments$relDir_adj <- segments$Dev.wind2 + segments$segment_vert_lef.DIFF
    segments$relDir_adj.bearing <-  abs(ifelse(segments$relDir_adj > 180, -360 + segments$relDir_adj, 
                                                segments$relDir_adj))
        
    # 2.7.3 Remove columns used for calculation
    segments$segment_vert_lef.DIFF <- NULL
    segments$relDir_adj <- NULL
    segments$relDir <- NULL
    
    }
    
    if (x == 1) {
      GPS_ID_segments = segments
    } else{
      GPS_ID_segments = rbind(GPS_ID_segments, segments)
    }
  
  }
  
  print(x)
    
}

  
### 3.0 Process and output segment dataframe  ----------------------------------

# 3.0.0 Remove NA variables
GPS_ID_segments <- subset(GPS_ID_segments, !is.na(segment_ID))
# 3.0.1 Rename relDir columns 
GPS_ID_segments <- rename(GPS_ID_segments, relDir = relDir_adj.bearing)
# 3.0.2 Make (decision) Point ID variable
GPS_ID_segments$pointID <- paste(GPS_ID_segments$TripID, GPS_ID_segments$counter, sep = ".")
# 3.0.3 Make focal segment '1' and non-focal '0'
GPS_ID_segments$segment_ID <- abs(GPS_ID_segments$segment_ID - 1)
# 3.0.4 Remove columns not relevant for analysis
cols_rm <- c("segment_vert_lef", "segment_vert_rig", "mapID", "counter", "Dev.wind2")
GPS_ID_segments[,cols_rm] <- NULL

## 3.1 Write to csv ------------------------------------------------------------
write.csv(GPS_ID_segments, paste0("./Data_inputs/WAAL_2013_gps_processed_aperture", aperture, "deg.csv"), row.names = F)
  
