## ---------------------------
##
## Script name: SUPP-MAT_sensitivity_analysis_apertures.R
##
## Purpose of script: This script varies the segment apertures between 15 and 90
## degrees, and assess how this influences the covariate output.
##
## Author: Dr. Natasha Gillies
##
## Date Created: 2022-03-17
##
## R version: 4.1.1 (2021-08-10)
##
## Email: gilliesne@gmail.com
##
## ---------------------------

### 0.0 Load packages ----------------------------------------------------------

# 0.0.0 Specify the packages
packages <- c("dplyr", "ncdf4", "lubridate", "birk", "ggplot2", "survival")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))

# 0.0.2 Specify deg2rad function
deg2rad <- function(deg) {(deg * pi) / (180)}


### 1.0 Load the data ----------------------------------------------------------

# 1.0.1 Load the GPS data
gps_2013Trav20 <- read.csv('Data_inputs/WAAL_2013_gps_Trav20.csv', stringsAsFactors = F)

# 1.0.2 Isolate first point of bout
gps_2013Trav20_1stpoint <- gps_2013Trav20 %>% 
                           group_by(ID, travbout) %>%
                           slice_head(n = 1) %>% 
                           ungroup()

# 1.0.2 Point to soundscape data 
path_to_IS_maps <- "E:/Soundscapes/"
IS_folder_maps <- dir(path_to_IS_maps, pattern = "2013")
GPS_ID_segments <- list()

### 2.0 Loop through each aperture, match SPL map and calculate effect sizes ---

## 2.0.0 Define segment parameters ---------------------------------------------
apertures <- c(15, 30, 60, 90)
transectlength <- 2025

for (a in 1:length(apertures)) {
  
  # 2.0.1 Set the segment-specific parameters
  segmentaperture <- apertures[a]
  segmentno <- 360/segmentaperture
  
  print(segmentaperture)
  
  ## 2.1 Match SPL data to GPS and split into segments -------------------------
  
  tok <- Sys.time()
  for (x in 1:length(IS_folder_maps)) {
    
    # 2.1.0 Point to IS map relevant to bird
    birdmapid <- IS_folder_maps[x]
    path_to_IS_files <- paste0(path_to_IS_maps, birdmapid, "/Interp/")
    
    # 2.1.1 Load IS files 
    IS_files <- list.files(path = path_to_IS_files)
    
    # 2.1.2 Extract bird ID
    IDbirdmap <- sapply(strsplit(IS_folder_maps[x], "_", fixed = TRUE),
                        function(i) paste(head(i, -1), collapse = "_"))
    
    if (grepl("_", IDbirdmap, fixed = TRUE) == FALSE) { IDbirdmap <- paste0(IDbirdmap, "_1")}
    
    # Change underscore to '.'
    IDbirdmap <- gsub("_", ".", IDbirdmap)
    
    # 2.1.3 Select the bird trip for which the soundscape is being loaded
    gps_2013_ID1 <- gps_2013Trav20_1stpoint %>% 
      filter(TripID == IDbirdmap)
    
    # If there are no GPS data for that map, skip
    if(nrow(gps_2013_ID1) == 0) next
    
    # Check data formatting
    gps_2013_ID1$TripID <- as.factor(as.character(gps_2013_ID1$TripID))
    gps_2013_ID1$DateTime <- as.POSIXct(gps_2013_ID1$DateTime, tz = "UTC")
    
    
    # 2.1.4 Get closest hour for each GPS fix to match with soundscapes 
    gps_2013_ID1$maptobe <-
      format(round(gps_2013_ID1$DateTime, units = "hours"), format = "%Y-%m-%d %H:%M")
    
    
    ## 2.2 Find the maptobe for each GPS point in the IS_files -----------------
    ## If no map is found then fill it with NAs. 
    
    for (i in 1:nrow(gps_2013_ID1) ){
      
      # 2.2.0 Get the timepoint
      TG <- gps_2013_ID1$maptobe[i]
      Maptobe <-
        paste0(
          sub("\\_Soundscape_.*", "", IS_files[i]),
          "_Soundscape_",
          substr(TG, 1, 4),
          substr(TG, 6, 7),
          substr(TG, 9, 10),
          "_",
          substr(TG, 12, 13),
          ".nc"
        )
      
      IDX <- which(IS_files == Maptobe)
      
      if (length(IDX) == 0) {
        
        # 2.2.1 Fill empty map with NAs
        segments = data.frame(
          segment_ID = as.numeric(NA),
          segment_n = as.numeric(NA),
          segment_vert_lef = as.numeric(NA),
          segment_vert_rig = as.numeric(NA),
          abs_SPL_2000dB = as.numeric(NA),
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
          max_dist = as.numeric(NA),
          per_trav_dist = as.numeric(NA),
          per_trip_time = as.numeric(NA),
          Trip_state = as.character(NA),
          WindSp = as.numeric(NA), # wind speed
          WindDir = as.numeric(NA), # wind direction
          Dev.wind2 = as.numeric(NA), # wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
          relDir_adj.bearing = as.numeric(NA),
            stringsAsFactors = FALSE
        ) 
        
      }   else {
        
      ID = IS_files[IDX]
        
      # 2.2.2 Extract the dateTime from the SPL map 
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
        
      # 2.2.3 Load the nc file - extract SPL for each latitude/longitude 
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
      baz <- ncvar_get(nc_file,"baz")# back azimuth angle, clockwise to north
      baz <- as.vector(baz)
      Gdist <- ncvar_get(nc_file,"dist")# Geodesic distance in degrees
      X <- as.data.frame(cbind(latitude,longitude,SPL_dB,SPL_Pa, Gdist,baz))
      X2 <- X[complete.cases(X * 0), , drop = FALSE]
      
      nc_close(nc_file)
        
      # 2.2.4 Set Back zenith angles (BAZ) in accordance with the GPS Bearing
      X2$baz_converted <- ifelse(
        X2$baz <= 90 & X2$baz >= 0,
        X2$baz - 180,
        ifelse (
          X2$baz > 90 & X2$baz <= 180,
          X2$baz - 180,
          ifelse (
            X2$baz >= -180 & X2$baz <= -90,
            (X2$baz + 180),
            ifelse (X2$baz > -90 &
                      X2$baz < 0, (X2$baz + 180), NA)
          )
        )
      )
        
      ## 2.3 Within each soundscape map divide the area into 12 segments of 30 deg each  ----
      ## Get the baz angles for the 12 segments starting from the left side of
      ## the focal segment, and then clockwise. 
      ## Always the same relative angles from the ontrack one! 
      
      # 2.3.1 Set the focal segment as +/- 15 degrees from bird's bearing
      focal_segment_deg <- c(
          round(gps_2013_ID1$Bearing[i]) - segmentaperture / 2,
          round(gps_2013_ID1$Bearing[i]) + segmentaperture / 2
        )
      
      if (any(abs(focal_segment_deg) > 180)) {
        b <- which(abs(focal_segment_deg) > 180)
        if (focal_segment_deg[b] < 0) {
          focal_segment_deg[b] <- 360 + focal_segment_deg[b]
        } else{
          focal_segment_deg[b] <- 360 - focal_segment_deg[b]
        }
      }
        
        
     # 2.3.2 Create the other segments from the focal segment   
      if (focal_segment_deg[1]<= (-180 + segmentaperture)) {
        segment_vert_lef <-
          (seq(from = focal_segment_deg[1], to = 180, by = segmentaperture))
        segment_vert_rig <-
          c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
      } else{
        segment_vert_lef <-
          c(seq(from = focal_segment_deg[1], to = 180, by = segmentaperture),
            rev(
              seq(
                from = focal_segment_deg[1] - segmentaperture,
                to = -180,
                by = -segmentaperture
              )
            ))
        segment_vert_rig <-
          c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
      }
      
        
      # 2.3.3 Add the exception to remove the 0 degree segment that will be created 
      # when segment_vert_lef = 180 and segment_vert_rig = -180
      if (any(abs(diff(segment_vert_lef)) == 360)) {
        segment_vert_lef <-
          segment_vert_lef[-which(diff(segment_vert_lef) != segmentaperture)]
        segment_vert_rig <-
          c(segment_vert_lef[c(2:length(segment_vert_lef), 1)])
      }
      
      if (length(segment_vert_lef) > segmentno) {
        segment_vert_lef <- c(segment_vert_lef[c(1:segmentno)])
        segment_vert_rig <- c(segment_vert_rig[c(1:segmentno)])
      }
      
        
      # 2.3.4 Label the segments as focal vs non-focal and numeric ID (in 
      # clockwise direction)  
      segment_ID <- c(0, rep(1, segmentno - 1))
      segment_n <- seq(1, segmentno)
        
      # 2.5.5 Make a dataframe containing all the segments    
      segments <- as.data.frame(cbind(segment_ID, # segment identity (focal vs non)
                                      segment_n, # segment number
                                      segment_vert_lef, # left bound
                                      segment_vert_rig)) # right bound
      
      ## 2.4 For each segment estimate the abs & standarized SPL and gdist to 45dB ----
        
      # 2.4.0 Add new empty columns to dataframe to be filled
      newcols <- c("abs_SPL_2000", "abs_SPL_2000dB", "maxGdist", "meanGdist_45dB")
      segments <-  cbind(segments, setNames( lapply(newcols, function(x) x = NA), newcols) ) 
      
      # 2.6. Loop through segments and calculate integrated SPL in each
      for (c in 1:nrow(segments)) {
        
        if (segment_vert_lef[c] > 0 & segment_vert_rig[c] < 0) {
          X2_1 <- X2 %>%
            filter(X2$baz_converted >= segment_vert_lef[c]  &
                     X2$baz_converted <= 180)
          X2_2 <- X2 %>%
            filter(X2$baz_converted <= segment_vert_rig[c]  &
                     X2$baz_converted >= -180)
          
          newX <- rbind(X2_1, X2_2)
          
          abs_SPL_2000dB <- newX %>%
            filter(Gdist <= transectlength) %>%
            summarise(x = 10 * log10(sum(SPL_Pa) / (Pref ^ 2)))
          segments$abs_SPL_2000dB[c] <- as.numeric(abs_SPL_2000dB)
          
        } else {
          
          abs_SPL_2000dB <- X2 %>%
            filter( X2$baz_converted >= segment_vert_lef[c]  &
                      X2$baz_converted <= segment_vert_rig[c] &
                      Gdist <= transectlength ) %>%
            summarise(x = 10 * log10(sum(SPL_Pa) / (Pref ^ 2)))
          segments$abs_SPL_2000dB[c] <- as.numeric(abs_SPL_2000dB)
          
        }
      }
      
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
      segments$max_dist = gps_2013_ID1$max_dist[i]
      segments$per_trav_dist = gps_2013_ID1$per_trav_dist[i]
      segments$per_trip_time = gps_2013_ID1$Trip_time[i] * 100
      segments$Trip_state = gps_2013_ID1$Trip_state[i]
      segments$WindSp = gps_2013_ID1$WindSp[i] # wind speed
      segments$WindDir = gps_2013_ID1$WindDir[i] # wind direction
      segments$Dev.wind2 = gps_2013_ID1$Dev.wind2[i] # wind direction relative to track direction with directionality removed i.e. 0 to 180 rather than -180 to 180)
      
        
      ## 2.5 Find wind direction for each segment  -------------------------------
        
      segment_no <- 360 / segmentaperture
      angDiffs <- c(seq(0, 180, by = segmentaperture),
                    seq((-180 + segmentaperture),-segmentaperture, segmentaperture))
      segments$segment_vert_lef.DIFF <- rep(angDiffs, nrow(segments) / segmentno)
      
      # 2.5.1 Calculate relative wind direction adjusted for segment and convert to c(-180, 180)
      segments$relDir_adj <- segments$Dev.wind2 + segments$segment_vert_lef.DIFF
      segments$relDir_adj.bearing <-  abs(ifelse(segments$relDir_adj > 180, -360 + segments$relDir_adj, 
                                                segments$relDir_adj))
        
      # 2.5.2 Remove unnecessary columns
      segments$segment_vert_lef.DIFF <- NULL
      segments$relDir_adj <- NULL
      segments$relDir <- NULL
       
      }
      
      if (x == 1 & i == 1) { GPS_ID_segments = segments
      
      }else{
        GPS_ID_segments = rbind(GPS_ID_segments, segments)
      }
      
    }
    print(x)
    
  }
  tik <- Sys.time()
  
  tik-tok
  
  ## Process output 
  GPS_ID_segments <- subset(GPS_ID_segments, !is.na(segment_ID))
  GPS_ID_segments <- rename(GPS_ID_segments, relDir = relDir_adj.bearing)
  GPS_ID_segments$pointID <- paste(GPS_ID_segments$TripID, GPS_ID_segments$counter, sep = ".")
  GPS_ID_segments$segment_ID <- abs(GPS_ID_segments$segment_ID - 1)
  
  ## Write to csv
  write.csv(GPS_ID_segments, paste0("./Data_inputs/WAAL_2013_GPS_segmentAperture_", segmentaperture, "deg.csv"), row.names = F)
  
}


### 3.0 Fit best models to each aperture (15, 60, 90) --------------------------

# 3.0.0 Define apertures and set list
apertures <- c(15, 30, 60, 90)
model_list <- vector(mode = "list", length = length(apertures))

for (i in 1:length(apertures)) {

  # 3.0.1 Set segment aperture
  segmentaperture <- apertures[i]
  
  # 3.0.2 Load the data
  modDat <- data.table::fread(paste0("./Data_inputs/WAAL_2013_GPS_segmentAperture_", apertures[i], "deg.csv"), 
                  stringsAsFactors = F, data.table = F)

  # Remove infinite/NA values
  modDat <- subset(modDat, !is.na(abs_SPL_2000dB))
  modDat <- subset(modDat, !is.infinite(abs_SPL_2000dB))
  
  # 3.0.3 Rename and process variables
  names(modDat)[1] <- "case"
  
  modDat$birdID <- modDat$birdID %>%
    strsplit( "_" ) %>%
    sapply( "[", 1 )
  
  factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
  modDat[factor_vars] <- lapply(modDat[factor_vars], factor)
  
  # 3.0.4 Scale continuous variables
  cont_vars <- c("abs_SPL_2000dB", "WindSp", "relDir")
  modDat[cont_vars] <- lapply(modDat[cont_vars], function(x) c(scale(x, center = TRUE, scale = TRUE)))
  
  # 3.0.5 Separate males and females
  modDat.F <- subset(modDat, Sex == "F")
  modDat.M <- subset(modDat, Sex == "M")
  
  ## 3.1 Set up the models -----------------------------------------------------
  RSF_mod.F <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.F)
  
  RSF_mod.M <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + relDir:WindSp +
                      strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate',  data = modDat.M)
  
  ## 3.2 Get effect sizes and p values -----------------------------------------
  
  # 3.2.0 FEMALES
  covariates.F <- rownames(data.frame(summary(RSF_mod.F)$coefficients))
  coefs.F <- data.frame(summary(RSF_mod.F)$coefficients)$exp.coef.
  pvalues.F <- data.frame(summary(RSF_mod.F)$coefficients)$Pr...z..
  
  output.F <- data.frame(cov = covariates.F, coefs = coefs.F, pvalues = pvalues.F, aperture = segmentaperture, sex = "F")
  
  # 3.2.1 MALES
  covariates.M <- rownames(data.frame(summary(RSF_mod.M)$coefficients))
  coefs.M <- data.frame(summary(RSF_mod.M)$coefficients)$exp.coef.
  pvalues.M <- data.frame(summary(RSF_mod.M)$coefficients)$Pr...z..
  
  output.M <- data.frame(cov = covariates.M, coefs = coefs.M, pvalues = pvalues.M, aperture = segmentaperture, sex = "M")
  
  output <- rbind(output.F, output.M)
  model_list[[i]] <- output
  
}

## 3.3 Bind up the output ------------------------------------------------------
sensitivity_output.coefs <- do.call("rbind", model_list)


### 4.0 Plot the results -------------------------------------------------------

# 4.0.0 Make a plotting dataframe
plotDat <- sensitivity_output.coefs

#### FIGURE S3 - Plot of effect size output ------------------------------------
png(filename = "Figures/FIGS3_sensitivity-aperture-coefs.png", width = 9, height = 7, units = "in", res = 600)
ggplot(aes(x = aperture, y = coefs, group = cov, col = cov), data = plotDat) + 
  geom_point() + geom_line() +
  geom_hline(yintercept = 1, lty = "dashed") +
  facet_grid(.~sex) + 
  labs(y = "Effect size", x = "Aperture size (degrees)", col = "Covariates") +
  scale_colour_viridis_d(labels = c("SPL","SPL:windDir","SPL:windSp", "windDir", "windDir:windSp")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))
dev.off()
