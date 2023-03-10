## ---------------------------
##
## Script name: SUPP-MAT_sensitivity_analysis_apertures.R
##
## Purpose of script: This script varies the segment apertures between 15 and 90
## degrees, and assess how this influences the covariate output. Section 7 plots
## SPL differences within decision points
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

## 0.0.0 Specify the packages
packages <- c("dplyr", "ncdf4", "lubridate", "birk", "ggplot2", "survival", "ggpubr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))

# 0.0.2 Specify deg2rad function
deg2rad <- function(deg) {(deg * pi) / (180)}


### 1.0 Process the GPS data for segmentation ----------------------------------

## 1.0.0 Load the GPS data
gps_2013Trav20 <- read.csv('Data_inputs/WAAL_2013_gps_Trav20.csv', stringsAsFactors = F)

# 1.0.1 Isolate first point of bout
gps_2013Trav20_1stpoint <- gps_2013Trav20 %>% 
                           group_by(BirdID, travbout) %>%
                           slice_head(n = 1) %>% 
                           ungroup()

# 1.0.1 Point to soundscape data 
path_to_IS_maps <- "E:/Soundscapes/"
IS_folder_maps <- dir(path_to_IS_maps, pattern = "2013")
GPS_ID_segments <- list()

### 2.0 Loop through each aperture, match SPL map and calculate effect sizes ---

## 2.0.0 Define segment parameters ---------------------------------------------
apertures <- c(15, 30, 60, 90)
transectlength <- 2025

for (j in 1:length(apertures)) {
  
  # 2.0.1 Set the segment-specific parameters
  segmentaperture <- apertures[j]
  segmentno <- 360/segmentaperture
  angDiffs <-  c(seq(0, 180, by = segmentaperture),
                 seq((-180 + segmentaperture),-segmentaperture, segmentaperture))
  
  print(segmentaperture)
  
  ## 2.1 Match SPL data to GPS and split into segments -------------------------
  
  tok <- Sys.time()
  for (x in 1:length(IS_folder_maps)) {
    
    # 2.1.0 Point to IS map relevant to bird
    birdmapid <- IS_folder_maps[x]
    path_to_IS_files <- paste0(path_to_IS_maps, birdmapid, "/Interp/")
    
    # 2.1.1 Load IS files 
    IS_files <- list.files(path = path_to_IS_files)
    
    # 2.1.2 Extract bird ID from the folder name
    IDbirdmap <- sapply(strsplit(IS_folder_maps[x], "_", fixed = TRUE),
                        function(i) paste(head(i, -1), collapse = "_"))
    
    if (grepl("_", IDbirdmap, fixed = TRUE) == FALSE) { IDbirdmap <- paste0(IDbirdmap, "_1")}
    
    # Change underscore to '.'
    IDbirdmap <- gsub("_", ".", IDbirdmap)
    
    # 2.1.3 Select the bird trip for which the soundscape is being loaded
    gps_2013_ID1 <- gps_2013Trav20_1stpoint %>% 
      filter(TripID == IDbirdmap)
    
    # If there are no GPS data for that map, skip
    if (nrow(gps_2013_ID1) == 0) next
    
    # Check data formatting
    gps_2013_ID1$TripID <- as.factor(as.character(gps_2013_ID1$TripID))
    gps_2013_ID1$DateTime <- as.POSIXct(gps_2013_ID1$DateTime, tz = "UTC")
    
    
    # 2.1.4 Get closest hour for each GPS fix to match with soundscapes 
    gps_2013_ID1$maptobe <-
      format(round(gps_2013_ID1$DateTime, units = "hours"), format = "%Y-%m-%d %H:%M")
    
    
## 2.2 Match each GPS fix to the closest SPL map in time and compute segments ----
    
    for (i in 1:nrow(gps_2013_ID1)) {
      
      # 2.2.0 Find the closest hour for each GPS point in the IS_files
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
      
      # If no map is found then fill it with NAs.
      if (length(IDX) == 0) {
        
      # 2.2.1 Fill empty map with NAs
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
        
      ## 2.3 Within each soundscape map divide the area into segments depending on aperture size  ----
      ## Get the baz angles for the segments starting from the left side of
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
      if (focal_segment_deg[1] <= (-180 + segmentaperture)) {
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
        
      # 2.4.0 Add new empty column to dataframe to be filled
      segments$abs_SPL_2000dB <- NA
      segments$abs_SPL_2000 <- NA
      
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
      
        
      ## 2.5 Find wind direction for each segment  -----------------------------
        
      # 2.5.0 Find angular differences between each segment 
      segments$segment_vert_lef.DIFF <- rep(angDiffs, nrow(segments) / segmentno)
      
      # 2.5.1 Calculate relative wind direction adjusted for segment and convert to c(-180, 180)
      segments$relDir_adj <- segments$Dev.wind2 + segments$segment_vert_lef.DIFF
      segments$relDir_adj.bearing <-  abs(ifelse(segments$relDir_adj > 180, -360 + segments$relDir_adj, 
                                                 segments$relDir_adj))
      
      # 2.5.2 Remove columns used for calculation
      segments$segment_vert_lef.DIFF <- NULL
      segments$relDir_adj <- NULL
      segments$relDir <- NULL
      
      }
      
      if (x == 1 & i == 1) { 
        GPS_ID_segments = segments
      } else {
        GPS_ID_segments = rbind(GPS_ID_segments, segments)
      }
      
    }
  
    print(x) 
  }
  tik <- Sys.time()
  
  tik - tok
  
  ## 2.6 Process output --------------------------------------------------------
  
  # 2.6.0 Remove NA segments
  GPS_ID_segments <- subset(GPS_ID_segments, !is.na(segment_ID))
  # 2.6.1 Rename relDir column
  GPS_ID_segments <- rename(GPS_ID_segments, relDir = relDir_adj.bearing)
  # 2.6.2 Make (decision) point ID variable
  GPS_ID_segments$pointID <- paste(GPS_ID_segments$TripID, GPS_ID_segments$counter, sep = ".")
  # 2.6.3 Make focal segment '1' and non-focal '0'
  GPS_ID_segments$segment_ID <- abs(GPS_ID_segments$segment_ID - 1)
  # 2.6.4 Remove columns not relevant for analysis
  cols_rm <- c("segment_vert_lef", "segment_vert_rig", "mapID", "counter", "Dev.wind2")
  GPS_ID_segments[,cols_rm] <- NULL
  
  ## 2.7 Write to csv ----------------------------------------------------------
  write.csv(GPS_ID_segments, paste0("./Data_inputs/WAAL_2013_GPS_segmentAperture_", segmentaperture, "deg.csv"), row.names = F)
  
}


### 3.0 For each aperture, run models and calculate effect sizes ---------------

## 3.0.0 Define apertures and set list
apertures <- c(15, 30, 60, 90)
model_output <- vector(mode = "list", length = length(apertures))

for (i in 1:length(apertures)) {
  
  # 3.0.1 Set segment aperture
  segmentaperture <- apertures[i]
  
  # 3.0.2 Load the data
  modDat <- data.table::fread(paste0("./Data_inputs/WAAL_2013_GPS_segmentAperture_", apertures[i], "deg.csv"), 
                              stringsAsFactors = F, data.table = F)
  
  # 3.0.3 Remove infinite/NA values
  modDat <- subset(modDat, !is.na(abs_SPL_2000_std))
  modDat <- subset(modDat, !is.infinite(abs_SPL_2000_std))
 
  # 3.0.4 Rename and process variables
  names(modDat)[1] <- "case"
  
  modDat$birdID <- modDat$birdID %>%
    strsplit( "_" ) %>%
    sapply( "[", 1 )
  
  factor_vars <- c("TripID", "birdID", "Sex", "pointID")
  modDat[factor_vars] <- lapply(modDat[factor_vars], factor)
  
  
# 4.0 Fit model to aperture data -----------------------------------------------
  
  # 4.0.0 Separate males and females
  modDat.F <- subset(modDat, Sex == "F")
  modDat.F <- droplevels(modDat.F)
  modDat.M <- subset(modDat, Sex == "M")
  modDat.M <- droplevels(modDat.M)
  
  # 4.0.1 Scale continuous variables
  cont_vars <- c("WindSp", "relDir")
  
  modDat.F[, cont_vars] <-
    lapply(modDat.F[, cont_vars], function(x)
      c(scale(x, center = TRUE, scale = TRUE)))
 
  modDat.M[, cont_vars] <-
    lapply(modDat.M[, cont_vars], function(x)
      c(scale(x, center = TRUE, scale = TRUE)))
  
  ## 4.1 Set up the models -----------------------------------------------------
  
  RSF_mod.F <- clogit(case ~ abs_SPL_2000_std*relDir + abs_SPL_2000_std:WindSp + relDir:WindSp +
                      strata(pointID), cluster = birdID, 
                    robust = TRUE, method = 'approximate', data = modDat.F)
  
  RSF_mod.M <- clogit(case ~ abs_SPL_2000_std*relDir + abs_SPL_2000_std:WindSp + relDir:WindSp +
                      strata(pointID), cluster = birdID, 
                    robust = TRUE, method = 'approximate',  data = modDat.M)
  

  ## 4.2 Get effect sizes  -----------------------------------------------------
  
  # 4.2.0 FEMALES
  covariates.F <- rownames(data.frame(summary(RSF_mod.F)$coefficients))
  coefs.F <- data.frame(summary(RSF_mod.F)$coefficients)$exp.coef.
  se.F <- data.frame(summary(RSF_mod.F)$coefficients)$se.coef.
  
  output.F <- data.frame(cov = covariates.F, coefs = coefs.F, se = se.F, aperture = segmentaperture, sex = "F")
  
  # 4.2.1 MALES
  covariates.M <- rownames(data.frame(summary(RSF_mod.M)$coefficients))
  coefs.M <- data.frame(summary(RSF_mod.M)$coefficients)$exp.coef.
  se.M <- data.frame(summary(RSF_mod.M)$coefficients)$se.coef.

  output.M <- data.frame(cov = covariates.M, coefs = coefs.M, se = se.M, aperture = segmentaperture, sex = "M")
  
  output <- rbind(output.F, output.M)
  model_output[[i]] <- output
  
}


### 5.0 Bind up output ---------------------------------------------------------

sensitivity_output.coefs <- do.call("rbind", model_output)


### 6.0 Plot the results -------------------------------------------------------

# 6.0.0 Make a plotting dataframe for effect sizes
plotDat <- sensitivity_output.coefs

## 6.1 Make an effect size plot ------------------------------------------------

effects_plot <- ggplot(aes(x = aperture, y = coefs, group = cov, col = cov), data = plotDat) + 
  geom_point(aes(shape = cov), size = 5) + 
  geom_errorbar(aes(ymin = coefs - se, ymax = coefs + se), width = 5) +
  geom_line(linetype = "dotted", size = 2) +
  geom_hline(yintercept = 1, lty = "dashed") +
  facet_grid(.~sex) + 
  labs(y = "Odds ratio", x = "Aperture size (degrees)") +
  scale_colour_viridis_d(name = "Covariates", 
                         labels = c("SP","SP:windDir", "SP:windSp","windDir", "windDir:windSp")) +
  scale_shape_manual(name = "Covariates", 
                     labels = c("SP","SP:windDir", "SP:windSp","windDir", "windDir:windSp"),
                     values = c(15, 16, 17, 18, 19)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.position = c(0.15,0.9),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.tag = element_text(size = 22))


#### FIGURE S2 - Plot of sensitivity analysis ----------------------------------
tiff(filename = "Figures/FIGS3_sensitivity-aperture-coefs.tif", width = 10, height = 10, units = "in", res = 600)
effects_plot
dev.off()

### 7.0 Plot SPL differences by decision point ---------------------------------

# 7.0.0 Load the relevant data
modDat <- data.table::fread("Data_inputs/WAAL_2013_gps_processed_aperture60deg.csv", 
                            data.table = F)

# 7.0.1 Rename and process variables
modDat <- rename(modDat, case = segment_ID)

factor_vars <- c("TripID", "birdID", "Sex", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# 7.0.2 Remove NA variables
modDat <- modDat[!is.na(modDat$abs_SPL_2000_std),]

## 7.1 Calculate SPL differences in PA within segments -------------------------

diffs <- modDat %>% 
  group_by(TripID, pointID) %>%
  mutate(diffs = abs_SPL_2000[case == 1] - abs_SPL_2000,
         maxMarker = ifelse(diffs == max(diffs), "max", "not"),
         focalVal = abs_SPL_2000[case == 1]) %>%
  filter(maxMarker == "max" & case == 0) %>%
  data.frame()

# 7.1.1 Convert pressure values to decibels
diffs$focal_dB <- 20*log10(diffs$focalVal/0.00002)
diffs$vals_dB <- 20*log10(diffs$abs_SPL_2000/0.00002)
diffs$delta_dB <- diffs$focal_dB - diffs$vals_dB



#### FIGURE S4  - SPL differences focal vs non-focal ---------------------------

diffs$Sex <- ifelse(diffs$Sex == "F", "Females", "Males")

SPL_diffs.hist <- ggplot(data = diffs, aes(x = delta_dB)) + 
  facet_grid(.~Sex) + 
  geom_histogram(bins = 30, boundary = 0) + 
  geom_vline(aes(xintercept = mean(delta_dB), group = Sex, col = "red"),  size = 1) +
  labs(x = "Max(SPL Focal segment - Non-focal segment) dB", 
       y = "Frequency", tag = "(a)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.tag = element_text(size = 18),
        legend.position = "none")

diffs$pointID_numeric <- as.numeric(sapply(stringr::str_split(diffs$pointID, "[.]"), tail, 1))

SPL_diffs.scatter <- ggplot(data = diffs, aes(x = pointID_numeric, y = delta_dB)) + 
  geom_point() + 
  facet_grid(.~Sex) + 
  labs(y = "Max(SPL Focal segment - Non-focal segment) dB", 
       x = "Decision point index", tag = "(b)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.title.y = element_text(size = 18))


tiff(filename = "Figures/FIGS5_SPL-differences.tif", width = 18, height = 9, units = "in", res = 600)
ggarrange(SPL_diffs.hist, SPL_diffs.scatter, 
          ncol = 2)
dev.off()

