### AIM:
### Get instantaneous wind speed for decision point - same for each cone
### Get wind conditions:
###                     - Instantaneous wind speed same for all cones
###                     - Relative wind direction averaged over bout of travel and
###                       adjusted for each cone   

library(dplyr)

# Get wind speed and direction for start of travel bouts ------------------

### Load 'gps_2013Trav20.csv' : processed file giving travel bouts > 20km
boutData <- data.table::fread('Data_inputs/gps_2013Trav20.csv', stringsAsFactors = F, data.table = F)
boutData <- boutData[order(boutData$TripID, boutData$DateTime),]


###-  1) calculate average wind direction for entire bout (relWind.avg)
###-  2) isolate first point in trip

boutData_first <- boutData %>% group_by(TripID, travbout) %>%
  mutate(relDir = Dev.wind2) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  data.frame()


# Match bout and cone data based on TripID variable -----------------------

### Load cone data : 'GPS_ID_cones_full_database.csv' includes all cone data, no environmental
#conesData <- data.table::fread("Data_inputs/GPS_ID_cones_full_database.csv", stringsAsFactors = F, data.table = F) # original
aperture <- 30
conesData <- data.table::fread(paste0("Data_inputs/GPS_ID_cones", aperture, "deg_full_database.csv"), stringsAsFactors = F, data.table = F) # variable aperture
conesData$WindSp <- NULL

conesData <- conesData[order(conesData$TripID, conesData$mapID),]

# Remove rows with no cone data
conesData <- subset(conesData, !is.na(cone_ID))

### Match datasets by time 
#   - 1) round boutData_first datetime to nearest hour
boutData_first$mapID <- format(round(boutData_first$DateTime, units="hours"), format="%Y-%m-%d %H:%M")   

#   - 2) isolate important columns
boutData_first <- boutData_first[,c(19,27,52,54,55),] #trip ID, WindSp, travbout, relWind_average, mapID

#   - 3) merge on time_nearest
conesData$TripID <- as.character(conesData$TripID)
boutData_first$TripID <- as.character(boutData_first$TripID)
conesBouts <- merge(conesData, boutData_first, by = c("TripID", "mapID"), all.x = TRUE)



# Calculate relative wind for each cone -----------------------------------

### Add column for angular difference between each cone (for calculations)

cone_no <- 360/aperture
angDiffs <- c(seq(0, 180, by = aperture), seq((-180+aperture), -aperture, aperture))
conesBouts <- conesBouts[order(conesBouts$TripID, conesBouts$counter, conesBouts$cone_n),]
conesBouts$cone_vert_lef.DIFF <- rep(angDiffs, nrow(conesBouts)/cone_no)

### Calculate relative wind direction adjusted for cone and convert to c(-180, 180)

conesBouts <- conesBouts %>% group_by(TripID, counter) %>%
                             mutate(relDir_adj = relDir + cone_vert_lef.DIFF,
                                    relDir_adj.bearing = abs(ifelse(relDir_adj > 180, -360 + relDir_adj, 
                                                                    relDir_adj))) %>%
                             data.frame()


### Remove some unnecessary columns

conesBouts$cone_vert_lef.DIFF <- NULL
conesBouts$relDir_adj <- NULL
conesBouts$relDir <- NULL


#write.csv(conesBouts, "gps_2013_cones_with_wind.csv", row.names = F)


# Isolate and process variables for modelling -----------------------------------------

# tripID, coneID, abs_SPL_2000db, birdID, counter, Sex, TripState, WindSp, relDir_adj.bearing

#modDat <- conesBouts[,c(1,5, 11, 14, 15, 16, 24, 25, 27)] # original
modDat <- conesBouts[,c(1,3, 9, 12, 13, 14, 17, 30, 32)] # variable aperture
names(modDat)[9] <- "relDir"
names(modDat)[7] <- "Trip_state"

## Encode focal cone as '1' and non-focal as '0'
modDat$cone_ID <- abs(1-modDat$cone_ID)

## Remove NA and infinite values
modDat <- subset(modDat, !is.na(abs_SPL_2000dB) & !is.na(Sex) &
                               !is.na(Trip_state) & !is.na(TripID) &
                               !is.na(cone_ID))

modDat <- subset(modDat, !is.infinite(abs_SPL_2000dB))

## Make decision point ID
modDat$pointID <- paste(modDat$TripID, modDat$counter, sep = ".")
modDat$counter <- NULL

## Output dataset
write.csv(modDat, paste0("Data_inputs/GPS_2013_model-data-APERTURE", aperture, ".csv"), row.names = F)

