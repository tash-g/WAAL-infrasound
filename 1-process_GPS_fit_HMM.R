
## ---------------------------
##
## Script name: 1-process_GPS_fit_HMM
##
## Purpose of script: This script performs the initial GPS processing, applies an 
## HMM to identify behaviour, and labels trip stage
##
## Author: Dr. Lucia Martina Martin Lopez (with support by Dr Natasha Gillies)
##
## Email: gilliesne@gmail.com
##
## ---------------------------


### 0.0 Load the packages ------------------------------------------------------

# 0.0.0 Define the packages
packages <- c("dplyr", "momentuHMM", "ggplot2", "plotly", "ggpubr", "magrittr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))


## 0.1 Create outputs folder if one does not exist -----------------------------

out.path <- "./Data_outputs/"

if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
}

## 0.2 Load the data -----------------------------------------------------------

## This dataset contains the longest trip for each bird. Distance of each fix to
## Crozet shelf has been calculated, and very short trips to the shelf at the start 
## and end of foraging trips have been removed.

gps_2013 <- read.csv("Data_inputs/WAAL_2013_gps_filtered.csv", stringsAsFactors = F)

# 0.2.1 Make ID a factor
gps_2013$BirdID <- as.factor(gps_2013$BirdID)
length(unique(gps_2013$BirdID)) # 89 Trips for 89 Individuals



### 1.0 Fit an HMM to the data -------------------------------------------------

## 1.1 Prepare the data for HMM  -----------------------------------------------

# 1.1.0 Make ID column for prepData function
gps_2013$ID <- gps_2013$TripID

# 1.1.1 Make dataset ready for HMM
hmm_data <- prepData(gps_2013,
                     type = "LL", # longs and lats
                     coordNames = c("Longitude", "Latitude")) 

head(hmm_data)

# Remove step lengths > 25 - unrealistic speed (45 fixes) 
hmm_data <- hmm_data %>%
            filter(!step > 25) 

# Remove NA angles (141 fixes)
hmm_data <- hmm_data %>%
            filter(!is.na(angle))

### NOTE: Initial values taken from Clay et al. 2020, J. Anim. Ecol.

# 1.1.1 Assign step lengths 
shape_0 <- c(12.46, 3.95, 0.34)
scale_0 <- c(3.734, 4.44, 0.19) 

# 1.1.2 Set zero values to small numbers
ind_zero <- which(hmm_data$step == 0)
if (length(ind_zero) > 0) {
  hmm_data$step[ind_zero] <- runif(length(ind_zero)) / 10000
}
ind_zero <- which(hmm_data$step == "NA")
if (length(ind_zero) > 0) {
  hmm_data$step[ind_zero] <- runif(length(ind_zero)) / 10000
}

stepPar0 <- c(shape_0,scale_0)

# 1.2.3 Assign turning angles 
location_0 <- c(0.0033,-0.016, 0.03)
concentration_0 <- c(47.15,  1.16,  39.00)

anglePar0 <- c(location_0, concentration_0)


## 1.2 Fit the HMM  ------------------------------------------------------------

stateNames <- c("travel", "search", "rest")

m1 <- fitHMM(
  data = hmm_data,
  nbStates = 3,
  dist = list(step = "gamma", angle = "vm"),
  Par0 = list(step = stepPar0, angle = anglePar0),
  estAngleMean = list(angle = TRUE),
  stateNames = stateNames
)

plotPR(m1) 

# 1.2.1 Store model as an .rdata object so you don't have to run from scratch each time
file.out <- paste0("Data_outputs/WAAL_2013_HMM.RData")
#save(m1, file = file.out)

#load(file.out)



### 2.0 Assign behavioural states  ---------------------------------------------

## 2.1 Compute most probable states using viterbi algorithm---------------------
hmm_data_out <- m1$data
hmm_data_out$State <- viterbi(m1)

## 2.2 Assign behaviours  ------------------------------------------------------

# 2.2.0 Assess step/angle distributions
ggplot(aes(x = step, fill = as.factor(State)), data = hmm_data_out) + geom_histogram(alpha = 0.5)
ggplot(aes(x = angle, fill = as.factor(State)), data = hmm_data_out) + geom_histogram(alpha = 0.5)

# 2.2.1 Label each state and check classification
hmm_data_out$State[hmm_data_out$State == 1] <- "Travel"
hmm_data_out$State[hmm_data_out$State == 2] <- "Search"
hmm_data_out$State[hmm_data_out$State == 3] <- "Rest"

# 2.2.2 Check states
table(hmm_data_out$State)

#  Rest Search Travel 
# 23904  28594  32727 

# 2.2.3 Calculate percentage time spent in each state 
hmm_data_out %>%
  group_by(State) %>%
  summarize(counts = n()) %>%
  mutate(per = counts / sum(counts) * 100) %>%
  collect()

#  State  counts   per
#  <chr>   <int> <dbl>
#1 Rest    23904  28.0
#2 Search  28594  33.6
#3 Travel  32727  38.4



### 4.0 Output the data ---------------------------------------------------------------------

# 4.0.0 Make a counter variable (used in processing in script 2)
hmm_data_out$ID <- as.factor(as.character(hmm_data_out$ID))

hmm_data_out %<>% group_by(ID) %>%
  mutate(counter = row_number(ID)) %>%
  data.frame()

# 4.0.1 Rename x and y columns
hmm_data_out <- rename(hmm_data_out, x_lon = x, y_lat = y)

# 4.0.2 Remove columns used for processing
cols.rm <- c("ID", "step", "angle")
hmm_data_out[,cols.rm] <- NULL

# 4.0.3 Check encoding of variables
hmm_data_out$BirdID <- as.factor(as.character(hmm_data_out$BirdID))
hmm_data_out$TripID <- as.factor(as.character(hmm_data_out$TripID))
factor_vars <- c("Sex", "State")
hmm_data_out[factor_vars] <- lapply(hmm_data_out[factor_vars], factor)
hmm_data_out$DateTime <- as.POSIXct(hmm_data_out$DateTime, format = "%Y-%m-%d %H:%M:%S")

# 4.0.4 Write the CSV
write.csv(hmm_data_out, "Data_inputs/WAAL_2013_gps_labelled.csv", row.names = F)

