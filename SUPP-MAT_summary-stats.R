## ---------------------------
##
## Script name: SUPP-MAT_summary-stats.R
##
## Purpose of script: This script produces the behaviour summaries from Table S2
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

# 0.0.0 Define packages
packages <- c("ggplot2", "dplyr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))


# 0.0.2 Define summary function
my.summary <- function(x) list(mean = mean(x), sd = sd(x))

# 0.0.3 Load boutFinder function
source("FUNCTION_boutFinder.R")


### 1.0 Label travel bouts and filter >20km ------------------------------------

# 1.0.0 Load the dataset
gps_2013 <- read.csv("Data_inputs/WAAL_2013_gps_labelled.csv", stringsAsFactors = F)

# 1.0.1 Index all rows in each trip for each bird
gps_2013Trav <- gps_2013 %>% 
  group_by(TripID) %>%
  mutate(idx = seq(1:n())) %>%
  # 1.1.2 Filter fixes labelled as 'travel'
  filter(State == "Travel") %>% 
  # 1.1.3 Label individual bouts
  mutate(travbout = boutFinder(idx)) %>% 
  data.frame()

gps_2013Trav$travbout <- as.factor(as.character(gps_2013Trav$travbout))

## 1.2 Estimate distance travelled in each bout & filter for travel bouts > 20km ------
gps_2013Trav20 <- gps_2013Trav %>%
  group_by(TripID, travbout) %>%
  mutate(TotdisttravBout = sum(DistTrav)) %>%
  filter(TotdisttravBout > 20) %>%
  data.frame()


### 2.0 Summarise trip metrics for entire trips --------------------------------

# 2.0.0 Summarise max distance from the colony, distance covered, trip duration
trip_metrics.full <- gps_2013 %>% 
                     group_by(TripID) %>%
                       summarise(sex = Sex[1],
                       max_dist_col = max(Dist_cro_shelf),
                       dist_covered = sum(DistTrav),
                       duration.hours = as.numeric(difftime(max(DateTime), min(DateTime), units = "hours"))) %>%
                     data.frame()

# 2.0.1 Get mean and sd for females and males
lapply(trip_metrics.full[trip_metrics.full$sex == "F",c(3:5)], my.summary)
lapply(trip_metrics.full[trip_metrics.full$sex == "M",c(3:5)], my.summary)


### FEMALES 

#$max_dist_col
#$max_dist_col$mean
#[1] 1083.125

#$max_dist_col$sd
#[1] 624.2486

#$dist_covered
#$dist_covered$mean
#[1] 5569.639

#$dist_covered$sd
#[1] 3549.249

#$duration.hours
#$duration.hours$mean
#[1] 238.245

#$duration.hours$sd
#[1] 116.2022

### MALES

#$max_dist_col
#$max_dist_col$mean
#[1] 1324.762

#$max_dist_col$sd
#[1] 914.4789

#$dist_covered
#$dist_covered$mean
#[1] 6138.454

#$dist_covered$sd
#[1] 3807.858

#$duration.hours
#$duration.hours$mean
#[1] 240.9231

#$duration.hours$sd
#[1] 93.0008

### 3.0 Summarise metrics for travel bouts (>20km) -----------------------------

# 3.0.0 Set POSIXct
gps_2013Trav20$DateTime <- as.POSIXct(gps_2013Trav20$DateTime, format = "%Y-%m-%d %H:%M:%S")

# 3.0.1 Summarise max distance from the colony, distance covered, trip duration
trip_metrics.trip <- gps_2013Trav20 %>% 
                     group_by(TripID, travbout) %>%
                     summarise(sex = Sex[1],
                               max_dist_col = max(Dist_cro_shelf),
                               dist_covered = sum(DistTrav),
                               duration.hours = as.numeric(difftime(max(DateTime), min(DateTime), units = "hours"))) %>%
                     data.frame()

lapply(trip_metrics.trip[trip_metrics.trip$sex == "F",c(4:6)], my.summary)
lapply(trip_metrics.trip[trip_metrics.trip$sex == "M",c(4:6)], my.summary)

### FEMALES
#$max_dist_col
#$max_dist_col$mean
#[1] 836.3435

#$max_dist_col$sd
#[1] 557.3695

#$dist_covered
#$dist_covered$mean
#[1] 110.6704

#$dist_covered$sd
#[1] 121.7914

#$duration.hours
#$duration.hours$mean
#[1] 2.107124

#$duration.hours$sd
#[1] 2.295277


### MALES
#$max_dist_col
#$max_dist_col$mean
#[1] 962.0647

#$max_dist_col$sd
#[1] 731.7829

#$dist_covered
#$dist_covered$mean
#[1] 138.8432

#$dist_covered$sd
#[1] 172.9571

#$duration.hours
#$duration.hours$mean
#[1] 2.461668
#
#$duration.hours$sd
#[1] 2.931283
