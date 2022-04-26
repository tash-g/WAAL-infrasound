
## ---------------------------
##
## Script name: 1-process_GPS_fit_HMM
##
## Purpose of script: This script performs the initial GPS processing, applies an 
## HMM to identify behaviour, and labels trip stage
##
## Author: Dr. Lucia Martina Martin Lopez (with support by Dr Natasha Gillies)
##
## Date Created: 2021-03-16
##
## Email: gilliesne@gmail.com
##
## ---------------------------


### 0.0 Load the packages ------------------------------------------------------

# Packages
packages <- c("dplyr", "momentuHMM", "ggplot2", "plotly", "ggpubr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
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

# Make ID a factor
gps_2013$BirdId <- as.factor(gps_2013$BirdId)
length(unique(gps_2013$BirdId)) # 89 Trips for 89 Individuals


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

# Remove NA angles
hmm_data <- hmm_data %>%
            filter(!is.na(angle))

### NOTE: Initial values taken from Clay et al. 2020

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

# Store model as an .rdata object so you don't have to run from scratch each time
file.out <- paste0("Data_outputs/WAAL_2013_HMM.RData")
#save(m1, file = file.out)

load(file.out)


### 2.0 Assign behavioural states  ---------------------------------------------

## 2.1 Compute most probable states using viterbi algorithm---------------------
hmm_data_out <- m1$data
hmm_data_out$State <- viterbi(m1)

## 2.2 Assign behaviours  ------------------------------------------------------

# 2.2.1 Assess step/angle distributions
ggplot(aes(x = step, fill = State), data = hmm_data_out) + geom_histogram(alpha = 0.5)
ggplot(aes(x = angle, fill = State), data = hmm_data_out) + geom_histogram(alpha = 0.5)

# 2.2.2 Label each state and check classification
hmm_data_out$State[hmm_data_out$State == 1] <- "Travel"
hmm_data_out$State[hmm_data_out$State == 2] <- "Search"
hmm_data_out$State[hmm_data_out$State == 3] <- "Rest"

# 2.2.3 Check states
table(hmm_data_out$State)

#Rest Search Travel 
#23888  28650  32828

# 2.2.4 Calculate percentage time spent in each state 
hmm_data_out %>%
  group_by(State) %>%
  summarize(counts = n()) %>%
  mutate(per = counts / sum(counts) * 100) %>%
  collect()

#State  counts   per
#<chr>   <int> <dbl>
#1 Rest    23888  28.0
#2 Search  28650  33.6
#3 Travel  32828  38.5


### 3.0 Identify trip stage ----------------------------------------------------

#### Identify whether bird at outbound, middle, or inbound part of trip

## 3.1 Calculate trip metrics --------------------------------------------------

hmm_data_out$ID <- as.factor(as.character(hmm_data_out$ID))

## Distance traveled since the bird left the colony, 
hmm_data_out <- hmm_data_out %>%
                group_by(ID) %>%
  # 3.1.1 Cumulative distance since leaving colony
                mutate(cum_trav_dist = cumsum(DistTrav),
  # 3.1.2 Percentage distance covered
                       per_trav_dist = cum_trav_dist*100/max(cum_trav_dist),
  # 3.1.3 Index each fix within a trip 
                       counter = row_number(BirdId),
  # 3.1.4 Total trip time in minutes
                       Trip_time =  counter * 15 / max(counter * 15)) %>%
                data.frame()
  
#### Determining trip stage (outbound, middle, inbound) based on methods by 
#### Wakefield et al. 2009: performed at population level rather than individual.

## 3.2. Calculate distance from the colony as the proportion of the max distance ----
## reached during that trip (dcol/dmax) and the time elapsed as a proportion 
## of the total trip time (t/tmax)

hmm_data_out <- hmm_data_out %>%
                group_by(ID) %>%
                mutate(dcol_dmax = Dist_cro_shelf/max(Dist_cro_shelf))


## 3.3. The total variance in dcol/dmax for all locations occurring before t/tmax ----
## is then plotted against t/tmax. The rate of change dcol/dmax with t/tmax is a 
## measure of the rate at which birds move relative to the colony. 
## Hence, by graphically examining the variance of dcol/dmax with t/tmax we were
## able to identify the value of t/tmax at which the birds typically ceased 
## commuting rapidly away from the colony and that at which they began commuting 
## rapidly back again. Locations lying between these two values were categorized 
## as the middle stage, and the remainder as the outward or return stages, as 
## appropriate.

# 3.3.1 Plot distance from colony as proportion of maximum distance from colony
FIGS2_A <- ggplot(data = hmm_data_out) +
  geom_point(aes(y = dcol_dmax, x = Trip_time), size = 0.1) +
  labs(y = bquote(~italic(d)[col]~"/"~italic(d)[max]), 
       x = bquote(~italic(t)~"/"~italic(t)[max]),
       tag = "(a)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.tag = element_text(size = 22)) 


# 3.3.2 Calculate variance in dcol/dmax for all locations occuring before t/tmax
# (var_asc) and after t/tmax (var_desc)
var_asc <- c()
var_desc <- c()

for (i in 1:length(seq(0, 1, length.out = 50))) {
  var_asc[i] <-
    hmm_data_out %>% ungroup() %>% filter(Trip_time < seq(0, 1, length.out = 50)[i]) %>%
    summarise(variance_dist = var(dcol_dmax))
  
  var_desc[i] <-
    hmm_data_out %>% ungroup() %>% filter(Trip_time > seq(0, 1, length.out =
                                                            50)[i]) %>%
    summarise(variance_dist = var(dcol_dmax))
  
}

var_asc <- unlist(var_asc)
var_desc <- unlist(var_desc)
idx <- seq(0, 1, length.out = 50)

# 3.3.3 Plot variance in d/dmax before and after t/tmax
FIGS2_B <- ggplot() +
  geom_point(aes(y = var_asc, x = idx)) +
  geom_point(aes(y = var_desc, x = idx, color = "red")) +
  geom_vline(xintercept = idx[17], linetype = "dashed") +
  geom_vline(xintercept = idx[35],
             linetype = "dashed",
             color = "red") +
  labs(y = bquote("Var ("~italic(d)[col]~"/"~italic(d)[max]~")"), 
       x = bquote(~italic(t)~"/"~italic(t)[max]),
       tag = "(b)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.tag = element_text(size = 22),
        legend.position = "none") 

#### FIGURE S2 - Plot to estimate trip stage -----------------------------------

png(filename = "Figures/FIGS2_trip_stage.png", width = 16, height = 7, units = "in", res = 900)
ggarrange(FIGS2_A, FIGS2_B, ncol = 2)
dev.off()

# 3.4 Classify trips as out, middle, or in ------------------------------------ 
hmm_data_out <- hmm_data_out %>%
                    mutate(Trip_state = case_when(Trip_time < idx[17] ~ 'out',
                                                  Trip_time > idx[17] &
                                                  Trip_time < idx[35] ~ 'mid',
                                                  Trip_time > idx[35] ~ 'in')) %>%
               data.frame()



### 4.0 Output the data ---------------------------------------------------------------------

# 4.0.1 Rename x and y columns
hmm_data_out <- rename(hmm.data.out, x_lon = x, y_lat = y)

# 4.0.2 Write the CSV
write.csv(hmm_data_out, "Data_inputs/WAAL_2013_gps_labelled.csv", row.names = F)

