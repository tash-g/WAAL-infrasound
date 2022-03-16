#'*AIM: Compare manually classified behaviour with HMM output to determine accuracy.*


# Load packages -----------------------------------------------------------

# Packages
packages <- c("data.table", "caret")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

#  Load data --------------------------------------------------------------

## Load GPS data containing HMM behavioural classifications

hmm_data <- read.csv("./Data_inputs/WAAL_GPS_2013_HMM-classification.csv")

# Extract relevant columns, rename state column, format datetime
hmm_data <- hmm_data[,c(18, 21, 23, 39)] # BirdID, Ring, Sex, DateTime, State
names(hmm_data)[4] <- "state_hmm"
hmm_data$DateTime <- as.POSIXct(hmm_data$DateTime, format = "%Y-%m-%d %H:%M:%S")

## Load validation data
manual_data <- read.csv("./Data_inputs/WAAL_GPS_2013_manualStates_anon.csv")

manual_data$STATE[manual_data$STATE == 1] <- "Travel"
manual_data$STATE[manual_data$STATE == 2] <- "Search"
manual_data$STATE[manual_data$STATE == 3] <- "Rest"

# Rename state column, format datetime
names(manual_data)[7] <- "state_manual"
manual_data$DateTime <- as.POSIXct(manual_data$DateTime, format = "%d/%m/%Y %H:%M")


# Merge the dataframes for validation -------------------------------------

## Merge to nearest minute
setkey(setDT(hmm_data), "BirdId", "DateTime")
setkey(setDT(manual_data), "BirdId", "DateTime")
gps_comp <- as.data.frame(hmm_data[manual_data, roll = "nearest"])

### Compute accuracy

# Convert to factors
gps_comp[,c(4,10)] <- lapply(gps_comp[,c(4,10)], as.factor)

# Compute confusion matrix
confusionMatrix(gps_comp$state_hmm, gps_comp$state_manual)

tab <- table(gps_comp$state_hmm, gps_comp$state_manual)
tab <- tab / rowSums(tab)

