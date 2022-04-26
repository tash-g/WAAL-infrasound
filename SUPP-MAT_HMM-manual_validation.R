## ---------------------------
##
## Script name: SUPP-MAT_HMM_manual_validation.R
##
## Purpose of script: Compare the behavioural classification output of the HMM to
## manually labelled foraging trips
##
## Author: Dr. Natasha Gillies
##
## Date Created: 2022-03-16
##
## Email: gilliesne@gmail.com
##
## ---------------------------

### 0.0 Load packages -----------------------------------------------------------

# 0.0.1 Define the Packages
packages <- c("data.table", "caret", "dplyr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.2 Load packages
invisible(lapply(packages, library, character.only = TRUE))

### 1.0  Load and process the data ---------------------------------------------

## 1.1 Load GPS data containing HMM behavioural classifications ----------------
hmm_data <- read.csv("./Data_inputs/WAAL_2013_gps_labelled.csv")

# 1.1.2 Extract relevant columns, rename state column, format datetime
colnames <- c("BirdId", "Sex", "DateTime", "State")
hmm_data <- hmm_data[,c(colnames)] # BirdID, Ring, Sex, DateTime, State
hmm_data <- rename(hmm_data, state_hmm = State)
hmm_data$DateTime <- as.POSIXct(hmm_data$DateTime, format = "%Y-%m-%d %H:%M:%S")

## 1.2 Load manually labelled data ---------------------------------------------
manual_data <- read.csv("./Data_inputs/WAAL_2013_gps_manual_states.csv")

manual_data$STATE[manual_data$STATE == 1] <- "Travel"
manual_data$STATE[manual_data$STATE == 2] <- "Search"
manual_data$STATE[manual_data$STATE == 3] <- "Rest"

# 1.2.1 Rename state column, format datetime
manual_data <- rename(manual_data, state_manual = STATE)
manual_data$DateTime <- as.POSIXct(manual_data$DateTime, format = "%d/%m/%Y %H:%M")


### 2.0 Merge the dataframes for validation ------------------------------------

## 2.1 Merge to nearest minute -------------------------------------------------
setkey(setDT(hmm_data), "BirdId", "DateTime")
setkey(setDT(manual_data), "BirdId", "DateTime")
gps_comp <- as.data.frame(hmm_data[manual_data, roll = "nearest"])

## 2.2 Compute accuracy  -------------------------------------------------------

# 2.2.1 Convert to factors
factor_cols <- c("state_hmm", "state_manual")
gps_comp[,c(factor_cols)] <- lapply(gps_comp[,c(factor_cols)], as.factor)

# 2.2.2 Compute confusion matrix
confusionMatrix(gps_comp$state_hmm, gps_comp$state_manual)

tab <- table(gps_comp$state_hmm, gps_comp$state_manual)
tab <- tab / rowSums(tab)

#       Rest        Search      Travel
#Rest   0.970700637 0.029299363 0.000000000
#Search 0.298076923 0.462740385 0.239182692
#Travel 0.002642008 0.051519155 0.945838838