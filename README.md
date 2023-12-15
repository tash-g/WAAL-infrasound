# Infrasonic ambient noise as a cue for seabird foraging movements
Natasha Gillies*, Lucía Martina Martín López*, Olivier F. C. den Ouden, Jelle D. Assink, Mathieu Basille, Thomas A. Clay, Susana Clusella-Trullas, Rocío Joo, Henri Weimerskirch, Jeffrey N. Zeyl, Samantha C. Patrick

*These authors have contributed equally to this work. 

## Overview
This repository contains scripts and data to recreate the main results and figures of the following manuscript:

Albatross movement suggests sensitivity to infrasound cues at sea (2023). Gillies N*, Martin Lopez LM*, den Ouden OFC, Assink JD, Basille M, Clay T, Clusella-Trullas S, Joo R, Weimerskirch H, Zampolli M, Zeyl JN, Patrick SC. _PNAS_. 120 (42) e2218679120. DOI: https://doi.org/10.1073/pnas.2218679120

Note that microbarom sound maps are very large and therefore not available with this repository. Please get in touch if you are interested in using these data. 

## Scripts
A short description of each script is given below.

- **1-process_GPS_run_HMM.R** This script prepares the GPS data, fits a hidden Markov model (HMM) to identify behavioural states for each GPS fix, and separates stages of trip into 'outbound', 'middle' and 'inbound' sections. It produces the _'WAAL_2013_gps_labelled.csv'_ dataset which is subsequently processed in script 2, used to produce Figure S1.
- **2-define_decision_points_add_SPL.R** In this script, decision points (as defined in the text) are identified, microbarom sound pressure levels (SPL) are matched to each GPS fix, and the available area around the bird is split into 12 segments of 30 degrees each, ready for analysis. It produces the final datset for analysis. _Note: Loading, processing, and matching the microbarom maps requires significant processing power and so this script may take several hours to run._
- **3-fit_conditional_logit_models.R** This script fits conditional logistic regression models to the data to compare SPL conditions in selected versus non-selected segments. It fits and compares the models and produces summaries of the output, and includes code to produce figures 2 and 3 in the manuscript. It uses the dataset _'WAAL_2013_gps_processed_aperture30'_, which is produced using scripts 1 and 2. 
- **FUNCTION_boutFinder.R** Contains 'boutFinder' function, which is used to find continuous bouts of flight.
- **SUPP-MAT_gps-plot.R** This script contains the code to produce Figure S1, which shows a plot of all GPS tracks in the study, coloured by sex. It uses the dataset _'WAAL_2013_gps_labelled.csv'_, which is produced in script 1.
- **SUPP-MAT_HMM-manual_validation.R** This script contains the code to compare the behaviour outputs of the HMM to manual classification and assess accuracy.
- **SUPP_MAT_senstivity-analysis_apertures.R** This script contains the code to compare different aperture sizes for the segments. It first re-runs the processing in script 2 and outputs dataframes where aperture size has been varied. The best-fitting conditional logit models are then fitted to these datasets, and the effect sizes and directions of the output compared between the different aperture sizes. 
- **SUPP_MAT_summary-stats.R** This script contains code to summarise metrics from the foraging trips, such as maximum distance from colony, total distance covered, and trip duration. 

## Data inputs 
These datasets are used in the above scripts. The first dataset, 'WAAL_2013_gps_filtered' is processed using scripts 1 and 2 to produce the remaining datasets, but ready-made versions of these are provided for quick replication. Note that individual IDs have been recoded and so cannot be linked to existing datasets. Please contact the authors if you would like to make sure of these, as we may be able to offer addiitonal information, data, or advice. 

- **WAAL_2013_gps_filtered.csv** This is the main dataset from which the other datasets are produced. Each row corresponds to an individual GPS fix. Data have already been processed to remove very short trips to the Crozet shelf and to retain only one trip per individual. The variables are as follows:
	- _BirdId_: Encodes unique ID of bird; anonymised from original data (factor)
	- _TripID_: Encodes identity of trip for individual birds (factor)
	- _Sex_: Encodes whether bird is male (M) or female (F) (factor)
	- _DateTime_: Date and time of fix to nearest second (POSIXct)
	- _DistTrav_: Distance travelled since start of trip in km (numeric)
	- _WindSp_: Wind speed in m/s (numeric)
	- _Bearing_: Bearing of bird trajectory in degrees (numeric)
	- _Dev.wind2_: Wind direction (degrees) relative to bird bearing with directionality removed, i.e. 0 to 180 rather than -180-180 (numeric)
	- _Dist_cro_shelf_: Distance from Crozet shelf, km (numeric)
	- _Longitude_: Longitudinal position of GPS fix (numeric)
	- _Latitude_: Latitudinal position of GPS fix (numeric)
- **WAAL_2013_gps_labelled.csv** This dataset contains the same GPS fix information as the _'WAAL_2013_gps_filtered.csv'_ file, except each fix has been labelled with a behaviour and trip stage. The variables are as follows:
	- _BirdID_: Encodes unique ID of bird; anonymised from original data (factor)
	- _TripID_: Encodes identity of trip for individual birds (factor)
	- _Sex_: Encodes whether bird is male (M) or female (F) (factor)
	- _DateTime_: Date and time of fix to nearest second (POSIXct)
	- _DistTrav_: Distance travelled since start of trip in km (numeric)
	- _WindSp_: Wind speed in m/s (numeric)
	- _Bearing_: Bearing of bird trajectory in degrees (numeric)
	- _Dev.wind2_: Wind direction (degrees) relative to bird bearing with directionality removed, i.e. 0 to 180 rather than -180-180 (numeric)
	- _Dist_cro_shelf_: Distance from Crozet shelf, km (numeric)
	- _x_lon_: Longitudinal position of GPS fix (numeric)
	- _y_lat_: Latitudinal position of GPS fix (numeric)
	- _State_: Indicates probable behavioural state of GPS fix, either 'Travel', 'Search', or 'Rest' (factor)
	- _counter_: Index of GPS fix within trips, used in processing in Script 2 (numeric)
- **WAAL_2013_gps_manual_states.csv** This dataset contains expert manual classifications of each GPS fix into a different behavioural state (travel, search, or rest). It is used to assess the performance of the hidden Markov model, as described in the Supplementary Materials. The variables are as follows:
	- _Sex_: Encodes whether bird is male (M) or female (F) (factor)
	- _DateTime_: Date and time of fix to nearest second (POSIXct)
	- _state_manual_: Manual label indicating probable behavioural state of GPS fix, either 'Travel', 'Search', or 'Rest' (factor)
	- _BirdID_: Encodes unique ID of bird; anonymised from original data (factor)
- **WAAL_2013_gps_processed_aperture60.csv** This dataset is used for the main analyses, and contains rows for each segment (focal and non-focal) within decision points. There are 12 segments per decision point. The variables are as follows:
	- _segment_ID_: Encodes whether the segment is focal (1) or non-focal (0) (numeric)
	- _segment_n_: Number of segment, containing clockwise from focal (1) (numeric)
	- _abs_SPL_2000dB_: Sound pressure level integrated across the individual segment to a radius of 2000km, dB (numeric)
	- _abs_SPL_2000dB_: Sound pressure level, standardised within the decision point (numeric)
	- _birdID_: Encodes unique ID of bird; anonymised from original data (factor)
	- _TripID_: Encodes identity of trip for individual birds (factor)
	- _Sex_: Encodes whether bird is male (M) or female (F) (factor)
	- _x_lon_: Longitudinal position of GPS fix (numeric)
	- _y_lat_: Latitudinal position of GPS fix (numeric)
	- _State_: Indicates probable behavioural state of GPS fix, either 'Travel', 'Search', or 'Rest' (factor)
	- _Dist_cro_shelf_: Distance from Crozet shelf, km (numeric)
	- _WindSp_: Wind speed in m/s (numeric)
	- _relDir_: Wind direction (degrees) relative to segment bearing with directionality removed, i.e. 0 to 180 rather than -180-180 (numeric)
	- _pointID_: Encodes individual ID of the decision point (factor)
- **WAAL_gps_2013_Trav20.csv** This dataset is used in the segment aperture sensitivity analysis. Each row is a GPS fix from a dataset that has been filtered to only include bouts of travel lasting at least 20km. The variables are as follows:
	- _BirdID_: Encodes unique ID of bird; anonymised from original data (factor)
	- _TripID_: Encodes identity of trip for individual birds (factor)
	- _Sex_: Encodes whether bird is male (M) or female (F) (factor)
	- _DateTime_: Date and time of fix to nearest second (POSIXct)
	- _DistTrav_: Distance travelled since start of trip in km (numeric)
	- _WindSp_: Wind speed in m/s (numeric)
	- _Bearing_: Bearing of bird trajectory in degrees (numeric)
	- _Dev.wind2_: Wind direction (degrees) relative to bird bearing with directionality removed, i.e. 0 to 180 rather than -180-180 (numeric)
	- _Dist_cro_shelf_: Distance from Crozet shelf, km (numeric)
	- _x_lon_: Longitudinal position of GPS fix (numeric)
	- _y_lat_: Latitudinal position of GPS fix (numeric)
	- _State_: Indicates probable behavioural state of GPS fix, either 'Travel', 'Search', or 'Rest' (factor)
	- _counter_: Index of GPS fix within trips, used in processing in Script 2 (numeric)
	- _idx_: Index counting each row within a trip (numeric)
	- _travbout_: Encoding unique ID of travel bout within a trip (factor)
	- _TotdisttravBout_: Total distance covered within the bout of travel
