# Infrasonic ambient noise as a cue for seabird foraging movements
Natasha Gillies*, Lucía Martina Martín López*, Olivier F. C. den Ouden, Jelle D. Assink, Mathieu Basille, Thomas A. Clay, Susana Clusella-Trullas, Rocío Joo, Henri Weimerskirch, Jeffrey N. Zeyl, Samantha C. Patrick

*These authors have contributed equally to this work. 

## Overview
This repository contains scripts and data to recreate the main results and figures of this paper (currently in prep). Note that microbarom sound maps are very large and therefore not available with this repository. Please get in touch if you are interested in using these data. 

## Scripts
A short description of each script is given below.

- **1-process_GPS_run_HMM.R** This script prepares the GPS data, fits a hidden Markov model (HMM) to identify behavioural states for each GPS fix, and separates stages of trip into 'outbound', 'middle' and 'inbound' sections. It produces the dataset _'WAAL_2013_gps_labelled.csv'_ which is subsequently processed in script 2, used to produce Figure S1.
- **2-define_decision_points_add_SPL.R** In this script, decision points (as defined in the text) are identified, microbarom sound pressure levels (SPL) are matched to each GPS fix, and the available area around the bird is split into 12 segments of 30 degrees each, ready for analysis. It produces the final datset for analysis. _Note: Loading, processing, and matching the microbarom maps requires significant processing power and so this script may take several hours to run._
- **3-fit_conditional_logit_models.R** This script fits conditional logistic regression models to the data to compare SPL conditions in selected versus non-selected segments. It fits and compares the models and produces summaries of the output, and includes code to produce figures 2 and 3 in the manuscript. It uses the dataset _'WAAL_2013_gps_processed_aperture30'_, which is produced using scripts 1 and 2. 
- **FUNCTION_boutFinder.R** Contains 'boutFinder' function, which is used to find continuous bouts of flight.
- **SUPP_MAT_gps-plot.R** This script contains the code to produce Figure S1, which shows a plot of all GPS tracks in the study, coloured by sex. It uses the dataset _'WAAL_2013_gps_labelled.csv'_, which is produced in script 1.
- **SUPP-INF_HMM-manual_validation.R** This script contains the code to compare the behaviour outputs of the HMM to manual classification and assess accuracy.
- **SUPP_INF_senstivity-analysis_apertures.R** This script contains the code to compare different aperture sizes for the segments. It first re-runs the processing in script 2 and outputs dataframes where aperture size has been varied. The best-fitting conditional logit models are then fitted to these datasets, and the effect sizes and directions of the output compared between the different aperture sizes. 

## Data inputs 
These datasets are used in the above scripts. The first dataset, 'WAAL_2013_gps_filtered' is processed using scripts 1 and 2 to produce the remaining datasets, but ready-made versions of these are provided for quick replication. Note that individual IDs have been recoded and so cannot be linked to existing datasets. Please contact the authors if you would like to make sure of these, as we may be able to offer addiitonal information, data, or advice. 

- **WAAL_2013_gps_filtered.csv** This is the main dataset from which the other datasets are produced. Each row corresponds to an individual GPS fix. Data have already been processed to remove very short trips to the Crozet shelf and to retain only one trip per individual. The variables are as follows:
	- _BirdID_
	- _TripID_
	- _Sex_
	- _DateTime_
	- _DistTrav_
	- _WindSp_
	- _Bearing_
	- _Dev.wind2_ Wind direction (degrees) relative to bird bearing with directionality removed, i.e. 0 to 180 rather than -180-180
	- _LoD_
	- _Dist_cro_shelf
	- _Dist_col_
	- _max_dist_
	- _Dist_world_
	- _Longitude_
	- _Latitude_
- **WAAL_2013_gps_labelled.csv** 
- **WAAL_2013_gps_manual_states.csv**
- **WAAL_2013_gps_processed_aperture30.csv** 
- **WAAL_gps_2013_Trav20.csv**