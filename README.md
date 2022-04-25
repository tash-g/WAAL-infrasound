# Infrasonic ambient noise as a cue for seabird foraging movements
Natasha Gillies*, Lucía Martina Martín López*, Olivier F. C. den Ouden, Jelle D. Assink, Mathieu Basille, Thomas A. Clay, Susana Clusella-Trullas, Rocío Joo, Henri Weimerskirch, Jeffrey N. Zeyl, Samantha C. Patrick

*These authors have contributed equally to this work. 

## Overview
This repository contains scripts and data to recreate the main results and figures of this paper (currently in prep). Note 

## Scripts
A short description of each script is given below.

- **1_process_GPS_run_HMM.R** This script prepares the GPS data, fits a hidden Markov model (HMM) to identify behavioural states for each GPS fix, and separates stages of trip into 'outbound', 'middle' and 'inbound' sections.
- **2_define_decision_points_add_SPL.R** In this script, decision points (as defined in the text) are identified, microbarom sound pressure levels (SPL) are matched to each GPS fix, and the available area around the bird is split into 12 segments of 30 degrees each, ready for analysis. _Note: Loading, processing, and matching the microbarom maps requires significant processing power and so this script may take several hours to run._
- **ANALYSE_conditional_logist.R** This script fits conditional logistic regression models to the data to compare SPL conditions in selected versus non-selected segments. It fits and compares the models and produces summaries of the output, and includes code to produce figures 2 and 3 in the manuscript. 
- **WAAL_infrasound_functions.R** Contains 'boutFinder' function, which is used to find continuous bouts of flight.
- **SUPP_INF_gps-plot.R** This script contains the code to plot all GPS data.
- **SUPP-INF_HMM-manual_validation.R** This script contains the code to compare HMM outputs to manual classification of behaviours.
- **SUPP_INF_senstivity-analysis_apertures.R** This script contains the code to compare different aperture sizes for the segments.

## Data inputs 
- **WAAL_2013_gps_filtered.csv** 
- **WAAL_2013_gps_labelled.csv** 
- **WAAL_gps_2013_Trav20.csv**