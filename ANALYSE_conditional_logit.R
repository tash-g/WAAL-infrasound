##'**AIM: Fit a conditional logistic regression model to compare conditions of focal to non-focal sectors**
##'**within decision points**


# Load packages -----------------------------------------------------------

# Packages
packages <- c("survival", "ggplot2", "gridExtra", "dplyr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

## Install hab package for QIC comparison
#devtools::install_github("basille/hab")


# Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/GPS_2013_model-data-ORIGINAL.csv", data.table = F)

# Rename and process variables
names(modDat)[2] <- "case"

factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# Retain original variables for scaling later
modDat$abs_SPL_2000dB.OG <- modDat$abs_SPL_2000dB
modDat$relDir.OG <- modDat$relDir
modDat$WindSp.OG <- modDat$WindSp



# Check data structure ----------------------------------------------------

# Check only have 1s and 0s as cases
table(modDat$case)

# Check sum of cases within stratas = 1
table(tapply(modDat$case, modDat$pointID, sum)) 

# Check each strata has 12 cones (single one with 11)
table(table(modDat$pointID))

# Check total number of stratas
table(tapply(modDat$birdID, modDat$pointID, function(x) length(unique(x))))

# Check have values for each covariate
modDat %>%
  subset(case == 1) %>%
  summarise(
    SPL = sum(is.na(abs_SPL_2000dB)),
    Sex = sum(is.na(Sex)),
    WindDir = sum(is.na(relDir)),
    WindSp = sum(is.na(WindSp))
  )


# Run conditional logistic regression -------------------------------------

## NOTE: There is no variation in sex at the level of cluster, so fit male
## and females separately (to avoid blurring results)

## Scale continuous variables and remove NA variables
modDat[,c(3,7,8)] <- lapply(modDat[,c(3,7,8)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 

## Separate males and females
modDat.F <- subset(modDat, Sex == "F")
modDat.M <- subset(modDat, Sex == "M")

## Set up the models

### H_wind ###
H_wind.F <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID, 
                  robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_wind.F) 

H_wind.M <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID,
                   robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_wind.M) 


### H_SPL ###
H_SPL.F <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + 
                    strata(pointID), cluster = birdID, 
                   robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPL.F) 

H_SPL.M <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + 
                          strata(pointID), cluster = birdID, 
                          robust = TRUE, method = 'approximate',  data = modDat.M)
summary(H_SPL.M) 


### H_SPLTrip ###
H_SPLTrip.F <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + 
                        abs_SPL_2000dB:Trip_state + strata(pointID), cluster = birdID, 
                   robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPLTrip.F) 

H_SPLTrip.M <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + 
                        abs_SPL_2000dB:Trip_state + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_SPLTrip.M) 



## Compare models using QIC weights
hab::QIC(H_wind.F, H_SPL.F, H_SPLTrip.F) 

#QIC   QuasiLL     n nevent K     Trace deltaQIC     weight
#H_wind.F    9149.548 -4572.030 22211   1851 2  2.744307 0.000000 0.81910415
#H_SPL.F     9156.790 -4572.852 22211   1851 4  5.543440 7.241846 0.02191756
#H_SPLTrip.F 9152.827 -4565.779 22211   1851 6 10.634352 3.278887 0.15897829

hab::QIC(H_wind.M, H_SPL.M, H_SPLTrip.M) 

#QIC        QuasiLL     n nevent K     Trace   deltaQIC       weight
#H_wind.M    6562.355 -3279.416 15888   1324 2  1.761767 17.4088777 9.531185e-05
#H_SPL.M     6545.549 -3265.489 15888   1324 4  7.285583  0.6025057 4.252107e-01
#H_SPLTrip.M 6544.946 -3259.773 15888   1324 6 12.700448  0.0000000 5.746940e-01

# Visualise effects -------------------------------------------------------


### FEMALE 
summary(H_wind.F)

RSF_plot.F <- sjPlot::plot_model(H_wind.F, title="SSF Coefficients")
RSF_plot.F.data <- as.data.frame(RSF_plot.F$data)
RSF_plot.F.data <- subset(RSF_plot.F.data)

# Rename terms for plotting
RSF_plot.F.data$term <- as.character(RSF_plot.F.data$term)
RSF_plot.F.data$term[1:2] <- c("windDir", "windDir:windSp")

coefPlot.F <- ggplot() +
  geom_point(data = RSF_plot.F.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.F.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  labs(x = "Estimate (Odds Ratios)", y = "Variable") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))

### MALE 
summary(H_SPLTrip.M)

RSF_plot.M <- sjPlot::plot_model(H_SPLTrip.M, title="SSF Coefficients")
RSF_plot.M.data <- as.data.frame(RSF_plot.M$data)
RSF_plot.M.data <- subset(RSF_plot.M.data)

# Rename terms for plotting
RSF_plot.M.data$term <- as.character(RSF_plot.M.data$term)
RSF_plot.M.data$term[1:6] <- c("SPL", "windDir", "SPL:windDir", "SPL:StateMid", "SPL:StateOut",
                               "SPL:windSp")

coefPlot.M <- ggplot() +
  geom_point(data = RSF_plot.M.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.M.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw() +
  labs(x = "Estimate (Odds Ratios)", y = "Variable") +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))

## Coefficients plot
png(filename = "Figures/FIGX_coefs.png", width = 13, height = 7, units = "in", res = 600)
gridExtra::grid.arrange(coefPlot.F, coefPlot.M, ncol = 2)
dev.off()


### Predictive plots

## FEMALE ##

# Get predictions
pred_plot.F <- sjPlot::plot_model(H_wind.F, type = "int")
pred_plot.F <- data.frame(pred_plot.F$data)

# Set selected cones (1) to max y axis
modDat.F$dummy_cone <- 0
modDat.F$dummy_cone[modDat.F$case == TRUE] <- max(pred_plot.F$conf.high)
modDat.F$dummy_cone[modDat.F$case == FALSE] <- min(pred_plot.F$conf.low)

# Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.F$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

# Set colours
high_wind <- "#440154FF" # purple
low_wind <- "#FDE725FF"  # yellow

png(filename = "Figures/FIGX_Hwind-F.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_ribbon(data = pred_plot.F, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "Odds Ratio", x = "Wind Direction (°)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))
dev.off()



## MALE ##

# Get predictions - SPL
pred_plot.M <- sjPlot::plot_model(H_SPLTrip.M, type = "pred", terms = "abs_SPL_2000dB")
pred_plot.M <- data.frame(pred_plot.M$data)

# Set selected cones (1) to max y axis
modDat.M$dummy_cone <- 0
modDat.M$dummy_cone[modDat.M$case == 1] <- max(pred_plot.M$conf.high)
modDat.M$dummy_cone[modDat.M$case == 0] <- min(pred_plot.M$conf.low)

# Get scaling attributes for SPL
att_SPL <- attributes(scale(modDat.M$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
mylabels_SPL <- seq(50,75,5)
mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

png(filename = "Figures/FIGX_HSPLTrip-M.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_count(data = modDat.M, aes(x = abs_SPL_2000dB, y = dummy_cone), alpha = 0.2) +
  geom_ribbon(data = pred_plot.M, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M, aes(x = x, y = predicted), size = 1) +
  scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  labs(y = "Odds Ratio", x = "Sound Pressure Level (dB)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))
dev.off()



# Get predictions - SPL * wind direction
pred_plot.M <- sjPlot::plot_model(H_SPLTrip.M, type = "int", terms = c("abs_SPL_2000dB", "relDir"))
pred_plot.M <- data.frame(pred_plot.M[[1]]$data)

# Get scaling attributes for SPL
att_SPL <- attributes(scale(modDat.M$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
mylabels_SPL <- seq(50,75,5)
mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

png(filename = "Figures/FIGX_HSPLTrip-M.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_ribbon(data = pred_plot.M, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "Odds Ratio", x = "Sound Pressure Level (dB)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))
dev.off()



# APPENDIX ----------------------------------------------------------------


## Quick wind check
windDat <- subset(modDat, case == "TRUE")

## Direction
ggplot(aes(y = abs_SPL_2000dB, x = relDir), data = windDat) + 
  geom_point() + theme_bw() + labs(x = "Relative Wind Direction", y = "SPL")

windDat_noOutliers <- subset(windDat, abs_SPL_2000dB > 57)

ggplot(aes(y = abs_SPL_2000dB, x = relDir), data = windDat_noOutliers) + 
  geom_point() + theme_bw() + labs(x = "Relative Wind Direction", y = "SPL")


## Speed
ggplot(aes(y = abs_SPL_2000dB, x = WindSp), data = windDat) + 
  geom_point() + theme_bw() + labs(x = "Wind Speed (m/s)", y = "SPL")

ggplot(aes(y = abs_SPL_2000dB, x = WindSp), data = windDat_noOutliers) + 
  geom_point() + theme_bw() + labs(x = "Wind Speed (m/s)", y = "SPL")


