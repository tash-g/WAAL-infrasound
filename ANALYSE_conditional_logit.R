##'**AIM: Fit a conditional logistic regression model to compare conditions of**
##'**focal to non-focal sectors within decision points**


# 1.0 Load packages -----------------------------------------------------------

# Packages
packages <- c("survival", "ggplot2", "grid", "dplyr", "emmeans", "sjPlot", 
              "tibble", "ggpubr")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

## Install hab package for QIC comparison
#devtools::install_github("basille/hab")


# 1.1 Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/GPS_2013_model-data-ORIGINAL.csv", 
                            data.table = F)

# Rename and process variables
names(modDat)[2] <- "case"

factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# Retain original variables for scaling later
modDat$abs_SPL_2000dB.OG <- modDat$abs_SPL_2000dB
modDat$relDir.OG <- modDat$relDir
modDat$WindSp.OG <- modDat$WindSp



# 1.2 Check data structure ----------------------------------------------------

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


# 2.0 Run conditional logistic regression -------------------------------------

## NOTE: There is no variation in sex at the level of cluster, so fit male
## and females separately (to avoid blurring results)

## Scale continuous variables and remove NA variables
modDat[, c(3, 7, 8)] <-
  lapply(modDat[, c(3, 7, 8)], function(x)
    c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir

## Separate males and females
modDat.F <- subset(modDat, Sex == "F")
modDat.F <- droplevels(modDat.F)
modDat.M <- subset(modDat, Sex == "M")
modDat.M <- droplevels(modDat.M)

set.seed(817)

## Set up the models

### wind_model ###
H_wind.F <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID, 
                  robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_wind.F) 

H_wind.M <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID,
                   robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_wind.M) 


### wind + SPL_model ###
H_SPL.F <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + relDir:WindSp +
                    strata(pointID), cluster = birdID, 
                   robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPL.F) 

H_SPL.M <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + relDir:WindSp +
                          strata(pointID), cluster = birdID, 
                          robust = TRUE, method = 'approximate',  data = modDat.M)
summary(H_SPL.M) 


## Compare models using QIC weights
hab::QIC(H_wind.F, H_SPL.F) 

#                 QIC   QuasiLL     n nevent K     Trace  deltaQIC     weight
#H_wind.F    9149.548 -4572.030 22211   1851 2  2.744307 0.0000000 0.51626856
#H_SPL.F     9153.566 -4570.215 22211   1851 5  6.567876 4.0176137 0.06925672

hab::QIC(H_wind.M, H_SPL.M) 

#                QIC   QuasiLL     n nevent K     Trace   deltaQIC       weight
#H_wind.M    6562.355 -3279.416 15888   1324 2  1.761767 28.9549461 2.683097e-07
#H_SPL.M     6533.562 -3259.131 15888   1324 5  7.650200  0.1612348 4.798564e-01


## Get mean SPL for each trip stage for males
tapply(modDat$abs_SPL_2000dB.OG, modDat$Trip_state, function(x) c(mean(x), sd(x))) 

## Get summaries from best supported models
summary(H_wind.F)

#              exp(coef) exp(-coef) lower .95 upper .95
#relDir           0.8399      1.191    0.7912    0.8916
#relDir:WindSp    0.9401      1.064    0.8971    0.9852

summary(H_SPL.M)

#                      exp(coef) exp(-coef) lower .95 upper .95
#abs_SPL_2000dB           1.2530     0.7981    1.0998    1.4276
#relDir                   0.9958     1.0042    0.9428    1.0517
#abs_SPL_2000dB:relDir    0.9258     1.0802    0.8745    0.9801
#abs_SPL_2000dB:WindSp    1.0187     0.9816    0.9039    1.1482
#relDir:WindSp            0.9072     1.1023    0.8658    0.9506

### SUPPLEMENTARY INFO ###

### wind + SPL + trip
H_SPLTrip.F <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + relDir:WindSp +
                        abs_SPL_2000dB:Trip_state + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPLTrip.F) 

H_SPLTrip.M <- clogit(case ~ abs_SPL_2000dB*relDir + abs_SPL_2000dB:WindSp + relDir:WindSp +
                        abs_SPL_2000dB:Trip_state + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_SPLTrip.M) 



## Compare models using QIC weights
hab::QIC(H_wind.F, H_SPL.F, H_SPLTrip.F) 

#                 QIC   QuasiLL     n nevent K     Trace  deltaQIC     weight
#H_wind.F    9149.548 -4572.030 22211   1851 2  2.744307 0.0000000 0.51626856
#H_SPL.F     9153.566 -4570.215 22211   1851 5  6.567876 4.0176137 0.06925672
#H_SPLTrip.F 9149.988 -4563.296 22211   1851 7 11.697645 0.4392302 0.41447472

hab::QIC(H_wind.M, H_SPL.M, H_SPLTrip.M) 

#                QIC   QuasiLL     n nevent K     Trace   deltaQIC       weight
#H_wind.M    6562.355 -3279.416 15888   1324 2  1.761767 28.9549461 2.683097e-07
#H_SPL.M     6533.562 -3259.131 15888   1324 5  7.650200  0.1612348 4.798564e-01
#H_SPLTrip.M 6533.400 -3253.707 15888   1324 7 12.993235  0.0000000 5.201433e-01

## Get mean SPL for each trip stage for males
tapply(modDat$abs_SPL_2000dB.OG, modDat$Trip_state, function(x) c(mean(x), sd(x))) 



# 3.0 Visualise effects -------------------------------------------------------

### 3.1 FEMALE - WIND MODEL COEFFICIENTS --------------------------------------
summary(H_wind.F)

# Get data
RSF_plot.F.data <- data.frame(summary(H_wind.F)$conf.int)
RSF_plot.F.data <- tibble::rownames_to_column(RSF_plot.F.data, "term")
colnames(RSF_plot.F.data) <- c("term","estimate", "exp(coef)", "conf.low","conf.high")

# Add empty rows and reorder to match male data
RSF_plot.F.data[nrow(RSF_plot.F.data) + 3,] <- NA
RSF_plot.F.data$term <- c("windDir", "windDir:windSp", "SPL", "SPL:windDir", "SPL:windSp")
RSF_plot.F.data <- RSF_plot.F.data[c(3,1,4,5,2),]

coefPlot.F <- ggplot() +
  geom_point(data = RSF_plot.F.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.F.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  scale_x_continuous(breaks = c(0.6,0.9,1.2,1.5,1.8), limits = c(0.6,1.8)) +
  labs(x = "Estimate (Odds Ratio)", y = "Parameter") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 20)) +
  scale_y_discrete(limits = c("windDir:windSp", "SPL:windSp", "SPL:windDir",
                              "windDir","SPL")) +
  ggtitle("Females")

### 3.2 MALE - WIND + SPL MODEL COEFFICIENTS -------------------------------
summary(H_SPL.M)

# Get data
RSF_plot.M.data <- data.frame(summary(H_SPL.M)$conf.int)
RSF_plot.M.data <- tibble::rownames_to_column(RSF_plot.M.data, "term")
RSF_plot.M.data$term <- c("SPL", "windDir", "SPL:windDir", "SPL:windSp", "windDir:windSp")
colnames(RSF_plot.M.data) <- c("term", "estimate", "exp(coef)", "conf.low","conf.high")

coefPlot.M <- ggplot() +
  geom_point(data = RSF_plot.M.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.M.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw() +
  labs(x = "Estimate (Odds Ratio)", y = "Parameter") +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20)) +
  scale_x_continuous(breaks = c(0.6,0.9,1.2,1.5,1.8), limits = c(0.6,1.8)) +
  scale_y_discrete(limits = c("windDir:windSp", "SPL:windSp", "SPL:windDir",
                              "windDir","SPL")) +
  ggtitle("Males")


# FIGURE 2: Coefficients plot -----------------------------------
png(filename = "Figures/FIG2_coefs.png", width = 13, height = 7, units = "in", res = 600)
grid.draw(cbind(ggplotGrob(coefPlot.M), ggplotGrob(coefPlot.F), size = "last"))
dev.off()




## 3.3 FEMALE - WIND MODEL PREDICTIONS -------------------------------

# Get predictions
pred_plot.F <- sjPlot::plot_model(H_wind.F, type = "int")
pred_plot.F <- data.frame(pred_plot.F$data)

# Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.F$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

# Set colours
high_wind <- "#440154FF" # purple
low_wind <- "#FDE725FF"  # yellow

pred_F <- ggplot() + 
  geom_ribbon(data = pred_plot.F, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), limits = c(0, 3.5)) +
  labs(x = "Wind Direction (°)", title = "Females", tag = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.8,0.85),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0, 
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2))) 

png(filename = "Figures/FIGX_Hwind-F.png", width = 9, height = 7, units = "in", res = 600)
pred_F
dev.off()




## 3.4 MALE - WIND PREDICTIONS ## -------------------------------

# Get predictions
pred_plot.M1 <- sjPlot::plot_model(H_SPL.M, type = "int")[[3]]
pred_plot.M1 <- data.frame(pred_plot.M1$data)

# Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.M$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

pred_M.wind <- ggplot() + 
  geom_ribbon(data = pred_plot.M1, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                       group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M1, aes(x = x, y = predicted, group = group, 
                                     col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), limits = c(0, 3.5)) +
  labs(y = "Odds Ratio", x = "Wind Direction (°)", title = "Males", tag = "(a)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.8,0.85),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0,
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2)))

png(filename = "Figures/FIGX_Hwind-M.png", width = 9, height = 7, units = "in", res = 600)
pred_M.wind
dev.off()




## 3.5 MALE - SPL*WIND DIR PREDICTIONS ## -------------------------------

# Get predictions - SPL * wind direction
pred_plot.M2 <- sjPlot::plot_model(H_SPL.M, type = "int")[[1]]
pred_plot.M2 <- data.frame(pred_plot.M2$data)

# Get scaling attributes for SPL
att_SPL <- attributes(scale(modDat.M$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
mylabels_SPL <- seq(50,75,5)
mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

pred_M.SPL <- ggplot() + 
  geom_ribbon(data = pred_plot.M2, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                      group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M2, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "Odds Ratio", x = "Sound Pressure Level (dB)", tag = "(b)", title = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.2,0.85),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0,
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2)))


png(filename = "Figures/FIGX_HSPL-M.png", width = 9, height = 7, units = "in", res = 600)
pred_M.SPL
dev.off()

# FIGURE 3: Predictions plot -----------------------------------
png(filename = "Figures/FIG3_predictions.png", width = 13, height = 13, units = "in", res = 600)
ggarrange(pred_M.wind, pred_F, pred_M.SPL,
                  ncol = 2, nrow = 2, widths = c(2,1.75,2))
dev.off()


