## ---------------------------
##
## Script name: ANALYSE_conditional_logit
##
## Purpose of script: This script is used to fit conditional logistic regression
## models to the data to compare conditions between non-focal and focal segments
## within decision points. 
##
## Author: Dr. Natasha Gillies
##
## Date Created: 2022-01-19
##
## R version: 4.1.1 (2021-08-10)
##
## Email: gilliesne@gmail.com
##
## ---------------------------


### 0.0 Load packages ----------------------------------------------------------

# 0.0.0 Define the Packages
packages <- c("survival", "ggplot2", "grid", "dplyr", "emmeans", "sjPlot", 
              "tibble", "ggpubr", "cowplot")

# Install packages not yet installed - change lib to library path
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages], lib = "C:/Users/libraryPath")
#}

# 0.0.1 Load packages
invisible(lapply(packages, library, character.only = TRUE))

## Install hab package for QIC comparison
#devtools::install_github("basille/hab")


## 0.1 Create figures folder if one does not exist -----------------------------

out.path <- "./Figures/"

if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
}

## 0.2 Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/WAAL_2013_gps_processed_aperture30deg.csv", 
                            data.table = F)

# 0.2.1 Rename and process variables
modDat <- rename(modDat, case = segment_ID)

factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# 0.2.2 Retain original variables for scaling later
modDat$abs_SPL_2000dB_std.OG <- modDat$abs_SPL_2000dB_std
modDat$relDir.OG <- modDat$relDir
modDat$WindSp.OG <- modDat$WindSp

# Remove NA variables
modDat <- modDat[!is.na(modDat$abs_SPL_2000dB_std),]

## 0.3 Check data structure ----------------------------------------------------

# 0.3.1 Check only have 1s and 0s as cases
table(modDat$case)

# 0.3.2 Check sum of cases within stratas = 1
table(tapply(modDat$case, modDat$pointID, sum)) 

# 0.3.3 Check each strata has 12 cones (single one with 11)
table(table(modDat$pointID))

# 0.3.4 Check total number of stratas
table(tapply(modDat$birdID, modDat$pointID, function(x) length(unique(x))))

# 0.3.5 Check have values for each covariate
modDat %>%
  subset(case == 1) %>%
  summarise(
    SPL = sum(is.na(abs_SPL_2000dB)),
    Sex = sum(is.na(Sex)),
    WindDir = sum(is.na(relDir)),
    WindSp = sum(is.na(WindSp))
  )


### 1.0 Run conditional logistic regression ------------------------------------

#### NOTE: There is no variation in sex at the level of cluster, so fit male
#### and females separately (to avoid blurring results)

# 1.0.1 Scale continuous variables and remove NA variables
vars_scale <- c("WindSp", "relDir")
modDat[, vars_scale] <-
  lapply(modDat[, vars_scale], function(x)
    c(scale(x, center = TRUE, scale = TRUE)))

# 1.0.2 Separate males and females
modDat.F <- subset(modDat, Sex == "F")
modDat.F <- droplevels(modDat.F)
modDat.M <- subset(modDat, Sex == "M")
modDat.M <- droplevels(modDat.M)


## 1.1 Set up the models  ------------------------------------------------------

### wind_model ###
H_wind.F <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID, 
                  robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_wind.F) 

H_wind.M <- clogit(case ~ relDir + relDir:WindSp + strata(pointID), cluster = birdID,
                   robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_wind.M) 


### wind + SPL_model ###
H_SPL.F <- clogit(case ~ abs_SPL_2000dB_std*relDir + abs_SPL_2000dB_std:WindSp + relDir:WindSp +
                    strata(pointID), cluster = birdID, 
                   robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPL.F) 

H_SPL.M <- clogit(case ~ abs_SPL_2000dB_std*relDir + abs_SPL_2000dB_std:WindSp + relDir:WindSp +
                          strata(pointID), cluster = birdID, 
                          robust = TRUE, method = 'approximate',  data = modDat.M)
summary(H_SPL.M) 


## 1.2 Compare models using QIC weights   --------------------------------------
hab::QIC(H_wind.F, H_SPL.F) 

#              QIC   QuasiLL     n nevent K    Trace deltaQIC       weight
#H_wind.F 8838.760 -4417.141 21480   1790 2 2.238461 21.69677 1.943556e-05
#H_SPL.F  8817.063 -4402.695 21480   1790 5 5.836389  0.00000 9.999806e-01

hab::QIC(H_wind.M, H_SPL.M) 

#              QIC   QuasiLL     n nevent K    Trace deltaQIC       weight
#H_wind.M 6621.018 -3308.577 16020   1335 2 1.932559 45.96252 1.045598e-10
#H_SPL.M  6575.056 -3278.989 16020   1335 5 8.538800  0.00000 1.000000e+00


# 1.2.1 Get summaries from best supported models
summary(H_SPL.F)

#                          exp(coef) exp(-coef) lower .95 upper .95
#abs_SPL_2000dB_std           1.0324     0.9686    0.9823    1.0850
#relDir                       0.8365     1.1954    0.7942    0.8811
#abs_SPL_2000dB_std:relDir    0.8624     1.1595    0.8095    0.9187
#abs_SPL_2000dB_std:WindSp    1.0405     0.9610    0.9771    1.1081
#relDir:WindSp                0.9399     1.0640    0.8960    0.9858    

summary(H_SPL.M)

#                          exp(coef) exp(-coef) lower .95 upper .95
#abs_SPL_2000dB_std           1.1980     0.8347    1.0983    1.3068
#relDir                       0.9685     1.0325    0.9162    1.0238
#abs_SPL_2000dB_std:relDir    0.8585     1.1648    0.7855    0.9383
#abs_SPL_2000dB_std:WindSp    0.9923     1.0078    0.9025    1.0910
#relDir:WindSp                0.9204     1.0865    0.8779    0.9650



### 2.0 Visualise effects ------------------------------------------------------

# 2.0.0 Get focal vs non-focal SPL

# Standardised SPL
stdSPL_comp <- with(modDat, 
                        aggregate(abs_SPL_2000dB_std.OG ~ case + Sex, 
                              FUN =  function(x) c( SD = sd(x), MN = mean(x) ) ) ) 
stdSPL_comp <- do.call(data.frame, stdSPL_comp)

stdSPL_comp %>% group_by(Sex) %>% summarise(diff = abs_SPL_2000dB_std.OG.MN[2] - abs_SPL_2000dB_std.OG.MN[1],
                                            diff_SD = abs_SPL_2000dB_std.OG.SD[2] - abs_SPL_2000dB_std.OG.SD[1])

# Absolute SPL
stdSPL <- with(modDat, aggregate(abs_SPL_2000dB ~ case + Sex, FUN =  function(x) c( SD = sd(x), MN = mean(x) ) ) )
stdSPL <- do.call(data.frame, stdSPL)

stdSPL %>% group_by(Sex) %>% summarise(diff = abs_SPL_2000dB.MN[2] - abs_SPL_2000dB.MN[1],
                                            diff_SD = abs_SPL_2000dB.SD[2] - abs_SPL_2000dB.SD[1])



## 2.1 FEMALE - WIND + SPL MODEL COEFFICIENTS ----------------------------------------

# 2.1.0 Get data
RSF_plot.F.data <- data.frame(summary(H_SPL.F)$conf.int)
RSF_plot.F.data <- tibble::rownames_to_column(RSF_plot.F.data, "term")
RSF_plot.F.data$term <- c("SPL", "windDir", "SPL:windDir", "SPL:windSp", "windDir:windSp")
colnames(RSF_plot.F.data) <- c("term", "estimate", "exp(coef)", "conf.low","conf.high")

# 2.2.1 Make the plot
coefPlot.F <- ggplot() +
  geom_point(data = RSF_plot.F.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.F.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw() +
  labs(x = "Estimate (Odds Ratio)", y = "") +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20)) +
  scale_x_continuous(breaks = c(0.6,0.9,1.2,1.5,1.8), limits = c(0.6,1.8)) +
  scale_y_discrete(limits = c("windDir:windSp", "SPL:windSp", "SPL:windDir",
                              "windDir","SPL")) +
  ggtitle("Females")


## 2.2 MALE - WIND + SPL MODEL COEFFICIENTS ------------------------------------

# 2.2.0 Get data
RSF_plot.M.data <- data.frame(summary(H_SPL.M)$conf.int)
RSF_plot.M.data <- tibble::rownames_to_column(RSF_plot.M.data, "term")
RSF_plot.M.data$term <- c("SPL", "windDir", "SPL:windDir", "SPL:windSp", "windDir:windSp")
colnames(RSF_plot.M.data) <- c("term", "estimate", "exp(coef)", "conf.low","conf.high")

# 2.2.1 Make the plot
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


#### FIGURE 2: Coefficients plot -----------------------------------------------
png(filename = "Figures/FIG2_coefs.png", width = 13, height = 7, units = "in", res = 600)
grid.draw(cbind(ggplotGrob(coefPlot.M), ggplotGrob(coefPlot.F), size = "last"))
dev.off()


## 2.3 MALE - SPL*WIND DIR PREDICTIONS -----------------------------------------

# 2.3.0 Get predictions - SPL * wind direction
pred_plot.M2 <- sjPlot::plot_model(H_SPL.M, type = "int")[[1]]
pred_plot.M2 <- data.frame(pred_plot.M2$data)

# 2.3.1 Get scaling attributes for SPL
#att_SPL <- attributes(scale(modDat.M$abs_SPL_2000dB_std, center = TRUE, scale = TRUE))
#mylabels_SPL <- seq(50,75,5)
#mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]


# 2.3.2 Make the plot

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

pred_M.SPL <- ggplot() + 
  geom_ribbon(data = pred_plot.M2, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                       group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M2, aes(x = x, y = predicted, group = group, col = group), size = 1) +
 # scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_y_continuous(breaks = c(2,4,6,8), limits = c(0,8)) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "Odds Ratio", x = "Standardised Sound Pressure Level (dB)", tag = "(a)", title = "Male") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.18,0.9),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0,
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2)))


## 2.4 FEMALE - SPL*WIND DIR PREDICTIONS ---------------------------------------

# 2.4.0 Get predictions - SPL * wind direction
pred_plot.F2 <- sjPlot::plot_model(H_SPL.F, type = "int")[[1]]
pred_plot.F2 <- data.frame(pred_plot.F2$data)

# 2.4.1 Get scaling attributes for SPL
#att_SPL <- attributes(scale(modDat.F$abs_SPL_2000dB_std.OG, center = TRUE, scale = TRUE))
#mylabels_SPL <- seq(50,75,5)
#mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

# 2.4.2 Make the plot

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

pred_F.SPL <- ggplot() + 
  geom_ribbon(data = pred_plot.F2, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                       group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F2, aes(x = x, y = predicted, group = group, col = group), size = 1) +
 # scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_y_continuous(breaks = c(2,4,6,8), limits = c(0,8)) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "", x = "Standardised Sound Pressure Level (dB)", tag = "(b)", title = "Female") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = "none",
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2)))

## 2.5 MALE - wind direction * speed predictions -------------------------------

# 2.5.0 Get predictions - wind speed * wind direction
pred_plot.M <- sjPlot::plot_model(H_SPL.M, type = "int")[[3]]
pred_plot.M <- data.frame(pred_plot.M$data)

# 2.5.2 Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.M$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

# 2.5.3 Make the plot
pred_M.wind <- ggplot() + 
  geom_ribbon(data = pred_plot.F, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "Odds Ratio", x = "Wind Direction (°)", tag = "(c)", title = "") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.82,0.9),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0,
        plot.tag = element_text(size = 22))
dev.off()

## 2.6 FEMALE - wind direction * speed predictions -----------------------------

# 2.6.0 Get predictions - wind speed * wind direction
pred_plot.F <- sjPlot::plot_model(H_SPL.F, type = "int")[[3]]
pred_plot.F <- data.frame(pred_plot.F$data)

# 2.6.1 Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.F$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

# 2.6.2 Make the plot

# Set colours
high_wind <- "#440154FF" # purple
low_wind <- "#FDE725FF"  # yellow

pred_F.wind.base <- ggplot() + 
  geom_ribbon(data = pred_plot.F, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "", x = "Wind Direction (°)", tag = "(d)", title = "") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = "none",
        plot.tag = element_text(size = 22))

# 2.6.3 Add orientation schematic to figure
pred_F.wind <- ggdraw() + 
    draw_plot(pred_F.wind.base) +
    draw_image(
    "Figures/orientation_schematic.png", x = 0.95, y = 0.87, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.3, height = 0.3 )


#### FIGURE 3: Predictions plot ------------------------------------------------

png(filename = "Figures/FIG3_predictions.png", width = 15, height = 15, units = "in", res = 700)
ggarrange(pred_M.SPL, pred_F.SPL, pred_M.wind, pred_F.wind, ncol = 2, nrow = 2)
dev.off()

### 3.0 Supplementary materials ------------------------------------------------

## 3.1 Build the trip models  --------------------------------------------------

### wind + SPL + trip ###
H_SPLTrip.F <- clogit(case ~ abs_SPL_2000dB_std*relDir + abs_SPL_2000dB_std:WindSp + relDir:WindSp +
                        abs_SPL_2000dB_std:Trip_state + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.F)
summary(H_SPLTrip.F) 

H_SPLTrip.M <- clogit(case ~ abs_SPL_2000dB_std*relDir + abs_SPL_2000dB_std:WindSp + relDir:WindSp +
                        abs_SPL_2000dB_std:Trip_state + strata(pointID), cluster = birdID, 
                      robust = TRUE, method = 'approximate', data = modDat.M)
summary(H_SPLTrip.M) 


## 3.2 Compare models using QIC weights ----------------------------------------
hab::QIC(H_wind.F, H_SPL.F, H_SPLTrip.F) 

#                 QIC   QuasiLL     n nevent K    Trace   deltaQIC       weight
#H_wind.F    8838.760 -4417.141 21480   1790 2 2.238461 22.5346103 7.711646e-06
#H_SPL.F     8817.063 -4402.695 21480   1790 5 5.836389  0.8378363 3.967726e-01
#H_SPLTrip.F 8816.225 -4398.154 21480   1790 7 9.958101  0.0000000 6.032197e-01

summary(H_SPLTrip.F)

hab::QIC(H_wind.M, H_SPL.M, H_SPLTrip.M) 

#                 QIC   QuasiLL     n nevent K     Trace deltaQIC       weight
#H_wind.M    6621.018 -3308.577 16020   1335 2  1.932559 45.96252 9.873877e-11
#H_SPL.M     6575.056 -3278.989 16020   1335 5  8.538800  0.00000 9.443285e-01
#H_SPLTrip.M 6580.718 -3277.238 16020   1335 7 13.120931  5.66201 5.567153e-02

summary(H_SPL.M)

# 3.2.1 Get mean SPL for each trip stage for males
tapply(modDat$abs_SPL_2000dB_std, modDat$Trip_state, function(x) c(mean(x), sd(x))) 


