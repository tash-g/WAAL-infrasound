## ---------------------------
##
## Script name: 3-fit_conditional_logit_models.R
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
              "tibble", "ggpubr", "cowplot", "patchwork")

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

#if (dir.exists(out.path) == FALSE) {
#  dir.create(out.path)
#}

## 0.2 Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/WAAL_2013_gps_processed_aperture60deg.csv", 
                            data.table = F)

# 0.2.1 Rename and process variables
modDat <- rename(modDat, case = segment_ID)

factor_vars <- c("TripID", "birdID", "Sex", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# 0.2.2 Retain original variables for scaling later
modDat$abs_SPL_2000dB_std.OG <- modDat$abs_SPL_2000dB_std
modDat$relDir.OG <- modDat$relDir
modDat$WindSp.OG <- modDat$WindSp

# 0.2.3 Remove NA variables
modDat <- modDat[!is.na(modDat$abs_SPL_2000dB_std),]

## 0.3 Check data structure ----------------------------------------------------

# 0.3.0 Check only have 1s and 0s as cases
table(modDat$case)

# 0.3.1 Check sum of cases within stratas = 1
table(tapply(modDat$case, modDat$pointID, sum)) 

# 0.3.2 Check each strata has 6 cones 
table(table(modDat$pointID))

# 0.3.3 Check total number of stratas
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

# 1.0.0 Scale continuous variables and remove NA variables
vars_scale <- c("WindSp", "relDir", "abs_SPL_2000dB_std")
modDat[, vars_scale] <-
  lapply(modDat[, vars_scale], function(x)
    c(scale(x, center = TRUE, scale = TRUE)))

# 1.0.1 Separate males and females
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

#             QIC   QuasiLL     n nevent K    Trace deltaQIC       weight
#H_wind.F 6360.420 -3177.983 10746   1791 2 2.226734 26.54861 1.718072e-06
#H_SPL.F  6333.872 -3161.075 10746   1791 5 5.860984  0.00000 9.999983e-01

hab::QIC(H_wind.M, H_SPL.M) 

#              QIC   QuasiLL    n nevent K    Trace deltaQIC       weight
#H_wind.M 4770.176 -2383.153 8010   1335 2 1.934830 50.35415 1.163417e-11
#H_SPL.M  4719.822 -2351.242 8010   1335 5 8.669667  0.00000 1.000000e+00

# 1.2.1 Get summaries from best supported models
summary(H_SPL.F)

#                          exp(coef) exp(-coef) lower .95 upper .95
#abs_SPL_2000dB_std           1.0359     0.9654    0.9881    1.0860
#relDir                       0.8359     1.1964    0.7931    0.8809
#abs_SPL_2000dB_std:relDir    0.8555     1.1690    0.8030    0.9114
#abs_SPL_2000dB_std:WindSp    1.0432     0.9586    0.9838    1.1061
#relDir:WindSp                0.9387     1.0653    0.8952    0.9842    

summary(H_SPL.M)

#                          exp(coef) exp(-coef) lower .95 upper .95
#abs_SPL_2000dB_std           1.1923     0.8387    1.0969    1.2961
#relDir                       0.9692     1.0318    0.9169    1.0245
#abs_SPL_2000dB_std:relDir    0.8498     1.1767    0.7747    0.9322
#abs_SPL_2000dB_std:WindSp    0.9965     1.0035    0.9106    1.0905
#relDir:WindSp                0.9196     1.0875    0.8764    0.9648


### 2.0 Visualise effects ------------------------------------------------------

# 2.0.0 Get focal vs non-focal SPL

# 2.0.0.0 Standardised SPL
stdSPL_comp <- with(modDat, 
                        aggregate(abs_SPL_2000dB_std.OG ~ case + Sex, 
                              FUN =  function(x) c( SD = sd(x), MN = mean(x) ) ) ) 
stdSPL_comp <- do.call(data.frame, stdSPL_comp)

stdSPL_comp %>% group_by(Sex) %>% summarise(diff = abs_SPL_2000dB_std.OG.MN[2] - abs_SPL_2000dB_std.OG.MN[1],
                                            diff_SD = abs_SPL_2000dB_std.OG.SD[2] - abs_SPL_2000dB_std.OG.SD[1])

#Sex     diff diff_SD
#<fct>  <dbl>   <dbl>
#1 F     0.0624 -0.0302
#2 M     0.207  -0.0510

# 2.0.0.1 Absolute SPL
stdSPL <- with(modDat, aggregate(abs_SPL_2000dB ~ case + Sex, FUN =  function(x) c( SD = sd(x), MN = mean(x) ) ) )
stdSPL <- do.call(data.frame, stdSPL)

stdSPL %>% group_by(Sex) %>% summarise(diff = abs_SPL_2000dB.MN[2] - abs_SPL_2000dB.MN[1],
                                            diff_SD = abs_SPL_2000dB.SD[2] - abs_SPL_2000dB.SD[1])

#Sex    diff diff_SD
#<fct> <dbl>   <dbl>
#1 F     0.106  0.0130
#2 M     0.276 -0.109 

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
  geom_vline(xintercept = 1, colour = "blue", size = 0.2) +
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
  geom_vline(xintercept = 1, colour = "blue", size = 0.2) +
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

# 2.3.1 Make the plot

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

pred_M.SPL_plot <- ggplot() + 
  geom_ribbon(data = pred_plot.M2, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                       group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M2, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(limits = c(-3,2)) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "Odds Ratio", x = "Standardised Sound Pressure Level (dB)") +
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
        ) +
  guides(color = guide_legend(override.aes = list(size = 2)))

# 2.3.2 Add histogram of SPL to plot
pred_M.SPL_dens <- ggplot(data = subset(modDat.M, case == 1), aes(x = abs_SPL_2000dB_std)) + 
   geom_histogram(color = "black", fill = "white") + 
   theme_void() +
   labs(title = "Male", tag = "(a)") +
   theme(plot.title = element_text(face = "bold", size = 20),
        plot.tag = element_text(size = 22))

pred_M.SPL <- pred_M.SPL_dens + pred_M.SPL_plot + plot_layout(ncol = 1, nrow = 3, heights = c(0.25,4))

## 2.4 FEMALE - SPL*WIND DIR PREDICTIONS ---------------------------------------

# 2.4.0 Get predictions - SPL * wind direction
pred_plot.F2 <- sjPlot::plot_model(H_SPL.F, type = "int")[[1]]
pred_plot.F2 <- data.frame(pred_plot.F2$data)

# 2.4.1 Make the plot

# Set colours
tailwind <- "#404788FF" # blue/purple
headwind <- "#73D055FF"  # green

pred_F.SPL_plot <- ggplot() + 
  geom_ribbon(data = pred_plot.F2, aes(x = x, ymin = conf.low, ymax = conf.high, 
                                       group = group), alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F2, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(0,5)) +
  scale_x_continuous(limits = c(-3,2)) +
  scale_colour_manual("Wind Direction", values = c(tailwind, headwind), labels = c("Tailwind (0°)", "Headwind (180°)")) +
  labs(y = "", x = "Standardised Sound Pressure Level (dB)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = "none",
        plot.tag = element_text(size = 22)) +
  guides(color = guide_legend(override.aes = list(size = 2)))

# 2.4.2 Add histogram of SPL to plot
pred_F.SPL_dens <- ggplot(data = subset(modDat.F, case == 1), aes(x = abs_SPL_2000dB_std)) + 
  geom_histogram(color = "black", fill = "white") + 
  theme_void() +
  labs(tag = "(b)", title = "Female") +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.tag = element_text(size = 22))

pred_F.SPL <- pred_F.SPL_dens + pred_F.SPL_plot + plot_layout(ncol = 1, nrow = 3, heights = c(0.25,4))


## 2.5 MALE - wind direction * speed predictions -------------------------------

# 2.5.0 Get predictions - wind speed * wind direction
pred_plot.M <- sjPlot::plot_model(H_SPL.M, type = "int")[[3]]
pred_plot.M <- data.frame(pred_plot.M$data)

# 2.5.1 Get scaling attributes for windDir
att_windDir <- attributes(scale(modDat.M$relDir.OG, center = TRUE, scale = TRUE))
mylabels_windDir <- seq(0,180,45)
mybreaks_windDir <- scale(mylabels_windDir, att_windDir$`scaled:center`, att_windDir$`scaled:scale`)[,1]

# 2.5.2 Make the plot

# Set colours
high_wind <- "#440154FF" # purple
low_wind <- "#FDE725FF"  # yellow

pred_M.wind_plot <- ggplot() + 
  geom_ribbon(data = pred_plot.M, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.M, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "Odds Ratio", x = "Wind Direction (°)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = c(0.82,0.9),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text.align = 0,
        plot.tag = element_text(size = 22))

# 2.5.3 Add histogram of wind directions to plot
pred_M.wind_dens <- ggplot(data = subset(modDat.M, case == 1), aes(x = relDir)) + 
  geom_histogram(color = "black", fill = "white") + 
  theme_void() +
  labs(tag = "(c)") +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.tag = element_text(size = 22))

pred_M.wind <- pred_M.wind_dens + pred_M.wind_plot + plot_layout(ncol = 1, nrow = 3, heights = c(0.25,4))


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

pred_F.wind_base <- ggplot() + 
  geom_ribbon(data = pred_plot.F, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = pred_plot.F, aes(x = x, y = predicted, group = group, col = group), size = 1) +
  scale_colour_manual("Wind Speed", values = c(low_wind, high_wind), labels = c(expression ("Low (<1"~ms^{-1}~")"), 
                                                                                expression ("High (>18"~ms^{-1}~")"))) +
  scale_x_continuous(labels = mylabels_windDir, breaks = mybreaks_windDir) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "", x = "Wind Direction (°)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=16), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        plot.title = element_text(face = "bold", size = 20),
        legend.position = "none",
        plot.tag = element_text(size = 22))

# 2.6.3 Add histogram of wind directions to figure
pred_F.wind_dens <- ggplot(data = subset(modDat.F, case == 1), aes(x = relDir)) + 
  geom_histogram(color = "black", fill = "white") + 
  theme_void() +
  labs(tag = "(d)") +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.tag = element_text(size = 22))

pred_F.wind_plot <- pred_F.wind_dens + pred_F.wind_base + plot_layout(ncol = 1, nrow = 3, heights = c(0.25,4))

# 2.6.4 Add orientation schematic to figure
pred_F.wind <- ggdraw() + 
    draw_plot(pred_F.wind_plot) +
    draw_image(
    "Figures/orientation_schematic.png", x = 0.95, y = 0.87, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.3, height = 0.3 )



#### FIGURE 3: Predictions plot ------------------------------------------------

png(filename = "Figures/FIG3_predictions.png", width = 15, height = 15, units = "in", res = 700)
ggarrange(pred_M.SPL, pred_F.SPL, pred_M.wind, pred_F.wind, ncol = 2, nrow = 2)
dev.off()

