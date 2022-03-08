
### Load packages
packages <- c("lme4", "nlme", "MuMIn", "ggplot2", "tidyverse", "brms", "loo")

invisible(lapply(packages, library, character.only = TRUE))

### Create outputs folder
out.path <- "./Data_outputs/"
if(dir.exists(out.path) == FALSE){
  dir.create(out.path)
}


# Data processing ---------------------------------------------------------

modDat <- data.table::fread("Data_inputs/GPS_2013_model-data-ORIGINAL.csv", data.table = F)

## Check variable encoding
vars <- c("Sex", "Trip_state", "TripID", "pointID", "birdID")
modDat[vars] <- lapply(modDat[vars], factor)


# Visualizing the data -------------------------------------------------------

# Cone selection - effects look very marginal 
SPL <- ggplot(aes(y = abs_SPL_2000dB, x = factor(cone_ID)), data = modDat) + 
  geom_boxplot() + coord_flip()
dir <- ggplot(aes(y = relDir, x = factor(cone_ID)), data = modDat) + 
  geom_boxplot() + coord_flip()
spd <- ggplot(aes(y = WindSp, x = factor(cone_ID)), data = modDat) + 
  geom_boxplot() + coord_flip()

gridExtra::grid.arrange(SPL, dir, spd, ncol = 2, nrow = 2)



# Build global model in brms --------------------------------------------------------

## Note: for relDir, 0 = crosswind, <0 = headwind, >0 = tailwind


## Scale continuous variables
modDat[,c(3,8,9)] <- lapply(modDat[,c(3,8,9)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 


## Global model with wind variables
tok <- Sys.time()
SPL.wind_global <- brm(cone_ID~abs_SPL_2000dB*Trip_state +
                      abs_SPL_2000dB*relDir +
                      abs_SPL_2000dB*relDir*WindSp +
                      relDir*WindSp +
                      Sex + (1|TripID:pointID),  
                    data = modDat,
                    family = bernoulli(link = "logit"), 
                    warmup = 2000, iter = 4000, 
                    cores = 2, chains = 2, 
                    seed = 123) 
tik <- Sys.time()
tik-tok
 
SPL.wind_global <- add_criterion(SPL.wind_global, "waic")
save(SPL.wind_global, file = "Data_outputs/wind_brm-1.RData")



# Check global model convergence and fit -----------------------------------

## Plot coefficient values and chains
plot(SPL.wind_global)

## Caterpillar plot for convergence 

SPL_brm.tranformed <- ggmcmc::ggs(SPL.wind_global)

ggplot(filter(SPL_brm.tranformed, Parameter %in% c("b_Intercept", "b_abs_SPL_2000dB", "b_SexM")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")

## pp check to assess fit to data
pp_check(SPL.wind_global, ndraws = 1000)

## Check goodness of fit
bayes_R2(SPL.wind_global)



# Model comparison - run reduced models --------------------------------------------------------

## Model 2: no trip state
SPL.wind_noState <- brm(cone_ID~abs_SPL_2000dB*relDir +
                         abs_SPL_2000dB*relDir*WindSp +
                         relDir*WindSp +
                         Sex + (1|TripID:pointID),  
                       data = modDat,
                       family = bernoulli(link = "logit"), 
                       warmup = 2000, iter = 4000, 
                       cores = 2, chains = 2, 
                       seed = 123) 

SPL.wind_noState <- add_criterion(SPL.wind_noState, "waic")
save(SPL.wind_noState, file = "Data_outputs/wind_brm-2.RData")


## Model 3: no SPL * dir 
SPL.wind_noDir <- brm(cone_ID~abs_SPL_2000dB*relDir*WindSp +
                          relDir*WindSp +
                          Sex + (1|TripID:pointID),  
                        data = modDat,
                        family = bernoulli(link = "logit"), 
                        warmup = 2000, iter = 4000, 
                        cores = 2, chains = 2, 
                        seed = 123) 

SPL.wind_noDir <- add_criterion(SPL.wind_noDir, "waic")
save(SPL.wind_noDir, file = "Data_outputs/wind_brm-3.RData")


## Model 4: no SPL * dir * speed
SPL.wind_noDirSpeed <- brm(cone_ID~abs_SPL_2000dB*relDir +
                          relDir*WindSp +
                          Sex + (1|TripID:pointID),  
                        data = modDat,
                        family = bernoulli(link = "logit"), 
                        warmup = 2000, iter = 4000, 
                        cores = 2, chains = 2, 
                        seed = 123) 

SPL.wind_noDirSpeed <- add_criterion(SPL.wind_noDirSpeed, "waic")
save(SPL.wind_noDirSpeed, file = "Data_outputs/wind_brm-4.RData")

## Model 5: no SPL * dir AND no SPL * dir * speed
SPL.wind_noWind <- brm(cone_ID~abs_SPL_2000dB +
                          relDir*WindSp +
                          Sex + (1|TripID:pointID),  
                        data = modDat,
                        family = bernoulli(link = "logit"), 
                        warmup = 2000, iter = 4000, 
                        cores = 2, chains = 2, 
                        seed = 123) 

SPL.wind_noWind <- add_criterion(SPL.wind_noWind, "waic")
save(SPL.wind_noWind, file = "Data_outputs/wind_brm-5.RData")

## Model 6: quadratic SPL 
# Notation for quadratic x2 interacting with x1: ~ x1 + x2 + I(x2^2) + x1:x2 + x1:I(x2^2)

SPL.wind_quadSPL <- brm(cone_ID ~ relDir + abs_SPL_2000dB + abs_SPL_2000dB:relDir +
                          relDir:I(abs_SPL_2000dB^2) +
                          WindSp + 
                          abs_SPL_2000dB:relDir:WindSp +
                          relDir:WindSp:I(abs_SPL_2000dB^2) +
                          relDir*WindSp +
                          Sex + (1|pointID),  
                        data = modDat,
                        family = bernoulli(link = "logit"), 
                        warmup = 2000, iter = 4000, 
                        cores = 2, chains = 2, 
                        seed = 123) 

SPL.wind_quadSPL <- add_criterion(SPL.wind_quadSPL, "waic")
save(SPL.wind_quadSPL, file = "Data_outputs/wind_brm-6.RData")



# Model comparison - compare wAIC -----------------------------------------

modelFiles <- list("wind_brm-1.RData", "wind_brm-2.RData", "wind_brm-3.RData", "wind_brm-4.RData", "wind_brm-5.RData", "wind_brm-6.RData")
modelFiles <- as.list(paste0("Data_outputs/", modelFiles))
lapply(modelFiles, load, .GlobalEnv)

print(loo_compare(SPL.wind_noState, SPL.wind_noDir, SPL.wind_noDirSpeed, SPL.wind_noWind, SPL.wind_quadSPL,
                  criterion = "waic"), simplify = FALSE)

## Quadratic SPL comes out as top, but no effect of quadratic terms - suspect its
## low AIC is due to overfitting, so sticking to original

# Check selected model convergence and fit -----------------------------------

load("Data_outputs/wind_brm-2.RData")
summary(SPL.wind_noState)

## App to interrogate model
launch_shinystan(SPL.wind_noState)

## Plot coefficient values and chains
plot(SPL.wind_noState)

## Caterpillar plot for convergence 

SPL_brm.tranformed <- ggmcmc::ggs(SPL.wind_noState)

ggplot(filter(SPL_brm.tranformed, Parameter %in% c("b_Intercept", "b_abs_SPL_2000dB", "b_SexM")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")

## pp check to assess fit to data
pp_check(SPL.wind_noState, ndraws = 1000)

## Check goodness of fit
bayes_R2(SPL.wind_noState)

## Plot all coefficient values
mcmc_plot(SPL.wind_quadSPL) 


# Density plot to assess parameter importance -----------------------------

## INTERCEPT ##

ggplot(filter(SPL_brm.tranformed,Parameter == "b_Intercept", Iteration > 1000),
       aes(x = value))+
  geom_density(fill  = "yellow", alpha = .5) +
  geom_vline(xintercept = 0, col  = "red", size = 1)+
  scale_x_continuous(name = "Value") + 
  geom_vline(xintercept = as.numeric(summary(SPL.wind_brm)$fixed[1,3:4]),
             col = "blue",linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Intercept")

## SEX ##

ggplot(filter(SPL_brm.tranformed,Parameter == "b_SexM", Iteration > 1000),
       aes(x = value))+
  geom_density(fill  = "yellow", alpha = .5) +
  geom_vline(xintercept = 0, col  = "red", size = 1)+
  scale_x_continuous(name = "Value") + 
  geom_vline(xintercept = as.numeric(summary(SPL.wind_brm)$fixed[5,3:4]),
             col = "blue",linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Intercept")

## SPL ##

ggplot(filter(SPL_brm.tranformed,Parameter == "b_abs_SPL_2000dB", Iteration > 1000),
       aes(x = value))+
  geom_density(fill  = "yellow", alpha = .5) +
  geom_vline(xintercept = 0, col  = "red", size = 1)+
  scale_x_continuous(name = "Value") + 
  geom_vline(xintercept = as.numeric(summary(SPL.wind_brm)$fixed[2,3:4]),
             col = "blue",linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Intercept")



# Effect of infrasound on cone selection ----------------------------------

library(tidybayes)

SPL.wind_noState %>%
  spread_draws(b_Intercept, b_abs_SPL_2000dB) %>%
  mutate(abs_SPL_2000dB = list(seq(-6, 4, 0.5))) %>% #the observed value range of IF
  unnest(abs_SPL_2000dB) %>%
  mutate(pred = exp(b_Intercept + b_abs_SPL_2000dB*abs_SPL_2000dB)/(1+exp(b_Intercept + b_abs_SPL_2000dB*abs_SPL_2000dB))) %>%
  group_by(abs_SPL_2000dB) %>%
  summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = abs_SPL_2000dB, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha=0.2) +
  ylab("Probability of selecting cone") 

library(modelr)

## Get conditional effects
p <- conditional_effects(SPL.wind_noState)
plot(p)[[1]]


## Using sjPlot

sjPlot::plot_model(SPL.wind_noState, 
                                     type = "pred", terms = "abs_SPL_2000dB")

sjPlot::plot_model(SPL.wind_noState, 
                                     type = "pred", 
                                     terms = c("WindSp", "relDir"))


# Plot effects ------------------------------------------------------------

## Significant effects = SPL and dir*speed

# Get model data
plotDat_SPL <- data.frame(p[[1]])

# Set selected cones (1) to max y axis
modDat$dummy_cone <- 0
modDat$dummy_cone[modDat$cone_ID == 1] <- 0.12

### SPL

# Get scaling attributes for SPL
att_SPL <- attributes(scale(modDat$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
mylabels_SPL <- seq(50,80,5)
mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

png(filename = "Figures/FIGX_SPL-prob.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_count(data = modDat, aes(x = abs_SPL_2000dB, y = dummy_cone), alpha = 0.2) +
  geom_ribbon(data = plotDat, aes(x = abs_SPL_2000dB, ymin = lower__, ymax = upper__),
              alpha = 0.5, fill = "grey") +
  geom_line(data = plotDat, aes(x = abs_SPL_2000dB, y = estimate__), size = 1) +
  scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_y_continuous(breaks = c(0, 0.04, 0.08, 0.12), limits = c(0,0.12)) +
  labs(y = "Probability of Cone Selection", x = "Sound Pressure Level (dB)") +
  theme_bw() +
    theme(axis.text.x=element_text(size=16), 
          axis.text.y=element_text(size=16), 
          axis.title.x=element_text(size=18),
          axis.title.y=element_text(size=18),
          legend.position = "none")
dev.off()



### Wind direction * speed

plotDat_wind <- sjPlot::plot_model(SPL.wind_noState, 
           type = "pred", 
           terms = c("WindSp", "relDir"))
plotDat_wind <- data.frame(plotDat_wind$data)

# Get scaling attributes for speed
att_SPD <- attributes(scale(modDat$WindSp_OG, center = TRUE, scale = TRUE))
mylabels_SPD <- seq(0, 25, 5)
mybreaks_SPD <- scale(mylabels_SPD, att_SPD$`scaled:center`, att_SPD$`scaled:scale`)[,1]

# Set selected cones (1) to max y axis
modDat$dummy_cone[modDat$cone_ID == 1] <- 0.14

# Specify colours
tail_col <- "#D81B60"
cross_col <- "#1E88E5"
head_col <- "#FFC107"


png(filename = "Figures/FIGX_wind-prob.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_count(data = modDat, aes(x = WindSp, y = dummy_cone), alpha = 0.1) +
  geom_ribbon(data = plotDat_wind, aes(x = x, ymin = conf.low, ymax = conf.high, group = group),
              alpha = 0.5, fill = "grey") +
  geom_line(data = plotDat_wind, aes(x = x, y = predicted, group = group_col,
                                     col = group_col), size = 1) +
  scale_x_continuous(labels = mylabels_SPD, breaks = mybreaks_SPD) +
  scale_y_continuous(breaks = c(0, 0.04, 0.08, 0.12), limits = c(0,0.14)) +
  scale_colour_viridis_d(labels = c("Headwind", "Crosswind", "Tailwind")) +
  labs(y = "Probability of Cone Selection", x = "Wind Speed (m/s)", col = "Relative Wind Direction") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.position = "none")
dev.off()

