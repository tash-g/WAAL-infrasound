
library(lme4); library(nlme); library(MuMIn)

mydat <- data.table::fread("Data_inputs/gps_2013_cones_with_wind.csv")


# Data processing ---------------------------------------------------------

## Encode focal cone as '1' and non-focal as '0'
mydat$cone_ID <- abs(1-mydat$cone_ID)

## Remove NA values
mydat.noNA <- subset(mydat, !is.na(abs_SPL_2000dB) & !is.na(Sex) &
                            !is.na(Trip_state) & !is.na(TripID) &
                            !is.na(cone_ID))

## Remove inf values
mydat.noNA <- subset(mydat.noNA, !is.infinite(abs_SPL_2000dB))

## Standardise continuous variables
mydat.noNA$abs_SPL_2000dBsc <- scale(mydat.noNA$abs_SPL_2000dB, center = T, scale = T)

## Make counter/trip ID variable (think this is essentially cone ID)
mydat.noNA$counter_TripID <- paste(mydat.noNA$TripID, mydat.noNA$counter, sep = ".")

## Isolate important variables 
modDat <- mydat.noNA[,c(1,5, 11, 13, 14, 16, 24, 25, 27, 28, 29)] # tripID, coneID, abs_SPL_2000db, birdID, Sex, TripState, WindSp, relDir_adj.bearing, abs_SPL_2000dBsc, counter_TripID
names(modDat)[9] <- "relDir"

## Check variable encoding
modDat$Sex <- as.factor(modDat$Sex)
modDat$Trip_state <- as.factor(modDat$Trip_state)
modDat$TripID <- as.factor(modDat$TripID)



# Visualizing the data -------------------------------------------------------

# Cone selection
SPL <- ggplot(aes(x = abs_SPL_2000dB, y = cone_ID), data = modDat) + 
  geom_point()
dir <- ggplot(aes(x = relDir, y = cone_ID), data = modDat) + 
  geom_point()
spd <- ggplot(aes(x = WindSp, y = cone_ID), data = modDat) + 
  geom_point()

gridExtra::grid.arrange(SPL, dir, spd, ncol = 2, nrow = 2)



# Build global model in brms --------------------------------------------------------

library(brms); library(loo)

## Scale the variables
modDat[,c(3,8,9)] <- lapply(modDat[,c(3,8,9)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 

## Note: 0 = crosswind, neg = headwind, pos = tailwind

## Global model with wind variables
tok <- Sys.time()
SPL.wind_global <- brm(cone_ID~abs_SPL_2000dB*Trip_state +
                      abs_SPL_2000dB*relDir +
                      abs_SPL_2000dB*relDir*WindSp +
                      relDir*WindSp +
                      Sex + (1|counter_TripID),  
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

library(ggplot2); library(tidyverse)

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
                         Sex + (1|counter_TripID),  
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
                          Sex + (1|counter_TripID),  
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
                          Sex + (1|counter_TripID),  
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
                          Sex + (1|counter_TripID),  
                        data = modDat,
                        family = bernoulli(link = "logit"), 
                        warmup = 2000, iter = 4000, 
                        cores = 2, chains = 2, 
                        seed = 123) 

SPL.wind_noWind <- add_criterion(SPL.wind_noWind, "waic")
save(SPL.wind_noWind, file = "Data_outputs/wind_brm-5.RData")


# Model comparison - compare wAIC -----------------------------------------

modelFiles <- list("wind_brm-1.RData", "wind_brm-2.RData", "wind_brm-3.RData", "wind_brm-4.RData", "wind_brm-5.RData")
modelFiles <- as.list(paste0("Data_outputs/", modelFiles))
lapply(modelFiles, load, .GlobalEnv)

print(loo_compare(SPL.wind_noState, SPL.wind_noDir, SPL.wind_noDirSpeed, SPL.wind_noWind, criterion = "waic"), simplify = FALSE)

## Little difference between them all; choosing to retain wind variables as of biological interest but not trip state


# Check selected model convergence and fit -----------------------------------

## Plot coefficient values and chains
plot(SPL.wind_noState)

## Caterpillar plot for convergence 

SPL_brm.tranformed <- ggmcmc::ggs(SPL.wind_noState)

library(ggplot2); library(tidyverse)

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
