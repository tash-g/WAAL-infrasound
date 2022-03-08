## Conditional logistic regression may be an option to solve the problem that the current glmer
## doesn't actually link the 12 cones to one another. See definition:
"Conditional logistic regression (CLR) is a specialized type of logistic regression usually employed when 
case subjects with a particular condition or attribute are each matched with n control subjects without the 
condition. In general, there may be 1 to m cases matched with 1 to n controls."

## Let's give it a try!

library(survival); library(ggplot2); library(gridExtra)

# Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/GPS_2013_model-data-ORIGINAL.csv", data.table = F)

# Rename and process variables
names(modDat)[2] <- "case"
modDat$case <- ifelse(modDat$case == 1, TRUE, FALSE)

factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)

# Retain original variables for scaling later
modDat$abs_SPL_2000dB.OG <- modDat$abs_SPL_2000dB
modDat$relDir.OG <- modDat$relDir

# Run conditional logistic regression -------------------------------------

## Scale continuous variables and remove NA variables
modDat[,c(3,7,8)] <- lapply(modDat[,c(3,7,8)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 
modDat.naomit <- na.omit(modDat)

## Build global model
RSF_global <- clogit(case ~ strata(pointID) + abs_SPL_2000dB*relDir +
                  cluster(birdID), method = 'approximate', data = modDat)

summary(RSF_global)

# NOTE: No interaction effect, so remove

## Build reduced model
RSF_reduced <- clogit(case ~ strata(pointID) + abs_SPL_2000dB + relDir +
                       cluster(birdID), method = 'approximate', data = modDat)

summary(RSF_reduced)


### Trying with mclogit
library(mclogit)

RSF_Mclogit <- mclogit(cbind(case, pointID) ~ abs_SPL_2000dB*relDir, random = ~1|birdID, data = modDat)


# Compare data with different cone apertures ------------------------------

# Preallocate lists

apertures <- c(15 , 20, 30, 45, 60, 90) 
model_list <- vector(mode = "list", length = length(apertures))
p_values <- vector(mode = "list", length = length(apertures))
aperture_plots <- vector(mode = "list", length = length(apertures))

for (i in 1:length(apertures)){ 
  
  modDat <- data.table::fread(paste0("Data_inputs/GPS_2013_model-data-APERTURE", apertures[i], ".csv"), 
                              stringsAsFactors = F, data.table = F)
  
  ## Check variable encoding
  names(modDat)[2] <- "case"
  modDat$case <- ifelse(modDat$case == 1, TRUE, FALSE)
  
  factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
  modDat[factor_vars] <- lapply(modDat[factor_vars], factor)
  
  ## Scale continuous variables
  modDat[,c(3,7,8)] <- lapply(modDat[,c(3,7,8)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 
  
  ## Fit reduced model
  model_list[[i]] <- clogit(case ~ strata(pointID) + abs_SPL_2000dB + relDir + cluster(birdID), method = 'approximate', data = modDat)
 
  ## Extract p values for each coefficient
  p_values[[i]] <- data.frame(coef(summary(model_list[[i]]))[,6])
  p_values[[i]]$aperture <- apertures[i]
  
  ## Construct plot of effects
  my_sjplot <- sjPlot::plot_model(model_list[[i]], title=paste0("SSF Coefficients - ", apertures[i], " degrees"))
  my_sjplot.data <- as.data.frame(my_sjplot$data)
  my_sjplot.data <- subset(my_sjplot.data, term != "WindSp") # Wind speed is same for all cones, so not interested in effect
  
  RSF_plot <- ggplot() +
    geom_point(data = my_sjplot.data, aes(x = estimate, y = term)) + 
    geom_errorbar(data = my_sjplot.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
    geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
    theme_bw() + labs(title = paste0("SSF Coefficients - ", apertures[i], " degrees"))
  
  aperture_plots[[i]] <- RSF_plot

}


## Plot p values for each term with changing aperture

# Process the data
model_ps <- do.call("rbind", p_values)
model_ps <- cbind(rownames(model_ps), data.frame(model_ps, row.names=NULL))
colnames(model_ps) <- c("parameter", "p_value", "aperture")
model_ps$parameter <- gsub('[[:digit:]]+', '', model_ps$parameter)

# Extract parameters of interest
pars <- c("abs_SPL_dB", "relDir", "relDir:WindSp")
plot_ps <- model_ps[model_ps$parameter %in% pars,]

# Make the plot
png(filename = "Figures/FIGSX_compare-apertures-pValues.png", width = 9, height = 7, units = "in", res = 600)
ggplot(data = plot_ps, aes(x = aperture, y = p_value, col = parameter, group = parameter)) + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = apertures) +
  scale_colour_discrete(name = "Parameter", labels = c("SPL", "Wind Direction", "Wind Direction*Speed")) +
  theme_bw() + theme(legend.position = c(0.9, 0.9),
                     axis.text.x=element_text(size=16), 
                     axis.text.y=element_text(size=16), 
                     axis.title.x=element_text(size=18),
                     axis.title.y=element_text(size=18)) +
  labs(x = "Aperture Size (degrees)", y = "p value")
dev.off()

## Plot coefficient effect sizes for each aperture
png(filename = "Figures/FIGSX_compare-apertures-effectSizes.png", width = 9, height = 7, units = "in", res = 600)
do.call("grid.arrange", aperture_plots)
dev.off()



# Visualise effects -------------------------------------------------------

RSF_plot <- sjPlot::plot_model(RSF_reduced, title="SSF Coefficients")
RSF_plot.data <- as.data.frame(RSF_plot$data)
RSF_plot.data <- subset(RSF_plot.data, term != "WindSp")

## Coefficients plot
png(filename = "Figures/FIGX_coefs.png", width = 9, height = 7, units = "in", res = 600)
ggplot() +
  geom_point(data = RSF_plot.data, aes(x = estimate, y = term)) + 
  geom_errorbar(data = RSF_plot.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw()
dev.off()

## Predictive plots: effect of SPL

# Get predictions
SPL_plot <- sjPlot::plot_model(RSF_reduced, type = "pred", terms = "abs_SPL_2000dB")
SPL_plot <- data.frame(SPL_plot$data)

# Set selected cones (1) to max y axis
modDat$dummy_cone <- 0
modDat$dummy_cone[modDat$case == TRUE] <- max(SPL_plot$conf.high)
modDat$dummy_cone[modDat$case == FALSE] <- min(SPL_plot$conf.low)

# Get scaling attributes for SPL
att_SPL <- attributes(scale(modDat$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
mylabels_SPL <- seq(50,80,5)
mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]

png(filename = "Figures/FIGX_SPL-prob.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_count(data = modDat, aes(x = abs_SPL_2000dB, y = dummy_cone), alpha = 0.2) +
  geom_ribbon(data = SPL_plot, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.5, fill = "grey") +
  geom_line(data = SPL_plot, aes(x = x, y = predicted), size = 1) +
  scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) +
  labs(y = "Log Odds Ratio", x = "Sound Pressure Level (dB)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.position = "none")
dev.off()



## Effect of direction

# Get predictions
dir_plot <- sjPlot::plot_model(RSF_reduced, type = "pred", terms = "relDir")
dir_plot <- data.frame(dir_plot$data)

# Set selected cones (1) to max y axis
modDat$dummy_cone <- 0
modDat$dummy_cone[modDat$case == TRUE] <- max(dir_plot$conf.high)
modDat$dummy_cone[modDat$case == FALSE] <- min(dir_plot$conf.low)

# Get scaling attributes for SPL
att_DIR <- attributes(scale(modDat$relDir.OG, center = TRUE, scale = TRUE))
mylabels_DIR <- seq(0,180,90)
mybreaks_DIR <- scale(mylabels_DIR, att_DIR$`scaled:center`, att_DIR$`scaled:scale`)[,1]

png(filename = "Figures/FIGX_dir-prob_legend.png", width = 9, height = 7, units = "in", res = 600)
ggplot() + 
  geom_count(data = modDat, aes(x = relDir, y = dummy_cone), alpha = 0.2) +
  geom_ribbon(data = dir_plot, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.5, fill = "grey") +
  geom_line(data = dir_plot, aes(x = x, y = predicted), size = 1) +
  scale_x_continuous(labels = mylabels_DIR, breaks = mybreaks_DIR) +
  scale_y_continuous(breaks = c(0.8, 1, 1.2, 1.4)) +
  labs(y = "Log Odds Ratio", x = "Relative Wind Direction (°)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18))
dev.off()




# Compare males and females -----------------------------------------------

sexes <- c("F", "M")
RSF_models <- vector("list", length = length(sexes))

for (s in 1:length(sexes)) {
    
    ## Separate male and female data
    sexDat <- subset(modDat, Sex == sexes[s])
    
    # Scale variables
    sexDat[,c(3,8)] <- lapply(sexDat[,c(3,8)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 

    ## Build clogit model 
    RSF_models[[s]] <- clogit(case ~ strata(pointID) + abs_SPL_2000dB + relDir +
                            cluster(birdID), method = 'approximate', data = sexDat)
    
    
    
    ## Make coefficients plot
    
    # Get data
    RSF_plot <- sjPlot::plot_model(RSF_models[[s]], title="SSF Coefficients")
    RSF_plot.data <- as.data.frame(RSF_plot$data)
    
    # Build and save plot
    coefs_plot <- ggplot() +
      geom_point(data = RSF_plot.data, aes(x = estimate, y = term)) + 
      geom_errorbar(data = RSF_plot.data, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
      geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
      theme_bw()
    
    png(filename = paste0("Figures/FIGX_coefs_", sexes[s], ".png"), width = 9, height = 7, units = "in", res = 600)
      print(coefs_plot)
    dev.off()
    
    ## Predictive plots: effect of SPL
    
    # Get predictions
    SPL_plot <- sjPlot::plot_model(RSF_models[[s]], type = "pred", terms = "abs_SPL_2000dB")
    SPL_plot <- data.frame(SPL_plot$data)
    
    # Set selected cones (1) to max y axis
    sexDat$dummy_cone <- 0
    sexDat$dummy_cone[sexDat$case == TRUE] <- max(SPL_plot$conf.high)
    sexDat$dummy_cone[sexDat$case == FALSE] <- min(SPL_plot$conf.low)
    
    # Get scaling attributes for SPL
    att_SPL <- attributes(scale(sexDat$abs_SPL_2000dB.OG, center = TRUE, scale = TRUE))
    mylabels_SPL <- seq(50,80,5)
    mybreaks_SPL <- scale(mylabels_SPL, att_SPL$`scaled:center`, att_SPL$`scaled:scale`)[,1]
    
    SPL_plot <- ggplot() + 
      geom_count(data = sexDat, aes(x = abs_SPL_2000dB, y = dummy_cone), alpha = 0.2) +
      geom_ribbon(data = SPL_plot, aes(x = x, ymin = conf.low, ymax = conf.high),
                  alpha = 0.5, fill = "grey") +
      geom_line(data = SPL_plot, aes(x = x, y = predicted), size = 1) +
      scale_x_continuous(labels = mylabels_SPL, breaks = mybreaks_SPL) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(y = "Log Odds Ratio", x = "Sound Pressure Level (dB)") +
      theme_bw() +
      theme(axis.text.x=element_text(size=16), 
            axis.text.y=element_text(size=16), 
            axis.title.x=element_text(size=18),
            axis.title.y=element_text(size=18),
            legend.position = "none")
    
    png(filename = paste0("Figures/FIGX_SPL-prob_", sexes[s], ".png"), width = 9, height = 7, units = "in", res = 600)
    print(SPL_plot)
    dev.off()
    
    
    ## Effect of direction
    
    # Get predictions
    dir_plot <- sjPlot::plot_model(RSF_models[[s]], type = "pred", terms = "relDir")
    dir_plot <- data.frame(dir_plot$data)
    
    # Set selected cones (1) to max y axis
    sexDat$dummy_cone <- 0
    sexDat$dummy_cone[sexDat$case == TRUE] <- max(dir_plot$conf.high)
    sexDat$dummy_cone[sexDat$case == FALSE] <- min(dir_plot$conf.low)
    
    # Get scaling attributes for SPL
    att_DIR <- attributes(scale(sexDat$relDir.OG, center = TRUE, scale = TRUE))
    mylabels_DIR <- seq(0,180,90)
    mybreaks_DIR <- scale(mylabels_DIR, att_DIR$`scaled:center`, att_DIR$`scaled:scale`)[,1]
    
    dirPlot <- ggplot() + 
      geom_count(data = sexDat, aes(x = relDir, y = dummy_cone), alpha = 0.2) +
      geom_ribbon(data = dir_plot, aes(x = x, ymin = conf.low, ymax = conf.high),
                  alpha = 0.5, fill = "grey") +
      geom_line(data = dir_plot, aes(x = x, y = predicted), size = 1) +
      scale_x_continuous(labels = mylabels_DIR, breaks = mybreaks_DIR) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(y = "Log Odds Ratio", x = "Relative Wind Direction (°)") +
      theme_bw() +
      theme(axis.text.x=element_text(size=16), 
            axis.text.y=element_text(size=16), 
            axis.title.x=element_text(size=18),
            axis.title.y=element_text(size=18),
            legend.position = "none")
    
    png(filename = paste0("Figures/FIGX_dir-prob_", sexes[s], ".png"), width = 9, height = 7, units = "in", res = 600)
    print(dirPlot)
    dev.off()

}
