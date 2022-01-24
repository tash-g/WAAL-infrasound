## Conditional logistic regression may be an option to solve the problem that the current glmer
## doesn't actually link the 12 cones to one another. See definition:
"Conditional logistic regression (CLR) is a specialized type of logistic regression usually employed when 
case subjects with a particular condition or attribute are each matched with n control subjects without the 
condition. In general, there may be 1 to m cases matched with 1 to n controls."

## Let's give it a try!

## Next steps:
## - Tidy code
## - Drop non significant predictors
## - Compare sexes

# bird ID - cone ID - case (selected cone) - covariates

library(survival); library(ggplot2)

# Load the data -----------------------------------------------------------

modDat <- data.table::fread("Data_inputs/GPS_2013_model-data-ORIGINAL.csv", data.table = F)

# Rename and process variables
names(modDat)[2] <- "case"
modDat$case <- ifelse(modDat$case == 1, TRUE, FALSE)

factor_vars <- c("TripID", "birdID", "Sex", "Trip_state", "pointID")
modDat[factor_vars] <- lapply(modDat[factor_vars], factor)


# Run conditional logistic regression -------------------------------------

## Scale continuous variables and remove NA variables
modDat[,c(3,7,8)] <- lapply(modDat[,c(3,7,8)], function(x) c(scale(x, center = TRUE, scale = TRUE))) # abs_SPL_2000dB WindSp relDir 
modDat.naomit <- na.omit(modDat)

SSF <- clogit(case ~ strata(pointID) + abs_SPL_2000dB*WindSp*relDir + abs_SPL_2000dB*relDir + 
                relDir*WindSp + cluster(birdID), method = 'approximate', data = modDat)

SSF <- clogit(case ~ strata(pointID) + abs_SPL_2000dB + WindSp * relDir + cluster(birdID), method = 'approximate', data = modDat)


summary(SSF)

sjPlot::plot_model(SSF, title="SSF Coefficients")
test <- sjPlot::plot_model(SSF, title="SSF Coefficients")
test <- as.data.frame(test$data)

library(ggplot2)

test <- subset(test, term != "WindSp")

ggplot() +
  geom_point(data = test, aes(x = estimate, y = term)) + 
  geom_errorbar(data = test, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw()



# Cone sensitivity analysis -----------------------------------------------

apertures <- c(15 , 20, 30, 45, 60) # 15, 20, 30, 45, 60
model_list <- vector(mode = "list", length = length(apertures))
p_values <- vector(mode = "list", length = length(apertures))

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
  
  model_list[[i]] <- clogit(case ~ strata(pointID) + abs_SPL_2000dB*WindSp*relDir + abs_SPL_2000dB*relDir + 
                  relDir*WindSp + cluster(birdID), method = 'approximate', data = modDat)
 
  p_values[[i]] <- data.frame(coef(summary(model_list[[i]]))[,6])
  p_values[[i]]$aperture <- apertures[i]

}

model_ps <- do.call("rbind", p_values)
model_ps <- cbind(rownames(model_ps), data.frame(model_ps, row.names=NULL))
colnames(model_ps) <- c("parameter", "p_value", "aperture")
model_ps$parameter <- gsub('[[:digit:]]+', '', model_ps$parameter)

## Make plot of change in significance

# Extract parameters of interest
pars <- c("abs_SPL_2000dB", "relDir", "WindSp:relDir", "abs_SPL_2000dB:relDir")
plot_ps <- model_ps[model_ps$parameter %in% pars,]

ggplot(data = model_ps, aes(x = aperture, y = p_value, col = parameter, group = parameter)) + 
  geom_point() + geom_line() + theme




sjPlot::plot_model(model_list[[1]], title="SSF Coefficients")
test <- sjPlot::plot_model(SSF, title="SSF Coefficients")
test <- as.data.frame(test$data)

library(ggplot2)

i <- 5 # 15, 20, 30, 45, 60
test <- sjPlot::plot_model(model_list[[i]], title=paste0("SSF Coefficients - ", apertures[i], " degrees"))
test <- as.data.frame(test$data)
test <- subset(test, term != "WindSp")

SSF_plot60 <- ggplot() +
  geom_point(data = test, aes(x = estimate, y = term)) + 
  geom_errorbar(data = test, aes(xmin = conf.low, xmax = conf.high, y = term), width = 0.2) +
  geom_vline(xintercept = 1, colour = "blue", size = 1.5) +
  theme_bw() + labs(title = paste0("SSF Coefficients - ", apertures[i], " degrees"))

gridExtra::grid.arrange(SSF_plot15, SSF_plot20, SSF_plot30, SSF_plot45, SSF_plot60,
                        ncol = 2, nrow = 3)
