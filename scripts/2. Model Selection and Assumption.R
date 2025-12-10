

#Packages: lmtest, AICcmodavg


# Load Packages: ----------------------------------------------------------
library(lmtest)
library(AICcmodavg)
library(ggfortify)


# Fit Models --------------------------------------------------------------
full_model <- lm(delta_spc_daily ~ ice_cover * site_type, data = final_no_middle)
reduced_model <- lm(delta_spc_daily ~ ice_cover + site_type, data = final_no_middle)
single_model_1 <- lm(delta_spc_daily ~ ice_cover, data = final_no_middle)
single_model_2 <- lm(delta_spc_daily ~ site_type, data = final_no_middle)
intercept <- lm(delta_spc_daily ~ 1 , data = final_no_middle)

###PART C###---
logLik(full_model) 
logLik(reduced_model) 

###PART D###---
lrtest(full_model, reduced_model) 
  #p-value < 2.2e-16




# Objective 2 -------------------------------------------------------------
#Create list of models
models <- list(full_model, reduced_model, single_model_1, single_model_2, intercept)

#Create model name object
mod.names <- c('Full Model', 'Main Effect', 'Delta SPC and Ice Cover', 'Delta SPC and Site Type', 'Intercept Only')

#Create table with  AIC results
aic_table <- aictab(cand.set = models, modnames = mod.names)
#aictab() creates a table automatically with AIC model comparisons
#automatically creates Delta AIC column



# Assumption checking -----------------------------------------------------

#Run Autoplots for assumption checking
p1 <- autoplot(full_model, which = 1, label.size = 0)  # 1 = Residuals vs Fitted
      #gets rid of the number labels 

# Normal Q-Q
p2 <- autoplot(full_model, which = 2, label.size = 0)  # 2 = Normal Q-Q

# Scale-Location
p3 <- autoplot(full_model, which = 3, label.size = 0)  # 3 = Scale-Location

# Residuals vs Leverage
p5 <- autoplot(full_model, which = 5, label.size = 0)  # 5 = Residuals vs Leverage

#Histogram of residuals
hist_residuals <- hist(full_model$residuals)


ggsave("outputs/residuals_vs_fitted.png", p1, width = 5, height = 5)
ggsave("outputs/qq_plot.png", p2, width = 5, height = 5)
ggsave("outputs/scale_location.png", p3, width = 5, height = 5)
ggsave("outputs/residuals_vs_leverage.png", p5, width = 5, height = 5)