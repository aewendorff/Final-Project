# Packages ----------------------------------------------------------------
library(lmtest)
library(AICcmodavg)
library(ggfortify)
library(emmeans)
library(car)



# Calculate Summary Statistics --------------------------------------------
summary_stats <- final_no_middle %>%
  group_by(site_type, ice_cover) %>%
  summarise(mean = mean(delta_spc_daily, na.rm = TRUE),
            sd = sd(delta_spc_daily, na.rm = TRUE),
            min = min(delta_spc_daily, na.rm = TRUE),
            max = max(delta_spc_daily, na.rm = TRUE)) %>%
  ungroup()



# Fit Candidate Models --------------------------------------------------------------
full_model <- lm(delta_spc_daily ~ ice_cover * site_type, data = final_no_middle)
reduced_model <- lm(delta_spc_daily ~ ice_cover + site_type, data = final_no_middle)
single_model_1 <- lm(delta_spc_daily ~ ice_cover, data = final_no_middle)
single_model_2 <- lm(delta_spc_daily ~ site_type, data = final_no_middle)
intercept <- lm(delta_spc_daily ~ 1, data = final_no_middle)



# Model Selection ------------------------------------------
models <- list(full_model, reduced_model, single_model_1, single_model_2, intercept)
mod.names <- c('Full Model', 'Main Effect', 'Delta SPC and Ice Cover', 
               'Delta SPC and Site Type', 'Intercept Only')

#Create table with AIC results
aic_table <- aictab(cand.set = models, modnames = mod.names)
print(aic_table)

#Summary of full model
fm_summary <- summary(full_model)
print(fm_summary)

# Pairwise Testing with emmeans -------------------------------------------
emm <- emmeans(full_model, ~ ice_cover * site_type) #calculate emmeans from full model

#Effect of ice cover within each site type
contrast(emm, method = "revpairwise", by = "site_type", adjust = "none")

#Effect of site type within each ice cover level
contrast(emm, method = "revpairwise", by = "ice_cover", adjust = "none")

#Convert emmeans to dataframe to use for plotting
emm_df <- as.data.frame(emm)



# Assumption Checking -----------------------------------------------------

#Visual diagnostics of assumptions using autoplot
p1 <- autoplot(full_model, which = 1, label.size = 0) #Residuals vs Fitted
p2 <- autoplot(full_model, which = 2, label.size = 0) #Normal Q-Q
p3 <- autoplot(full_model, which = 3, label.size = 0) #Scale-Location (not really useful)
p5 <- autoplot(full_model, which = 5, label.size = 0) #Residuals vs Leverage

#Histogram of residuals
hist(full_model$residuals, main = "Histogram of Residuals", xlab = "Residuals")

#Levene's Test for homogeneity of variances
leveneTest(delta_spc_daily ~ interaction(ice_cover, site_type), data = final_no_middle)

#Save diagnostic plots
ggsave("outputs/residuals_vs_fitted.png", p1, width = 5, height = 5)
ggsave("outputs/qq_plot.png", p2, width = 5, height = 5)
ggsave("outputs/scale_location.png", p3, width = 5, height = 5)
ggsave("outputs/residuals_vs_leverage.png", p5, width = 5, height = 5)


# Re-run ANOVA with violation of homoscedasticity --------------------------

#Type III ANOVA with heteroscedasticity-consistent (White) standard errors
type_3 <- Anova(full_model, type = 3, white.adjust = TRUE)