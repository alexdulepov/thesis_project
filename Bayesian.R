library(brms)

set.seed(2025)

######ALL PRIORS ARE FLAT
fit_brm <- brm_multiple(
  formula = c66_46_avrsybp ~y1_app_temp_avg_46 +
    c66_46_q1_ht + c66_46_c_bmi + 
    c66_46_ht_med + c66_46_edu +  
    c66_46_smok + c66_46_alc +
    c66_46_phys + c66_46_occup + 
    c66_46_ap_age + sex + 
    c66_46_diab + c66_46_hpcl + 
    c66_46_season + c66_46_urban_rural+day_mean,
  data   = pmm_proj_multiimp,  # mids or list of data frames
  family = gaussian(),
  chains = 4,
  iter   = 2000,
  seed   = 2025,
  cores = 4
)

summary(fit_brm)

prior_summary(fit_brm)

#bayes_R2(fit_brm)

#plot(fit_brm, regex = TRUE)

####ALL PRIORS ARE WEAKLY INFORMATIVE



#####TEMPERATURE HAS STRONG PRIOS OTHERS WEAKLY INFORMATIVE

# 1) Define the prior for the temperature coefficient
#    We assume we want a Normal(-0.26, 0.04^2) on the slope of y1_app_temp_avg_46
my_priors_2 <- c(
  set_prior("normal(-0.26, 0.036)", class = "b", coef = "y1_app_temp_avg_46")
)

# 2) Fit the model on multiply imputed data
fit_brm_3 <- brm_multiple(
  formula = c66_46_avrsybp ~ y1_app_temp_avg_46 +
    c66_46_q1_ht + c66_46_c_bmi + 
    c66_46_ht_med + c66_46_edu +  
    c66_46_smok + c66_46_alc +
    c66_46_phys + c66_46_occup + 
    c66_46_ap_age + sex + 
    c66_46_diab + c66_46_hpcl + 
    c66_46_season + c66_46_urban_rural+day_mean,
  data   = pmm_proj_multiimp,  # mids or list of data frames
  family = gaussian(),
  prior  = my_priors_2,
  chains = 4,
  iter   = 2000,
  seed   = 2025,
  cores = 4
)

summary(fit_brm_3)

prior_summary(fit_brm_3)

my_priors_2 <- c(
  set_prior("normal(-0.26, 0.2)", class = "b", coef = "y1_avg_temp")
)

# 2) Fit the model on multiply imputed data
fit_brm_3 <- brm_multiple(
  formula = c66_46_avrsybp ~ y1_avg_temp +
    c66_46_q1_ht + c66_46_c_bmi + 
    c66_46_ht_med + c66_46_edu +  
    c66_46_smok + c66_46_alc +
    c66_46_phys + c66_46_occup + 
    c66_46_ap_age + sex + 
    c66_46_diab + c66_46_hpcl + 
    c66_46_season + c66_46_urban_rural+day_mean,
  data   = pmm_proj_multiimp,  # mids or list of data frames
  family = gaussian(),
  prior  = my_priors_2,
  chains = 4,
  iter   = 2000,
  seed   = 2025,
  cores = 4
)

summary(fit_brm_3)

get_prior(
  formula = c66_46_avrsybp ~ y1_app_temp_avg_46 + c66_46_q1_ht + c66_46_c_bmi + 
    c66_46_ht_med + c66_46_edu + c66_46_smok + c66_46_alc +
    c66_46_phys + c66_46_occup + c66_46_ap_age + sex + 
    c66_46_diab + c66_46_hpcl + c66_46_season + c66_46_urban_rural + day_mean,
  data = pmm_proj_multiimp,
  family = gaussian()
)

#bayes_R2(fit_brm_3)

#plot(fit_brm_3, regex = TRUE)


library(brms)

# 1) Set priors
my_priors_2 <- c(
  set_prior("normal(-0.26, 0.2)", class = "b", coef = "y1_app_temp_avg_46"),
  set_prior("normal(0, 1)", class = "b", dpar = "sigma")  # optional prior for sigma part
)

# 2) Specify model formula with separate sigma model
form_brm_4 <- bf(
  c66_46_avrsybp ~ y1_app_temp_avg_46 +
    c66_46_q1_ht + c66_46_c_bmi + 
    c66_46_ht_med + c66_46_edu +  
    c66_46_smok + c66_46_alc +
    c66_46_phys + c66_46_occup + 
    c66_46_ap_age + sex + 
    c66_46_diab + c66_46_hpcl + 
    c66_46_season + c66_46_urban_rural + day_mean,
  
  sigma ~ y1_app_temp_avg_46 + sex + c66_46_season  # this is the key part!
)

# 3) Fit the model using multiply imputed data
fit_brm_4 <- brm_multiple(
  formula = form_brm_4,
  data   = pmm_proj_multiimp,  # mids object or list of imputed data.frames
  family = gaussian(),
  prior  = my_priors_2,
  chains = 4,
  iter   = 2000,
  seed   = 2025,
  cores  = 4
)

summary(fit_brm_4)

loo_compare(loo(fit_brm_3), loo(fit_brm_4))

library(sjPlot)

# Save an HTML report of the model
#tab_model(fit_brm_3, file = "brms_output_strongprior.html")


library(rstanarm)

stan_model <- stan_glm(c66_46_avrsybp ~ y1_app_temp_avg_46 +
                         c66_46_q1_ht + c66_46_c_bmi + 
                         c66_46_ht_med + c66_46_edu +  
                         c66_46_smok + c66_46_alc +
                         c66_46_phys + c66_46_occup + 
                         c66_46_ap_age + sex + 
                         c66_46_diab + c66_46_hpcl + 
                         c66_46_season + c66_46_urban_rural+day_mean, data = bp_df_plot,
                       family = gaussian(), prior = normal(-0.26, 0.2), seed = 2025)
                       
summary(stan_model)

prior_summary(stan_model)

ss_res = var(residuals(stan_model))
ss_total = var(fitted(stan_model) + residuals(stan_model))
r2 = 1 - ss_res/ss_total
r2

r2_b = bayes_R2(stan_model)

summary(r2_b)

hist(r2_b) #distribution of R2

pp_check(stan_model, "dens_overlay") #distr of predicted scores (blue) and distr of observed scores

pp_check(stan_model, "stat") #distr of the predicted means by each iteration of the dependent variable
#evidence that the model fitss the data well

pp_check(stan_model, "stat_2d") #scatterplot of the predicted means and sd by each iteration of the dependent variable

compare(loo(stan_model),loo(stan_model_2)) #positive value means the second model is better, but should be less than SE

library(broom.mixed package)

tidy_coef = tidy(stan_model)
model_intercept = tidy_coef[1,2]

x1 = complete(pmm_proj_multiimp)

ggplot(x1, aes(x = y1_app_temp_avg_46, y = c66_46_avrsybp)) + 
  geom_point() + 
  geom_abline(intercept = 26.23, slope = -0.85) 
