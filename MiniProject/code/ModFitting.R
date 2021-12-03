# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 23, 2021
# File name: ModFitting.R
# Description: This script imports modified data for model fitting. 


rm(list=ls())
require(minpack.lm) # for nLS 
require(tidyverse)
require(broom)
require(nls.multstart) # for mult_start

data = read.csv("../data/ModifiedLogisticGrowthData.csv")
data_subset <- data %>% group_by(ID_no_Rep_dup) # subset data 

########################################
########### 1 -- OLS Model #############
########################################
# Fit OLS and saves coefficient and p-value outputs in a tibble
Tidy_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}

# Fit OLS and saves AIC/BIC outputs in a tibble
Glance_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% glance()
}

# Fit OLS and saves fitted values/residuals in a tibble
Augment_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% augment()
}

# Run the loops and saves output in a tibble
Tidy_OLS_results <- data_subset %>% group_modify(~ Tidy_OLS (data = .)) 
Glance_OLS_results <- data_subset %>% group_modify(~ Glance_OLS (data = .)) 
Augment_OLS_results <- data_subset %>% group_modify(~ Augment_OLS (data = .)) 

# Save results as a separate csv 
write.csv(Glance_OLS_results, "../data/Glance_OLS_results.csv") # for AIC analysis in a separate script
write.csv(Augment_OLS_results, "../data/Augment_OLS_results.csv") # for assumption testing in a separate script


#################################################
########### 2 -- Quadratic equation #############
#################################################
# Fit Quadratic equation and saves coefficient and p-value outputs in a tibble
Tidy_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


# Fit Quadratic equation and saves AIC/BIC outputs in a tibble
Glance_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% glance() # AIC and BIC outputs
}

# Fit Quadratic equation and saves fitted values/residuals in a tibble
Augment_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% augment() # AIC and BIC outputs
}

# Run the loops and saves output in a tibble
Tidy_Qua_results <- data_subset %>% group_modify(~ Tidy_Qua (data = .)) 
Glance_Qua_results <- data_subset %>% group_modify(~ Glance_Qua (data = .)) 
Augment_Qua_results <- data_subset %>% group_modify(~ Augment_Qua (data = .)) 

# Save results as a separate csv 
write.csv(Glance_Qua_results, "../data/Glance_Qua_results.csv") # for AIC analysis in a separate script
write.csv(Augment_Qua_results, "../data/Augment_Qua_results.csv")



#############################################
########### 3 -- Cubic equation #############
#############################################

# Fit Cubic equation and saves coefficient and p-value outputs in a tibble
Tidy_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>%  tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}

# Fit Cubic equation and saves AIC/BIC outputs in a tibble
Glance_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>% glance() # AIC and BIC outputs
}

# Fit Cubic equation and saves fitted values/residuals in a tibble
Augment_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>% augment() # AIC and BIC outputs
}

# Run the loops and saves output in a tibble
Tidy_Cub_results <- data_subset %>% group_modify(~ Tidy_Cub (data = .)) 
Glance_Cub_results <- data_subset %>% group_modify(~ Glance_Cub (data = .))
Augment_Cub_results <- data_subset %>% group_modify(~ Augment_Cub (data = .)) 

# Save results as a separate csv 
write.csv(Glance_Cub_results, "../data/Glance_Cub_results.csv") # for AIC analysis in a separate script
write.csv(Augment_Cub_results, "../data/Augment_Cub_results.csv")

####################################################
############# 4 -- Logistic model - ################ 
####################################################
# logistic model in log scale
logistic <- function(t, r, K, N0){                      
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

# Fit logistic model and saves coefficient and p-value outputs in a tibble
Tidy_logistic <- function(data, ...) {
  
  tryCatch (
    exp={
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
    r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
    
    nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
          list(K = max(data$LogPopBio),
               N0 = data$LogPopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
               r = r_val), 
          control = list(maxiter = 500)) %>% 
      tidy() %>% # constructs tibble that stores coefficient and p-value outputs
      pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){
      tibble(estimate_K = c(NA_real_),
             estimate_N0 = c(NA_real_),
             estimate_r = c(NA_real_),
             std.error_K = c(NA_real_),
             std.error_N0 = c(NA_real_),
             std.error_r = c(NA_real_),
             statistic_K = c(NA_real_),
             statistic_N0 = c(NA_real_),
             statistic_r = c(NA_real_),
             p.value_K = c(NA_real_),
             p.value_N0 = c(NA_real_),
             p.value_r = c(NA_real_),
        
        )
      
    }
  )
}
  
  
# Fit logistic model and saves AIC/BIC outputs in a tibble
Glance_logistic <- function(data, ...){
  
  tryCatch (
    exp = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"]
      
      nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)],
                 r = r_val), 
            control = list(maxiter = 500)) %>% 
        glance() # constructs tibble that stores AIC and BIC outputs
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}

# Fit logistic model and saves fitted values/residuals in a tibble
Augment_logistic <- function(data, ...){
  
  tryCatch (
    exp = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"]
      
      nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)],
                 r = r_val), 
            control = list(maxiter = 500)) %>% 
        augment() # constructs tibble that stores AIC and BIC outputs
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}

# Run the loops and saves output in a tibble
Tidy_logistic_results <- data_subset %>% group_modify(~ Tidy_logistic (data = .)) 
Glance_logistic_results <- data_subset %>% group_modify(~ Glance_logistic (data = .))
Augment_logistic_results <- data_subset %>% group_modify(~ Augment_logistic (data = .)) 

# Save results as a separate csv 
write.csv(Glance_logistic_results, "../data/Glance_logistic_results.csv") # for AIC analysis in a separate script
write.csv(Augment_logistic_results, "../data/Augment_logistic_results.csv")

#####################################################
########### 5 -- Gompertz model - nlsLM #############
#####################################################
# Modified gompertz growth model (Zwietering 1990)
gompertz <- function(t, r, K, N0, t_lag){     
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
} 

# Fit gompertz model and saves coefficient and p-value outputs in a tibble
Tidy_gompertz<- function(data, ...) {
  
  tryCatch(
    exp = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects data points in the first half of the time series
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
            data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){
      tibble(estimate_K = c(NA_real_),
             estimate_N0 = c(NA_real_),
             estimate_r = c(NA_real_),
             estimate_t_lag = c(NA_real_),
             std.error_K = c(NA_real_),
             std.error_N0 = c(NA_real_),
             std.error_r = c(NA_real_),
             std.error_t_lag = c(NA_real_),
             statistic_K = c(NA_real_),
             statistic_N0 = c(NA_real_),
             statistic_r = c(NA_real_),
             statistic_t_lag = c(NA_real_),
             p.value_K = c(NA_real_),
             p.value_N0 = c(NA_real_),
             p.value_r = c(NA_real_),
             p.value_t_lag = c(NA_real_)
      )
      
    }
  )
}





# Fit gompertz model and saves AIC/BIC outputs in a tibble
Glance_gompertz <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data)
      r_val <- coef(summary(lm_data))["Time", "Estimate"]
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series))
      t_lag_first_half <- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
            data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>%
        glance()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
  
}

# Fit logistic model and saves fitted values/residuals in a tibble
Augment_gompertz <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data)
      r_val <- coef(summary(lm_data))["Time", "Estimate"]
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series))
      t_lag_first_half <- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
            data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>%
        augment()
    },
    error = function(e){
      tibble(LogPopBio = c(NA_real_),
             Time = c(NA),
             .fitted = c(NA_real_),
             .resid = c(NA_real_))
    }
  )
  
}

# Run the loops and saves output in a tibble
Tidy_gompertz_results <- data_subset %>% group_modify(~ Tidy_gompertz(data = .))
Glance_gompertz_results <- data_subset %>% group_modify(~ Glance_gompertz (data = .)) 
Augment_gompertz_results <- data_subset %>% group_modify(~ Augment_gompertz (data = .)) 

# Save results as a separate csv 
write.csv(Glance_gompertz_results, "../data/Glance_gompertz_results.csv") # for AIC analysis in a separate script
write.csv(Augment_gompertz_results, "../data/Augment_gompertz_results.csv")



##########################################################
########### 5a -- Gompertz model - multstart #############
##########################################################
# Modified gompertz growth model (Zwietering 1990)
gompertz <- function(t, r, K, N0, t_lag){     
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
} 

# Fit gompertz model and saves coefficient and p-value outputs in a tibble
Tidy_gompertz_multstart <- function(data, ...) {
  
  tryCatch(
    exp = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects data points in the first half of the time series
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 500, 
                    start_lower = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){
      tibble(estimate_K = c(NA_real_),
             estimate_N0 = c(NA_real_),
             estimate_r = c(NA_real_),
             estimate_t_lag = c(NA_real_),
             std.error_K = c(NA_real_),
             std.error_N0 = c(NA_real_),
             std.error_r = c(NA_real_),
             std.error_t_lag = c(NA_real_),
             statistic_K = c(NA_real_),
             statistic_N0 = c(NA_real_),
             statistic_r = c(NA_real_),
             statistic_t_lag = c(NA_real_),
             p.value_K = c(NA_real_),
             p.value_N0 = c(NA_real_),
             p.value_r = c(NA_real_),
             p.value_t_lag = c(NA_real_)
      )
      
    }
  )
}





# Fit gompertz model and saves AIC/BIC outputs in a tibble
Glance_gompertz_multstart <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data)
      r_val <- coef(summary(lm_data))["Time", "Estimate"]
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series))
      t_lag_first_half <- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 500, 
                    start_lower = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>%
        glance()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
  
}



# Fit gompertz model and saves fitted values/residuals in a tibble
Augment_gompertz_multstart <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data)
      r_val <- coef(summary(lm_data))["Time", "Estimate"]
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series))
      t_lag_first_half <- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 500, 
                    start_lower = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$LogPopBio), 
                                    N0 = data$LogPopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>%
        augment()
    },
    error = function(e){
      tibble(LogPopBio = c(NA_real_),
             Time = c(NA),
             .fitted = c(NA_real_),
             .resid = c(NA_real_))
    }
  )
  
}

# Run the loops and saves output in a tibble
Tidy_gompertz_multstart_results <- data_subset %>% group_modify(~ Tidy_gompertz_multstart (data = .))
Glance_gompertz_multstart_results <- data_subset %>% group_modify(~ Glance_gompertz_multstart (data = .)) 
Augment_gompertz_multstart_results <- data_subset %>% group_modify(~ Augment_gompertz_multstart (data = .)) 

# Save results as a separate csv 
write.csv(Glance_gompertz_multstart_results, "../data/Glance_gompertz_multstart_results.csv") # for AIC analysis in a separate script
write.csv(Augment_gompertz_multstart_results, "../data/Augment_gompertz_multstart_results.csv")



####################################################
########### 6 -- Baranyi model - nlsLM #############
####################################################
# baranyi model
baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                         exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                               exp(-r * t_lag) - exp(-r * 
                                                                           (t + t_lag)))))-1)/(exp(K-N0))))}


# Fit baranyi model and saves coefficient and p-value outputs in a tibble
Tidy_baranyi <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(PopBio ~ baranyi(t = Time, r, K, N0, t_lag), 
            data = data,
            list(K = max(data$PopBio),
                 N0 = data$PopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){       # if error, generate a row of NAs for that subset
      tibble(estimate_K = c(NA_real_),
             estimate_N0 = c(NA_real_),
             estimate_r = c(NA_real_),
             estimate_t_lag = c(NA_real_),
             std.error_K = c(NA_real_),
             std.error_N0 = c(NA_real_),
             std.error_r = c(NA_real_),
             std.error_t_lag = c(NA_real_),
             statistic_K = c(NA_real_),
             statistic_N0 = c(NA_real_),
             statistic_r = c(NA_real_),
             statistic_t_lag = c(NA_real_),
             p.value_K = c(NA_real_),
             p.value_N0 = c(NA_real_),
             p.value_r = c(NA_real_),
             p.value_t_lag = c(NA_real_))
    }
  )
}




# Fit baranyi model and saves AIC/BIC outputs in a tibble
Glance_baranyi <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(PopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = data,
            list(K = max(data$PopBio),
                 N0 = data$PopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        glance()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}


# Fit baranyi model and saves fitted values/residuals in a tibble
Augment_baranyi <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(PopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = data,
            list(K = max(data$PopBio),
                 N0 = data$PopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        augment()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}

# Run the loops and saves output in a tibble
Tidy_baranyi_results <- data_subset %>% group_modify(~ Tidy_baranyi (data = .)) 
Glance_baranyi_results <- data_subset %>% group_modify(~ Glance_baranyi (data = .))
Augment_baranyi_results <- data_subset %>% group_modify(~ Augment_baranyi (data = .)) 

# Save results as a separate csv 
write.csv(Glance_baranyi_results, "../data/Glance_baranyi_results.csv") # for AIC analysis in a separate script
write.csv(Augment_baranyi_results, "../data/Augment_baranyi_results.csv")


################################################################
################# 6a - Baranyi model - multstart ###############
################################################################
# Fit baranyi model and saves coefficient and p-value outputs in a tibble
Tidy_baranyi_multstart <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 500, 
                    start_lower = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){       # if error, generate a row of NAs for that subset
      tibble(estimate_K = c(NA_real_),
             estimate_N0 = c(NA_real_),
             estimate_r = c(NA_real_),
             estimate_t_lag = c(NA_real_),
             std.error_K = c(NA_real_),
             std.error_N0 = c(NA_real_),
             std.error_r = c(NA_real_),
             std.error_t_lag = c(NA_real_),
             statistic_K = c(NA_real_),
             statistic_N0 = c(NA_real_),
             statistic_r = c(NA_real_),
             statistic_t_lag = c(NA_real_),
             p.value_K = c(NA_real_),
             p.value_N0 = c(NA_real_),
             p.value_r = c(NA_real_),
             p.value_t_lag = c(NA_real_))
    }
  )
}




# Fit baranyi model and saves AIC/BIC outputs in a tibble
Glance_baranyi_multstart <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 1000, 
                    start_lower = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>% 
        glance()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}

# Fit baranyi model and saves AIC/BIC outputs in a tibble
Augment_baranyi_multstart <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects 
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nls_multstart(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), 
                    data = data,
                    iter = 1000, 
                    start_lower = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half - (1/2 * t_lag_first_half)), 
                    start_upper = c(K = max(data$PopBio), 
                                    N0 = data$PopBio[which.min(data$Time_series)], 
                                    r = r_val, 
                                    t_lag = t_lag_first_half + (1/2 * t_lag_first_half)),
                    supp_errors = 'Y'
      ) %>% 
        augment()
    },
    error = function(e){
      tibble(sigma = c(NA_real_),
             isConv = c(NA),
             finTol = c(NA_real_),
             logLik = c(NA_real_),
             AIC = c(NA_real_),
             BIC = c(NA_real_),
             deviance = c(NA_real_),
             df.residual = c(NA_real_),
             nobs = c(NA_real_))
    }
  )
}

# Run the loops and saves output in a tibble
Tidy_baranyi_multstart_results <- data_subset %>% group_modify(~ Tidy_baranyi_multstart (data = .)) 
Glance_baranyi_multstart_results <- data_subset %>% group_modify(~ Glance_baranyi_multstart (data = .))
Augment_baranyi_multstart_results <- data_subset %>% group_modify(~ Augment_baranyi_multstart (data = .)) 

# Save results as a separate csv 
write.csv(Glance_baranyi_multstart_results, "../data/Glance_baranyi_multstart_results.csv") # for AIC analysis in a separate script
write.csv(Augment_baranyi_multstart_results, "../data/Augment_baranyi_multstart_results.csv")

##########################################################################
############## Time needed to run each model function ####################
##########################################################################
system.time({ Tidy_OLS(data) })
system.time({ Tidy_Qua(data) })
system.time({ Tidy_Cub(data) })
system.time({ Tidy_logistic(data) })
system.time({ Tidy_gompertz(data) })
system.time({ Tidy_gompertz_multstart(data) })
system.time({ Tidy_baranyi(data) })
system.time({ Tidy_baranyi_multstart(data) })



#########################################################################
############### Extract parameters for graph plotting ###################
#########################################################################
Logistic_parameters <- select(Tidy_logistic_results, c("ID_no_Rep_dup", "estimate_K", "estimate_N0", "estimate_r"))
Gompertz_parameters <- select(Tidy_gompertz_results, c("ID_no_Rep_dup", "estimate_K", "estimate_N0", "estimate_r", "estimate_t_lag"))
Baranyi_parameters <- select(Tidy_baranyi_results, c("ID_no_Rep_dup", "estimate_K", "estimate_N0", "estimate_r", "estimate_t_lag"))
Gompertz_multstart_parameters <- select(Tidy_gompertz_multstart_results, c("ID_no_Rep_dup", "estimate_K", "estimate_N0", "estimate_r", "estimate_t_lag"))
Baranyi_multstart_parameters <- select(Tidy_baranyi_multstart_results, c("ID_no_Rep_dup", "estimate_K", "estimate_N0", "estimate_r", "estimate_t_lag"))

# rename column names to distinguish between models
colnames(Logistic_parameters)[2:4] <- paste("Log_", colnames(Logistic_parameters[,c(2:4)]), sep = "")
colnames(Gompertz_parameters)[2:5] <- paste("Gom_", colnames(Gompertz_parameters[,c(2:5)]), sep = "")
colnames(Baranyi_parameters)[2:5] <- paste("Bar_", colnames(Baranyi_parameters[,c(2:5)]), sep = "")
colnames(Gompertz_multstart_parameters)[2:5] <- paste("Gom_mult_", colnames(Gompertz_parameters[,c(2:5)]), sep = "")
colnames(Baranyi_multstart_parameters)[2:5] <- paste("Bar_mult_", colnames(Baranyi_parameters[,c(2:5)]), sep = "")

# merge all ABIC dataframes and save as a new dataframe
Parameters_nLS_all <- Reduce(function(x, y) merge(x, y, by = "ID_no_Rep_dup", all = TRUE), 
                       list(Logistic_parameters, Gompertz_parameters, Baranyi_parameters, Gompertz_multstart_parameters, Baranyi_multstart_parameters))


write.csv(Parameters_nLS_all, "../data/Parameters_nLS.csv")







