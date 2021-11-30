rm(list=ls())
require(ggplot2)
require(minpack.lm)
require(tidyverse)
require(broom)

data = read.csv("../data/ModifiedLogisticGrowthData.csv")
data_subset <- data %>% group_by(ID_no_Rep) %>% filter(Medium == "TSB" & Temp == "5") # subset data 


########### 1 -- OLS Model #############
Tidy_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% glance()
}


Tidy_OLS_output <- data_subset %>% group_modify(~ Tidy_OLS (data = .)) 

Glance_OLS_output <- data_subset %>% group_modify(~ Glance_OLS (data = .)) 



########### 2 -- Quadratic equation #############
Tidy_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% glance() # AIC and BIC outputs
}


Tidy_Qua_output <- data_subset %>% group_modify(~ Tidy_Qua (data = .)) 
Glance_Qua_output <- data_subset %>% group_modify(~ Glance_Qua (data = .)) 




########### 3 -- Cubic equation #############
Tidy_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>%  tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>% glance() # AIC and BIC outputs
}


Tidy_Cub_output <- data_subset %>% group_modify(~ Tidy_Cub (data = .)) 
Glance_Cub_output <- data_subset %>% group_modify(~ Glance_Cub (data = .))



############# 4 -- Logistic model - Loop through multiple datasets ##### 
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}


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



Tidy_logistic_results <- data_subset %>% group_modify(~ Tidy_logistic (data = .)) 

Glance_logistic_results <- data_subset %>% group_modify(~ Glance_logistic (data = .))








########### 5 -- Gompertz model #############
gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
} 

### KEEP
Tidy_gompertz <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series)) # only selects data points in the first half of the time series
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){          # if error, generate a row of NAs for that subset
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
             p.value_t_lag = c(NA_real_),)
    }
  )
}

### KEEP
Glance_gompertz <- function(data, ...) {
  
  tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data)
      r_val <- coef(summary(lm_data))["Time", "Estimate"]
      
      first_half_time_series <- filter(data, Time_series < 1/2 * max(Time_series))
      t_lag_first_half <- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = data,
            list(K = max(data$LogPopBio),
                 N0 = data$LogPopBio[which.min(data$Time_series)],
                 r = r_val,
                 t_lag = t_lag_first_half), 
            control = list(maxiter = 500)) %>%
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





Tidy_gompertz_results <- data_subset %>% group_modify(~ Tidy_gompertz(data = .))

Glance_gompertz_results <- data_subset %>% group_modify(~ Glance_gompertz (data = .)) 






########### 6 -- Baranyi model #############
baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                                          exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                         exp(-r * t_lag) - exp(-r * 
                                                                 (t + t_lag)))))-1)/(exp(K-N0))))}


### GOOD
Tidy_baranyi <- function(data, ...) {
  
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




# KEEP
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



Tidy_baranyi_output <- data_subset %>% group_modify(~ Tidy_baranyi (data = .)) 
Glance1_baranyi_output <- data_subset %>% group_modify(~ Glance_baranyi (data = .))



############### Plotting the graph ###################
# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 23, 2021
# File name: ModFitting.R
# Description: This script imports modified data for model fitting. 


rm(list=ls())
require(ggplot2)
require(minpack.lm)
require(tidyverse)
require(broom)

data = read.csv("../data/ModifiedLogisticGrowthData.csv")

Subset_ID95 <- subset(data, ID_no_Rep == "95_1", select=X:ID_no_Rep) # Subset data with ID 95
data_subset <- data %>% group_by(ID_no_Rep) %>% filter(Medium == "TSB" & Temp == "5") # Subset data for testing


### Plotting linear models only
Plotting <- function(data, ...) {
  
  combineplot <- ggplot(data_subset, aes(x = Time, y = LogPopBio)) +
    geom_point(size = 2.5) +
    geom_smooth(method = "lm", col = "red", se = F) + # line for OLS
    geom_smooth (method= "lm", col = "orange", formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
    geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
    theme_bw() + # make the background white
    theme(aspect.ratio=1)+ # make the plot square 
    labs(x = "Time", y = "log(Population Size)") +
    facet_wrap(data_subset$ID_no_Rep)
  
  print(combineplot) 
  
}


Plotting (data_subset = .) # plot linear models only






### Plot linear and nLS models
OlS_datasubset <- lm(LogPopBio ~ Time, data = data)
Qua_datasubset <- lm(LogPopBio ~ Time + I(Time^2) , data = data)
Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data)

timepoints <- seq(min(data$Time), max(data$Time), 0.5) 

logistic_points <- logistic(t = timepoints,
                            r = coef(data)["r"],
                            K = coef(data)["K"],
                            N = coef(data)["N0"])

gompertz_points <- gompertz(t = timepoints,
                            r = coef(data)["r"], 
                            K = coef(data)["K"], 
                            N = coef(data)["N0"], 
                            t_lag = coef(data)["t_lag"])

baranyi_points <- baranyi(t = timepoints, 
                          r = coef(data)["r"], 
                          K = coef(data)["K"], 
                          N = coef(data)["N0"], 
                          t_lag = coef(data)["t_lag"])

df1 <- data.frame(timepoints, logistic_points)
df1$model <- "Logistic model"
names(df1) <- c("Time", "LogPopBio", "model")

df2 <- data.frame(timepoints, gompertz_points)
df2$model <- "Gompertz model"
names(df2) <- c("Time", "LogPopBio", "model")

df3 <- data.frame(timepoints, baranyi_points)
df3$model <- "Baranyi model"
names(df3) <- c("Time", "LogPopBio", "model")

model_frame <- rbind(df1, df2, df3)


combineplot <- ggplot(data, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", col = "red", se = F) + # line for OLS
  geom_smooth (method= "lm", col = "orange", formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)") +
  facet_wrap(data$ID_no_Rep)

print(combineplot)




Plotting_all (data_subset = .)




