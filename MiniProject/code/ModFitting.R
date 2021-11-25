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
data_subset <- data %>% group_by(ID_no_Rep) # subset data 


########### 1 -- OLS Model #############
Tidy_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% 
    tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_OLS <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  lm_data <- lm(LogPopBio ~ Time, data = data) %>% 
    glance()
}


Tidy_OLS_output <- data_subset %>% group_modify(~ Tidy_OLS (data = .)) 
Glance_OLS_output <- data_subset %>% group_modify(~ Glance_OLS (data = .)) 



########### 2 -- Quadratic equation #############
Tidy_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>%  # Fit lm with quadratic equation
    tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_Qua <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Qua_data <- lm(LogPopBio ~ Time + I(Time^2) , data = data) %>% 
    glance() # AIC and BIC outputs
}


Tidy_Qua_output <- data_subset %>% group_modify(~ Tidy_Qua (data = .)) 
Glance_Qua_output <- data_subset %>% group_modify(~ Glance_Qua (data = .)) 




########### 3 -- Cubic equation #############
Tidy_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>%  # Fit lm with quadratic equation
    tidy() %>% # coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}


Glance_Cub <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) 
  r_val <- coef(summary(lm_data))["Time","Estimate"] 
  
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data) %>% 
    glance() # AIC and BIC outputs
}


Tidy_Cub_output <- data_subset %>% group_modify(~ Tidy_Cub (data = .)) 
Glance_Cub_output <- data_subset %>% group_modify(~ Glance_Cub (data = .))




########### 4 -- Logistic model #############
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

N0_min <- Subset_ID95$LogPopBio[which.min(Subset_ID95$Time_series)] # lowest population size
K_max <- max(Subset_ID95$LogPopBio) # highest population size
r_est <- r_val # estimate from OLS fitting

Logistic_ID95 <- nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = Subset_ID95, list(r = r_est, K = K_max, N0 = N0_min), trace = T)
summary(Logistic_ID95) # nls logistics output summary

AIC(Logistic_ID95)
BIC(Logistic_ID95)


############# Logistic model - Loop through multiple datasets ##### 
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}


Tidy_logistic <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)], # find the log population size when the time point is the smallest
             r = r_val), control = list(maxiter = 500)) %>% 
    tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}

Glance_logistic <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"]
  
  nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)],
             r = r_val), control = list(maxiter = 500)) %>% 
    glance() # constructs tibble that stores AIC and BIC outputs
}


Tidy_logistic_output <- data_subset %>% group_modify(~ Tidy_logistic (data = .)) 

Glance_logistic_output <- data_subset %>% group_modify(~ Glance_logistic (data = .))








########### 5 -- Gompertz model #############
gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
} 


first_half_time_series <- filter(data_subset, Time_series < 1/2 * max(Time_series))
t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]

Tidy_gompertz <- function(data, ...) {
  
  out <- tryCatch(
    expr = {
      lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
      r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
      
      first_half_time_series <- filter(data_subset, Time_series < 1/2 * max(Time_series))
      t_lag_first_half<- first_half_time_series$Time[which.max(diff(diff(first_half_time_series$LogPopBio)))]
      
      nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = data,
            list(K = max(data_subset$LogPopBio),
                 N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)], # find the log population size when the time point is the smallest
                 r = r_val,
                 t_lag = t_lag_first_half), control = list(maxiter = 500)) %>% 
        tidy() %>% # constructs tibble that stores coefficient and p-value outputs
        pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
    },
    error = function(e){ 
      print("error")
      print(data$ID_no_Rep[1])
      print(e)
    },
    warning = function(w){
      print("warning")
      print(data$ID_no_Rep[1])
      print(w)
    },
    finally = function(f){
      print("TryCatch done")
      print(data$ID_no_Rep[1])
      print(f)
    }
  )
}



Glance_gompertz <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"]
  
  nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)],
             r = r_val,
             t_lag = t_lag_first_half), control = list(maxiter = 500), trace = T) %>% 
    glance() # constructs tibble that stores AIC and BIC outputs
}


Tidy_gompertz_output <- data_subset %>% group_modify(~ Tidy_gompertz (data = .)) 

Glance_gompertz_output <- data_subset %>% group_modify(~ Glance_gompertz (data = .))






  

N0_gstart <- min(Subset_ID95$LogPopBio) # lowest population size
K_gstart <- max(Subset_ID95$LogPopBio) # highest population size
r_gstart <- r_val # estimate from OLS fitting
t_lag_gstart <- Subset_ID95$Time[which.max(diff(diff(Subset_ID95$LogPopBio)))] # find last timepoint of lag phase


Gompertz_ID95 <- nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = Subset_ID95,
                      list(t_lag = t_lag_gstart, r = r_gstart, N0 = N0_gstart, K = K_gstart), 
                      control = list(maxiter = 500)) # increase max no of iterations
summary(Gompertz_ID95)





########### 6 -- Baranyi model #############
baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                         exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                               exp(-r * t_lag) - exp(-r * 
                                                                           (t + t_lag)))))-1)/(exp(K-N0))))}


Tidy_baranyi <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"] # save r2 estimate as r_val
  
  nlsLM(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = data,
        list(K = max(data_subset$PopBio),
             N0 = data_subset$PopBio[which.min(data_subset$Time_series)], # find the log population size when the time point is the smallest
             r = r_val,
             t_lag = data_subset$Time[which.max(diff(diff(data_subset$LogPopBio)))]), control = list(maxiter = 500)) %>% 
    tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}

Glance_baranyi <- function(data, ...) {
  
  lm_data <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
  r_val <- coef(summary(lm_data))["Time","Estimate"]
  
  nlsLM(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)],
             r = r_val,
             t_lag = data_subset$Time[which.max(diff(diff(data_subset$LogPopBio)))]), control = list(maxiter = 500)) %>% 
    glance() # constructs tibble that stores AIC and BIC outputs
}


Tidy_baranyi_output <- data_subset %>% group_modify(~ Tidy_baranyi (data = .)) 

Glance_baranyi_output <- data_subset %>% group_modify(~ Glance_baranyi (data = .))



N0_bstart <- min(Subset_ID95$LogPopBio) # lowest population size
K_bstart <- max(Subset_ID95$PopBio) # highest population size
r_bstart <- r_val # needs maximum specific growth rate
t_lag_bstart <- Subset_ID95$Time[which.max(diff(diff(Subset_ID95$LogPopBio)))] # find last timepoint of lag phase


Baranyi_ID95 <- nlsLM(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = Subset_ID95,
                      list(t_lag = t_lag_bstart, r = r_bstart, N0 = N0_bstart, K = K_bstart), 
                      control = list(maxiter = 500), trace = T) # increase max no of iterations
summary(Baranyi_ID95)

AIC(Baranyi_ID95)
BIC(Baranyi_ID95)


############### Plotting the graph ###################
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
} 


baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                                          exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                         exp(-r * t_lag) - exp(-r * 
                                                                 (t + t_lag)))))-1)/(exp(K-N0))))}


Plotting <- function(data, ...) {
  OlS_datasubset <- lm(LogPopBio ~ Time, data = data)
  Qua_datasubset <- lm(LogPopBio ~ Time + I(Time^2) , data = data)
  Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data)
  
  timepoints <- seq(min(data_subset$Time), max(data_subset$Time), 0.5) 
  
  logistic_points <- logistic(t = timepoints,
                              r = coef(data_subset)["r"],
                              K = coef(data_subset)["K"],
                              N = coef(data_subset)["N0"])
  
  gompertz_points <- gompertz(t = timepoints,
                              r = coef(data_subset)["r"], 
                              K = coef(data_subset)["K"], 
                              N = coef(data_subset)["N0"], 
                              t_lag = coef(data_subset)["t_lag"])
  
  baranyi_points <- baranyi(t = timepoints, 
                            r = coef(data_subset)["r"], 
                            K = coef(data_subset)["K"], 
                            N = coef(data_subset)["N0"], 
                            t_lag = coef(data_subset)["t_lag"])

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


combineplot <- ggplot(data_subset, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", col = "red", se = F) + # line for OLS
  geom_smooth (method= "lm", col = "orange", formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")

print(combineplot)

}


Plotting_output <- data_subset %>% group_modify(~ Plotting (data = .))
