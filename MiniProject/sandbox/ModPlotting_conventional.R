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
para = read.csv("../data/Parameters_nLS.csv")

Subset_ID95 <- subset(data, ID_no_Rep == c("1_1" | "10_1" | "100_1"), select=X:ID_no_Rep) # Subset data with ID 95
data_subset <- data %>% group_by(ID_no_Rep) %>% filter(Medium == "TSB" & Temp == "5") # Subset data for testing

OlS_datasubset <- lm(LogPopBio ~ Time, data = data)
Qua_datasubset <- lm(LogPopBio ~ Time + I(Time^2) , data = data)
Cub_data <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = data)

logistic <- function(t, r, K, N0){
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}                     # logistic model in log scale

logistic <- nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), 
                  data = data_subset,
                  list(K = max(data$LogPopBio),
                       N0 = data$LogPopBio[which.min(data$Time_series)],
                       r = r_val))

timepoints <- seq(min(data$Time), max(data$Time), 0.5) 



logistic_points <- logistic(t = timepoints,
                            r = coef(data_subset)["r"],
                            K = coef(data_subset)["K"],
                            N = coef(data_subset)["N0"])

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
Plotting_all <- function(data, ...) {
  
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
  
}
    

Plotting_all (data_subset = .)
    
    
    




