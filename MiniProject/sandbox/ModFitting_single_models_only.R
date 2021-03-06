# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 23, 2021
# File name: ModFitting.R
# Description: This script imports modified data for model fitting. 


rm(list=ls())
require(ggplot2)
require(minpack.lm)
require(tidyverse)
require(broom)

data = read.csv("../data/ModifiedLogisticGrowthData.csv") # import modified data into script
Subset_ID95 <- subset(data, ID_no_Rep_dup == "95_1", select=X:ID_no_Rep_dup) # Subset data with ID 95


########### 1 -- OLS Model #############
OLS_ID95 <- lm(LogPopBio ~ Time, data = data) # fit lm with log scale
summary(OLS_ID95)

r_val <- coef(summary(OLS_ID95))["Time","Estimate"] # extract r estimate and save it as in a new variable

AIC(OLS_ID95)
BIC(OLS_ID95)


########### 2 -- Quadratic equation #############
Qua_ID95 <- lm(LogPopBio ~ Time + I(Time^2) , data = Subset_ID95) # Fit lm with quadratic equation
summary(Qua_ID95)

AIC(Qua_ID95)
BIC(Qua_ID95)


########### 3 -- Cubic equation #############
Cub_ID95 <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = Subset_ID95) # fit lm with cubic equation
summary(Cub_ID95)

AIC(Cub_ID95)
BIC(Cub_ID95)


########### 4 -- Logistic model #############
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

N0_min <- min(Subset_ID95$LogPopBio) # lowest population size
K_max <- max(Subset_ID95$LogPopBio) # highest population size
r_est <- r_val # estimate from OLS fitting

Logistic_ID95 <- nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = Subset_ID95, list(r = r_est, K = K_max, N0 = N0_min), trace = T)
summary(Logistic_ID95) # nls logistics output summary

AIC(Logistic_ID95)
BIC(Logistic_ID95)


########### 5 -- Gompertz model #############
gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
}   

N0_gstart <- min(Subset_ID95$LogPopBio) # lowest population size
K_gstart <- max(Subset_ID95$LogPopBio) # highest population size
r_gstart <- r_val # estimate from OLS fitting
t_lag_gstart <- Subset_ID95$Time[which.max(diff(diff(Subset_ID95$LogPopBio)))] # find last timepoint of lag phase


Gompertz_ID95 <- nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = Subset_ID95,
                      list(t_lag = t_lag_gstart, r = r_gstart, N0 = N0_gstart, K = K_gstart), 
                      control = list(maxiter = 500)) # increase max no of iterations
summary(Gompertz_ID95)


AIC(Gompertz_ID95)
BIC(Gompertz_ID95)



########### 6 -- Baranyi model #############
baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                         exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                               exp(-r * t_lag) - exp(-r * 
                                                                           (t + t_lag)))))-1)/(exp(K-N0))))}


N0_bstart <- min(Subset_ID95$PopBio) # lowest population size
K_bstart <- max(Subset_ID95$PopBio) # highest population size
r_bstart <- r_val # needs maximum specific growth rate
t_lag_bstart <- Subset_ID95$Time[which.max(diff(diff(Subset_ID95$LogPopBio)))] # find last timepoint of lag phase


Baranyi_ID95 <- nlsLM(LogPopBio ~ baranyi(t = Time, r, K, N0, t_lag), data = Subset_ID95,
                      list(t_lag = t_lag_bstart, r = r_bstart, N0 = N0_bstart, K = K_bstart), 
                      control = list(maxiter = 1000), trace = T) # increase max no of iterations
summary(Baranyi_ID95)

AIC(Baranyi_ID95)
BIC(Baranyi_ID95)


############### Plotting the graph ###################
timepoints <- seq(min(Subset_ID95$Time), max(Subset_ID95$Time), 0.5) 

logistic_points <- logistic(t = timepoints,  # time points for logistic equation
                            r = coef(Logistic_ID95)["r"], 
                            K = coef(Logistic_ID95)["K"], 
                            N = coef(Logistic_ID95)["N0"])

gompertz_points <- gompertz(t = timepoints,  # time points for gompertz equation
                            r = coef(Gompertz_ID95)["r"], 
                            K = coef(Gompertz_ID95)["K"], 
                            N = coef(Gompertz_ID95)["N0"], 
                            t_lag = coef(Gompertz_ID95)["t_lag"])

baranyi_points <- baranyi(t = timepoints,  # time points for gompertz equation
                            r = coef(Baranyi_ID95)["r"], 
                            K = coef(Baranyi_ID95)["K"], 
                            N = coef(Baranyi_ID95)["N0"], 
                            t_lag = coef(Baranyi_ID95)["t_lag"])

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


combineplot <- ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", col = "red", se = F) + # line for OLS
  geom_smooth (method= "lm", col = "orange", formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")

print(combineplot)
