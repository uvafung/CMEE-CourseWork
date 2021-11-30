# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 23, 2021
# File name: ModFitting.R
# Description: This script imports modified data for model fitting. 


rm(list=ls())
require(ggplot2)
require(minpack.lm)

data = read.csv("../data/ModifiedLogisticGrowthData.csv") # import modified data into script
Subset_ID95 <- subset(data, ID_no == "95", select=X:ID_no) # Subset data with ID 95


########### Model fitting -- OLS #############
OLS_ID95 <- lm(LogPopBio ~ Time, data = Subset_ID95) # fit lm with log scale
summary(OLS_ID95)

ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(x = "Time", y = "log(Population Size)")


########### Model fitting -- Quadratic equation #############
Qua_ID95 <- lm(LogPopBio ~ Time + I(Time^2) , data = Subset_ID95) # Fit lm with quadratic equation
summary(Qua_ID95)

plot <- ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")

print(plot)


########### Model fitting -- Cubic equation #############
Cub_ID95 <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = Subset_ID95) # fit lm with cubic equation
summary(Cub_ID95)

plot <- ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) + 
  geom_point(size = 2.5) + 
  geom_smooth (method= "lm", formula = y ~ x + I(x^2) + I(x^3), se=F) +
  theme_bw() +
  theme(aspect.ratio=1) + 
  labs(x = "Time", y = "log(Population Size)")

print(plot)


########### Model fitting -- Logistic model #############
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

N0_min <- min(Subset_ID95$LogPopBio) # lowest population size
K_max <- max(Subset_ID95$LogPopBio) # highest population size
r_est <- 0.003827 # this one is a random no. -- should use our estimate from the OLS fitting from above

Logistic_ID95 <- nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = Subset_ID95, list(r = r_est, K = K_max, N0 = N0_min), trace = T)
summary(Logistic_ID95) # nls logistics output summary


########## Plotting the graph ###############
timepoints <- seq(min(Subset_ID95$Time), max(Subset_ID95$Time), 0.5) ### Plot nls line

logistic_points <- logistic(t = timepoints, 
                            r = coef(Logistic_ID95)["r"], 
                            K = coef(Logistic_ID95)["K"], 
                            N = coef(Logistic_ID95)["N0"])


df1 <- data.frame(timepoints, logistic_points)
df1$model <- "Logistic equation"
names(df1) <- c("Time", "N", "model")


ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +   ### Plot nls model
  geom_point(size = 3) +
  geom_line(data = df1, aes(x = Time, y = N, col = model), size = 1) +
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "Cell number")




########### Model fitting -- Gompertz model #############
gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
}   

N0_gstart <- min(Subset_ID95$LogPopBio) # lowest population size, note log scale
K_gstart <- max(Subset_ID95$LogPopBio) # highest population size, note log scale
r_gstart <- 0.0012 # use our previous estimate from the OLS fitting from above
t_lag_gstart <- Subset_ID95$Time[which.max(diff(diff(Subset_ID95$LogPopBio)))] # find last timepoint of lag phase


fit_gompertz <- nlsLM(LogPopBio ~ gompertz(t = Time, r, K, N0, t_lag), data = Subset_ID95,
                      list(t_lag = t_lag_gstart, r = r_gstart, N0 = N0_gstart, K = K_gstart), 
                      control = list(maxiter = 500)) # increase max no of iterations
summary(fit_gompertz)





############### Plotting the graph ###################
timepoints <- seq(min(Subset_ID95$Time), max(Subset_ID95$Time), 0.5) ### Plot nls line

logistic_points <- logistic(t = timepoints, 
                            r = coef(Logistic_ID95)["r"], 
                            K = coef(Logistic_ID95)["K"], 
                            N = coef(Logistic_ID95)["N0"])

gompertz_points <- gompertz(t = timepoints, 
                            r = coef(fit_gompertz)["r"], 
                            K = coef(fit_gompertz)["K"], 
                            N = coef(fit_gompertz)["N0"], 
                            t_lag = coef(fit_gompertz)["t_lag"])

df1 <- data.frame(timepoints, logistic_points)
df1$model <- "Logistic model"
names(df1) <- c("Time", "LogPopBio", "model")

df2 <- data.frame(timepoints, gompertz_points)
df2$model <- "Gompertz model"
names(df2) <- c("Time", "LogPopBio", "model")

model_frame <- rbind(df1, df2)

ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_line(data = model_frame, aes(x = Time, y = LogN, col = model), size = 1) +
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")

plottt <- ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # adds line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogN, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")

print(plottt)
