# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 29, 2021
# File name: ModPlotting_para.R
# Description: This script imports modified data for graph plotting. 

rm(list=ls())
require(ggplot2)
require(minpack.lm)
require(tidyverse)
require(broom)

data = read.csv("../data/ModifiedLogisticGrowthData.csv")
Subset_ID95 <- subset(data, ID_no_Rep_dup == "95_1", select=X:ID_no_Rep_dup) # Subset data with ID 95

para = read.csv("../data/Parameters_nLS.csv")
rownames(para) <- para$ID_no_Rep_dup


########### 1 -- OLS Model #############
OLS_ID95 <- lm(LogPopBio ~ Time, data = Subset_ID95) # fit lm with log scale
summary(OLS_ID95)

r_val <- coef(summary(OLS_ID95))["Time","Estimate"] # extract r estimate and save it as in a new variable


########### 2 -- Quadratic equation #############
Qua_ID95 <- lm(LogPopBio ~ Time + I(Time^2) , data = Subset_ID95) # Fit lm with quadratic equation
summary(Qua_ID95)


########### 3 -- Cubic equation #############
Cub_ID95 <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = Subset_ID95) # fit lm with cubic equation
summary(Cub_ID95)


########### 4 -- Logistic model #############
logistic <- function(t, r, K, N0){                      # logistic model in log scale
  return((N0 * K * exp(r * t))/(K+N0 * (exp(r * t)-1)))
}

########### 5 -- Gompertz model #############
gompertz <- function(t, r, K, N0, t_lag){     # Modified gompertz growth model (Zwietering 1990)
  return(N0 + (K - N0) * exp(-exp(r * exp(1) * (t_lag - t)/((K - N0) * log(10)) + 1)))
}   

########### 6 -- Baranyi model #############
baranyi <- function(t=Time, r, K, N0, t_lag) {N0 + r * (t + (1/r) * log(exp(-r*t) + 
                                                                          exp(-r * t_lag) - exp(-r * (t + t_lag)))) - 
    log(1 + ((exp(r * (t + (1/r) * log(exp(-r*t) +
                                         exp(-r * t_lag) - exp(-r * 
                                                                 (t + t_lag)))))-1)/(exp(K-N0))))}

timepoints <- seq(min(Subset_ID95$Time), max(Subset_ID95$Time), 0.5) 

logistic_points <- logistic(t = timepoints,  # time points for logistic equation
                            r = para['95_1', 'Log_estimate_r'], 
                            K = para['95_1', 'Log_estimate_K'], 
                            N = para['95_1', 'Log_estimate_N0'])

gompertz_points <- gompertz(t = timepoints,  # time points for gompertz equation
                            r = para['95_1', 'Gom_estimate_r'], 
                            K = para['95_1', 'Gom_estimate_K'], 
                            N = para['95_1', 'Gom_estimate_N0'], 
                            t_lag = para['95_1', 'Gom_estimate_t_lag'])

baranyi_points <- baranyi(t = timepoints,  # time points for gompertz equation
                          r = para['95_1', 'Bar_estimate_r'], 
                          K = para['95_1', 'Bar_estimate_K'], 
                          N = para['95_1', 'Bar_estimate_N0'], 
                          t_lag = para['95_1', 'Bar_estimate_t_lag'])

baranyi_points <- log(baranyi_points)

gompertz_mult_points <- gompertz(t = timepoints,  # time points for gompertz equation
                            r = para['95_1', 'Gom_mult_Gom_estimate_r'], 
                            K = para['95_1', 'Gom_mult_Gom_estimate_K'], 
                            N = para['95_1', 'Gom_mult_Gom_estimate_N0'], 
                            t_lag = para['95_1', 'Gom_mult_Gom_estimate_t_lag'])

baranyi_mult_points <- baranyi(t = timepoints,  # time points for gompertz equation
                          r = para['95_1', 'Bar_mult_Bar_estimate_r'], 
                          K = para['95_1', 'Bar_mult_Bar_estimate_K'], 
                          N = para['95_1', 'Bar_mult_Bar_estimate_N0'], 
                          t_lag = para['95_1', 'Bar_mult_Bar_estimate_t_lag'])




df1 <- data.frame(timepoints, logistic_points)
df1$model <- "Logistic model"
names(df1) <- c("Time", "LogPopBio", "model")

df2 <- data.frame(timepoints, gompertz_points)
df2$model <- "Gompertz model"
names(df2) <- c("Time", "LogPopBio", "model")

df3 <- data.frame(timepoints, baranyi_points)
df3$model <- "Baranyi model"
names(df3) <- c("Time", "LogPopBio", "model")

df4 <- data.frame(timepoints, gompertz_mult_points)
df4$model <- "Gompertz mult model"
names(df4) <- c("Time", "LogPopBio", "model")

df5 <- data.frame(timepoints, baranyi_mult_points)
df5$model <- "Baranyi mult model"
names(df5) <- c("Time", "LogPopBio", "model")

model_frame <- rbind(df1, df2, df3, df4, df5)

combineplot <- ggplot(Subset_ID95, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", aes(colour = "OLS model"), se = F) + # line for OLS
  geom_smooth (method= "lm", aes(colour = "Cubic model"), formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", aes(colour = "Quadratic model"), formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")+
  scale_colour_manual(name="legend", values=c("#F8766D", "#E7861B", "yellow", "#00BC59", "#80CDC1", "#CF78FF", "#00ABFD", "red")) 

print(combineplot)


  