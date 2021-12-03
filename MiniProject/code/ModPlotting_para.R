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
Subset_ID131_1 <- subset(data, ID_no_Rep_dup == "131_1", select=X:ID_no_Rep_dup) # Subset data with ID 131_1
Subset_ID277_1 <- subset(data, ID_no_Rep_dup == "277_1", select=X:ID_no_Rep_dup)
Subset_ID259_1 <- subset(data, ID_no_Rep_dup == "259_1", select=X:ID_no_Rep_dup)

para = read.csv("../data/Parameters_nLS.csv")
rownames(para) <- para$ID_no_Rep_dup

#######################################################################
########## Graph ID131_1 -- Baranyi significant best fit ##############
#######################################################################

########### 1 -- OLS Model #############
OLS_ID131_1 <- lm(LogPopBio ~ Time, data = Subset_ID131_1) # fit lm with log scale
r_val <- coef(summary(OLS_ID131_1))["Time","Estimate"] # extract r estimate and save it as in a new variable


########### 2 -- Quadratic equation #############
Qua_ID131_1 <- lm(LogPopBio ~ Time + I(Time^2) , data = Subset_ID131_1) # Fit lm with quadratic equation


########### 3 -- Cubic equation #############
Cub_ID131_1 <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = Subset_ID131_1) # fit lm with cubic equation


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

timepoints <- seq(min(Subset_ID131_1$Time), max(Subset_ID131_1$Time), 0.5) 

logistic_points <- logistic(t = timepoints,  # time points for logistic equation
                            r = para['131_1', 'Log_estimate_r'], 
                            K = para['131_1', 'Log_estimate_K'], 
                            N = para['131_1', 'Log_estimate_N0'])

gompertz_points <- gompertz(t = timepoints,  # time points for gompertz equation
                            r = para['131_1', 'Gom_estimate_r'], 
                            K = para['131_1', 'Gom_estimate_K'], 
                            N = para['131_1', 'Gom_estimate_N0'], 
                            t_lag = para['131_1', 'Gom_estimate_t_lag'])

baranyi_points <- baranyi(t = timepoints,  # time points for gompertz equation
                          r = para['131_1', 'Bar_estimate_r'], 
                          K = para['131_1', 'Bar_estimate_K'], 
                          N = para['131_1', 'Bar_estimate_N0'], 
                          t_lag = para['131_1', 'Bar_estimate_t_lag'])

baranyi_points <- log(baranyi_points)

gompertz_mult_points <- gompertz(t = timepoints,  # time points for gompertz equation
                            r = para['131_1', 'Gom_mult_Gom_estimate_r'], 
                            K = para['131_1', 'Gom_mult_Gom_estimate_K'], 
                            N = para['131_1', 'Gom_mult_Gom_estimate_N0'], 
                            t_lag = para['131_1', 'Gom_mult_Gom_estimate_t_lag'])

baranyi_mult_points <- baranyi(t = timepoints,  # time points for gompertz equation
                          r = para['131_1', 'Bar_mult_Bar_estimate_r'], 
                          K = para['131_1', 'Bar_mult_Bar_estimate_K'], 
                          N = para['131_1', 'Bar_mult_Bar_estimate_N0'], 
                          t_lag = para['131_1', 'Bar_mult_Bar_estimate_t_lag'])




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
df4$model <- "Gompertz msp model"
names(df4) <- c("Time", "LogPopBio", "model")

df5 <- data.frame(timepoints, baranyi_mult_points)
df5$model <- "Baranyi msp model"
names(df5) <- c("Time", "LogPopBio", "model")

model_frame <- rbind(df1, df2, df3, df4, df5)

pdf("../results/ID131.pdf", height=6, width=6) 
combineplot <- ggplot(Subset_ID131_1, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", aes(colour = "OLS model"), se = F) + # line for OLS
  geom_smooth (method= "lm", aes(colour = "Cubic model"), formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", aes(colour = "Quadratic model"), formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")+
  scale_colour_manual(name="legend", values=c("#F8766D", "#E7861B", "#FFD700", "#00BC59", "blue", "#CF78FF", "#00ABFD", "red")) 

print(combineplot)
dev.off()


################ Graph for ID277_1 --  poor fit for all models ##############

########### 1 -- OLS Model #############
OLS_ID277_1 <- lm(LogPopBio ~ Time, data = Subset_ID277_1) # fit lm with log scale
r_val <- coef(summary(OLS_ID277_1))["Time","Estimate"] # extract r estimate and save it as in a new variable


########### 2 -- Quadratic equation #############
Qua_ID277_1 <- lm(LogPopBio ~ Time + I(Time^2) , data = Subset_ID277_1) # Fit lm with quadratic equation


timepoints1 <- seq(min(Subset_ID277_1$Time), max(Subset_ID277_1$Time), 0.5) 

logistic_points1 <- logistic(t = timepoints1,  # time points for logistic equation
                            r = para['277_1', 'Log_estimate_r'], 
                            K = para['277_1', 'Log_estimate_K'], 
                            N = para['277_1', 'Log_estimate_N0'])

gompertz_points1 <- gompertz(t = timepoints1,  # time points for gompertz equation
                            r = para['277_1', 'Gom_estimate_r'], 
                            K = para['277_1', 'Gom_estimate_K'], 
                            N = para['277_1', 'Gom_estimate_N0'], 
                            t_lag = para['277_1', 'Gom_estimate_t_lag'])

gompertz_mult_points1 <- gompertz(t = timepoints1,  # time points for gompertz equation
                                 r = para['277_1', 'Gom_mult_Gom_estimate_r'], 
                                 K = para['277_1', 'Gom_mult_Gom_estimate_K'], 
                                 N = para['277_1', 'Gom_mult_Gom_estimate_N0'], 
                                 t_lag = para['277_1', 'Gom_mult_Gom_estimate_t_lag'])



df11 <- data.frame(timepoints1, logistic_points1)
df11$model <- "Logistic model"
names(df11) <- c("Time", "LogPopBio", "model")

df21 <- data.frame(timepoints1, gompertz_points1)
df21$model <- "Gompertz model"
names(df21) <- c("Time", "LogPopBio", "model")

df41 <- data.frame(timepoints1, gompertz_mult_points1)
df41$model <- "Gompertz msp model"
names(df41) <- c("Time", "LogPopBio", "model")

model_frame1 <- rbind(df11, df21, df41)

pdf("../results/ID277.pdf", height=6, width=6) 
combineplot1 <- ggplot(Subset_ID277_1, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", aes(colour = "OLS model"), se = F) + # line for OLS
  geom_smooth (method= "lm", aes(colour = "Quadratic model"), formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  geom_line(data = model_frame1, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")+
  scale_colour_manual(name="legend", values=c("#00BC59", "blue", "#CF78FF", "#00ABFD", "red")) 

print(combineplot1)
dev.off()

################ Graph for ID259_1 --  two models both fit well ###########
########### 3 -- Cubic equation #############
Cub_ID259_1 <- lm(LogPopBio ~ Time + I(Time^2) + I(Time^3), data = Subset_ID259_1) # fit lm with cubic equation


timepoints2 <- seq(min(Subset_ID259_1$Time), max(Subset_ID259_1$Time), 0.5) 

gompertz_points2 <- gompertz(t = timepoints2,  # time points for gompertz equation
                            r = para['259_1', 'Gom_estimate_r'], 
                            K = para['259_1', 'Gom_estimate_K'], 
                            N = para['259_1', 'Gom_estimate_N0'], 
                            t_lag = para['259_1', 'Gom_estimate_t_lag'])

df22 <- data.frame(timepoints2, gompertz_points2)
df22$model <- "Gompertz model"
names(df22) <- c("Time", "LogPopBio", "model")

pdf("../results/ID259.pdf", height=6, width=6) 
combineplot2 <- ggplot(Subset_ID259_1, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth (method= "lm", aes(colour = "Cubic model"), formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_line(data = df22, aes(x = Time, y = LogPopBio, col = model), size = 1) + # se=F removes confidence interval
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)")+
  scale_colour_manual(name="legend", values=c("#FFD700", "#00BC59")) 

print(combineplot2)
dev.off()



