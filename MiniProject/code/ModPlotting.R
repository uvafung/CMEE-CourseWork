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
