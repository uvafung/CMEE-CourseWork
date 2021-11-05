# Author: Uva Fung
# Date: Nov 5 2021
# Description: calculate the correlation coefficients for Florida temperature data

rm(list=ls())

load("../data/KeyWestAnnualMeantemperature.RData")
ls()
class(ats)
head(ats)

### Scatter plot of Temp ~ year with regression line ###
plot(ats, xlab="Year", ylab ="Temperature in degree Celsius")
title("(1)", adj = 0)
abline(lm(Temp ~ Year, data = ats), col = "blue")


Year <- as.factor(ats$Year)
Temp <- as.factor(ats$Temp)

cc <- cor(ats$Year, ats$Temp, method = "pearson") # calculate correlation between successive years and save as cc
cc

### Permutation analysis ###
random <- rep(NA, 1000) #set as function

for (i in 1:1000){
  random[i] <- cor(ats$Year, sample(ats$Temp, replace = F)) # calculate correlation coefficient 1000 times using randomly generated temp
}

print(random)

hist(random) 

p_value = length(random[random>cc]) # number of random correlation coefficients with value greater than the observed one
fraction = p_value/1000
fraction

### Histogram for random correlation coefficients ###
random_hist <- hist(random, xlim=c(-1,1), 
     main ="",
     xlab="Correlation coefficients")
abline(v = cc, col = "red")
title("(2)", adj = 0)


pdf("../results/Florida_warming_fig.pdf", height=6, width=6) 

par(mfrow=c(2,1)) 

plot(ats, xlab="Year", ylab ="Temperature in degree Celsius")
title("(1a)", adj = 0)
abline(lm(Temp ~ Year, data = ats), col = "blue")

random_hist <- hist(random, xlim=c(-1,1), 
                    main ="",
                    xlab="Correlation coefficients")
abline(v = cc, col = "red")
title("(1b)", adj = 0)

dev.off()

print("Script completes!")   # print when run with source() to show that script is working



