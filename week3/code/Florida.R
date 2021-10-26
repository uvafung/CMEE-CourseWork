rm(list=ls())
load("../data/KeyWestAnnualMeantemperature.RData")
ls()
class(ats)
head(ats)
plot(ats)

Year <- as.factor(ats$Year)
Temp <- as.factor(ats$Temp)

cc <- cor(ats$Year, ats$Temp, method = "pearson") # calculate correlation between successive years and save as cc
cc

random <- rep(NA, 1000) #set as function

for (i in 1:1000){
  random[i] <- cor(ats$Year, sample(ats$Temp, replace = F)) # calculate correlation coefficient 1000 times using randomly generated temp
}

print(random)

hist(random)

p_value = length(random[random>cc])

fraction = p_value/1000
fraction

hist(random, xlim=c(-1,1), 
     main ="Distribution of random correlation coefficients",
     xlab="Correlation coefficients")
abline(v = cc)


