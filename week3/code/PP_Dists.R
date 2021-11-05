# Author: Uva Fung
# Date: Nov 5 2021
# Description: Draws and saves three figures, each containing subplots of distributions of predator mass, 
# prey mass, and the size ratio of prey mass over predator mass by feeding interaction type. 
# The script also calculates the (log) mean and median predator mass, prey mass and predator-prey size-ratios to a csv file.

rm(list = ls())
require(tidyverse)

### Read csv
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")


### Convert all units from mg into grams
MyDF$Prey.mass.g <- ifelse(grepl("mg", MyDF$Prey.mass.unit), MyDF$Prey.mass * 0.001, MyDF$Prey.mass)

##### Add a column to store the ratio output #####
MyDF <- transform(MyDF, Predator.Prey.Ratio = Prey.mass.g / Predator.mass)

##### Add columns to store the log10 outputs #####
MyDF <- transform(MyDF, Log.Predator.mass = log10(Predator.mass))
MyDF <- transform(MyDF, Log.Prey.mass.g = log10(Prey.mass.g))
MyDF <- transform(MyDF, Log.Predator.Prey.Ratio = log10(Predator.Prey.Ratio))

View(MyDF)

##### Subplots for Predator mass distribution by feeding interaction types #####
pdf("../results/Pred_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Pred_subplot1 <- hist((MyDF$Log.Predator.mass[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
main = "Distribution of Insectivorous Predator Mass")

par(mfg=c(2,1))
Pred_subplot2 <- hist((MyDF$Log.Predator.mass[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance",
     main = "Distribution of Predacious/Piscivorous Predator Mass")

par(mfg=c(3,1))
Pred_subplot3 <- hist((MyDF$Log.Predator.mass[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
     main = "Distribution of Piscivorous Predator Mass")

par(mfg=c(1,2))
Pred_subplot4 <- hist((MyDF$Log.Predator.mass[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance",
     main = "Distribution of Planktivorous Predator Mass")

par(mfg=c(2,2))
Pred_subplot5 <- hist((MyDF$Log.Predator.mass[MyDF$Type.of.feeding.interaction == "predacious"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
     main = "Distribution of Predacious Predator Mass")

dev.off();


##### Subplots for Prey mass distribution by feeding interaction types #####
pdf("../results/Prey_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Prey_subplot1 <- hist((MyDF$Log.Prey.mass.g[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Insectivorous Feeding")

par(mfg=c(2,1))
Prey_subplot2 <- hist((MyDF$Log.Prey.mass.g[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance",
                      main = "Distribution of Prey mass in Predacious/Piscivorous Feeding")

par(mfg=c(3,1))
Prey_subplot3 <- hist((MyDF$Log.Prey.mass.g[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Piscivorous Feeding")

par(mfg=c(1,2))
Prey_subplot4 <- hist((MyDF$Log.Prey.mass.g[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance",
                      main = "Distribution of Prey mass in Planktivorous Feeding")

par(mfg=c(2,2))
Prey_subplot5 <- hist((MyDF$Log.Prey.mass.g[MyDF$Type.of.feeding.interaction == "predacious"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Predacious Feeding")

dev.off();

##### Subplots for predator-prey size-ratio distribution by feeding interaction types #####
pdf("../results/SizeRatio_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Ratio_subplot1 <- hist((MyDF$Log.Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Insectivorous Feeding")

par(mfg=c(2,1))
Ratio_subplot2 <- hist((MyDF$Log.Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance",
                      main = "Distribution of predator-prey size-ratio in Predacious/Piscivorous Feeding")

par(mfg=c(3,1))
Ratio_subplot3 <- hist((MyDF$Log.Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Piscivorous Feeding")

par(mfg=c(1,2))
Ratio_subplot4 <- hist((MyDF$Log.Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance",
                      main = "Distribution of predator-prey size-ratio in Planktivorous Feeding")

par(mfg=c(2,2))
Ratio_subplot5 <- hist((MyDF$Log.Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "predacious"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Predacious Feeding")

dev.off();

###### create new dataframe to store new calculations #####
mean_logpred <- c(tapply(MyDF$Log.Predator.mass, MyDF$Type.of.feeding.interaction, mean)) # calculate mean predator mass by feeding type

mean_logprey <- c(tapply(MyDF$Log.Prey.mass.g, MyDF$Type.of.feeding.interaction, mean)) # calculate mean prey mass by feeding type

median_logpred <- c(tapply(MyDF$Log.Predator.mass, MyDF$Type.of.feeding.interaction, median)) # calculate median predator mass by feeding type

median_logprey <- c(tapply(MyDF$Log.Prey.mass.g, MyDF$Type.of.feeding.interaction, median)) # calculate median prey mass by feeding type

mean_logratio <- c(tapply(MyDF$Log.Predator.Prey.Ratio, MyDF$Type.of.feeding.interaction, mean)) # calculate mean predator-prey size ratio by feeding type
median_logratio <- c(tapply(MyDF$Log.Predator.Prey.Ratio, MyDF$Type.of.feeding.interaction, median)) # calculate median predator-prey size ratio by feeding type, then save as vector

New_Results <- data.frame(mean_logpred, mean_logprey, median_logpred, median_logprey, mean_logratio, median_logratio) #create new dataframe
names(New_Results) <- c("Mean.log10.Predator.Mass", "Mean.log10.Prey.Mass", "Median.log10.Predator.Mass", "Median.log10.Prey.Mass", "Mean.Predator.Prey.Size.Ratio", "Median.Predator.Prey.Size.Ratio")

write.csv(New_Results, "../results/PP_results.csv") #store new dataframe output as csv

print("Script completes!")   # print to show that script is working
