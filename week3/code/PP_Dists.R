#Write a script that draws and saves three figures, 
# each containing subplots of distributions of predator mass, prey mass, 
# and the size ratio of prey mass over predator mass by feeding interaction type. 
# Use logarithms of masses (or size ratios)for all three plots. 
# In addition, the script should calculate the (log) mean and median predator mass, 
# prey mass and predator-prey size-ratios to a csv file.

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

##### Add a column to store the ratio output #####
MyDF <- transform(MyDF, Predator.Prey.Ratio = Predator.mass / Prey.mass)
View(MyDF)

##### Subplots for Predator mass distribution by feeding interaction types #####
pdf("../results/Pred_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Pred_subplot1 <- hist(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
     main = "Distribution of Insectivorous Predator Mass")

par(mfg=c(2,1))
Pred_subplot2 <- hist(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance",
     main = "Distribution of Predacious/Piscivorous Predator Mass")

par(mfg=c(3,1))
Pred_subplot3 <- hist(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
     main = "Distribution of Piscivorous Predator Mass")

par(mfg=c(1,2))
Pred_subplot4 <- hist(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance",
     main = "Distribution of Planktivorous Predator Mass")

par(mfg=c(2,2))
Pred_subplot5 <- hist(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == "predacious"]), 
     xlab= "log10(Predator Mass (g))", ylab = "Abundance", 
     main = "Distribution of Predacious Predator Mass")

dev.off();


##### Subplots for Prey mass distribution by feeding interaction types #####
pdf("../results/Prey_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Prey_subplot1 <- hist(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Insectivorous Feeding")

par(mfg=c(2,1))
Prey_subplot2 <- hist(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance",
                      main = "Distribution of Prey mass in Predacious/Piscivorous Feeding")

par(mfg=c(3,1))
Prey_subplot3 <- hist(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Piscivorous Feeding")

par(mfg=c(1,2))
Prey_subplot4 <- hist(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance",
                      main = "Distribution of Prey mass in Planktivorous Feeding")

par(mfg=c(2,2))
Prey_subplot5 <- hist(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == "predacious"]), 
                      xlab= "log10(Prey Mass (g))", ylab = "Abundance", 
                      main = "Distribution of Prey mass in Predacious Feeding")

dev.off();

##### Subplots for predator-prey size-ratio distribution by feeding interaction types #####
pdf("../results/SizeRatio_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))

par(mfg=c(1,1))
Prey_subplot1 <- hist(log10(MyDF$Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "insectivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Insectivorous Feeding")

par(mfg=c(2,1))
Prey_subplot2 <- hist(log10(MyDF$Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "predacious/piscivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance",
                      main = "Distribution of predator-prey size-ratio in Predacious/Piscivorous Feeding")

par(mfg=c(3,1))
Prey_subplot3 <- hist(log10(MyDF$Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "piscivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Piscivorous Feeding")

par(mfg=c(1,2))
Prey_subplot4 <- hist(log10(MyDF$Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "planktivorous"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance",
                      main = "Distribution of predator-prey size-ratio in Planktivorous Feeding")

par(mfg=c(2,2))
Prey_subplot5 <- hist(log10(MyDF$Predator.Prey.Ratio[MyDF$Type.of.feeding.interaction == "predacious"]), 
                      xlab= "Predator-prey size-ratio", ylab = "Abundance", 
                      main = "Distribution of predator-prey size-ratio in Predacious Feeding")

dev.off();



###### create new dataframe to store new calculations #####
mean_pred <- tapply(MyDF$Predator.mass, MyDF$Type.of.feeding.interaction, mean) # calculate mean predator mass by feeding type
log_mean_pred <- c(log10(mean_pred)) # calculate log 10 of mean predator mass by feeding type, then save as vector

mean_prey <- tapply(MyDF$Prey.mass, MyDF$Type.of.feeding.interaction, mean) # calculate mean prey mass by feeding type
log_mean_prey <- c(log10(mean_prey)) # calculate log 10 of mean prey mass by feeding type, then save as vector

median_pred <- tapply(MyDF$Predator.mass, MyDF$Type.of.feeding.interaction, median) # calculate median predator mass by feeding type
log_median_pred <- c(log10(median_pred)) # calculate log 10 of median predator mass by feeding type, then save as vector

median_prey <- tapply(MyDF$Prey.mass, MyDF$Type.of.feeding.interaction, median) # calculate median prey mass by feeding type
log_median_prey <- c(log10(median_prey)) # calculate log 10 of median prey mass by feeding type, then save as vector

mean_ratio <- c(tapply(MyDF$Predator.Prey.Ratio, MyDF$Type.of.feeding.interaction, mean)) # calculate mean predator-prey size ratio by feeding type
median_ratio <- c(tapply(MyDF$Predator.Prey.Ratio, MyDF$Type.of.feeding.interaction, median)) # calculate median predator-prey size ratio by feeding type, then save as vector

New_Results <- data.frame(log_mean_pred, log_mean_prey, log_median_pred, log_median_prey, mean_ratio, median_ratio) #create new dataframe
names(New_Results) <- c("Mean.log10.Predator.Mass", "Mean.log10.Prey.Mass", "Median.log10.Predator.Mass", "Median.log10.Prey.Mass", "Mean.Predator.Prey.Size.Ratio", "Median.Predator.Prey.Size.Ratio")

write.csv(New_Results, "../results/PP_results.csv") #store new dataframe output as csv



