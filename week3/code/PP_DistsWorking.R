#Write a script that draws and saves three figures, 
# each containing subplots of distributions of predator mass, prey mass, 
# and the size ratio of prey mass over predator mass by feeding interaction type. 
# Use logarithms of masses (or size ratios)for all three plots. 
# In addition, the script should calculate the (log) mean and median predator mass, 
# prey mass and predator-prey size-ratios to a csv file.

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

head(MyDF)
View(MyDF)
str(MyDF)


# Subplots for Predator
pdf("../results/Pred_Subplots.pdf", 11.7, 8.3) 
par(mfrow=c(3,2))
par(mar=c(3,3,3,3))

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





