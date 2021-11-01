MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
dim(MyDF)
str(MyDF)
head(MyDF)
require(tidyverse) #load tidyverse
dplyr::glimpse(MyDF)
MyDF$Type.of.feeding.interaction <- as.factor(MyDF$Type.of.feeding.interaction) # save it as a factor
MyDF$Location <- as.factor(MyDF$Location) # save as a factor
str(MyDF)

### Scatterplot ###
plot(MyDF$Predator.mass, MyDF$Prey.mass) # scatterplot
plot(log(MyDF$Predator.mass),log(MyDF$Prey.mass)) # scatterplot with log scale
plot(log10(MyDF$Predator.mass),log10(MyDF$Prey.mass)) # scatterplot with log10 scale
plot(log10(MyDF$Predator.mass),log10(MyDF$Prey.mass), pch=20) # scatterplot with log10 scale, change marker
plot(log10(MyDF$Predator.mass),log10(MyDF$Prey.mass), pch=20, 
     xlab = "Predator Mass (g)", ylab ="Prey Mass (g)"    # add x and y axis labels
     )

### Historgram ###
hist(MyDF$Predator.mass)
hist(log10(MyDF$Predator.mass),
     xlab = "log10(PredatorMass (g))", ylab = "Count") # change to log10 scale, add labels
hist(log10(MyDF$Predator.mass),
     xlab = "log10(PredatorMass (g))", ylab = "Count",
     col = "lightblue", border = "pink") # change borders and colour
hist(log10(MyDF$Prey.mass),
     xlab = "log10(PreyMass (g))", ylab = "Count",
     col = "lightblue", border = "pink") # change borders and colour

### Subplots ###
par(mfcol=c(2,1)) #initialize multi-panel plot
par(mfg=c(1,1)) # specify which subplot to use first
hist(log10(MyDF$Predator.mass),
     xlab = "log10(Predator Mass (g))", ylab = "Count", col = "lightblue", border = "pink", 
     main = 'Predator') # main = Add title
par(mfg = c(2,1)) # Second sub-plot
hist(log10(MyDF$Prey.mass),
     xlab = "log10(Prey Mass (g))", ylab = "Count", col = "lightgreen", border = "pink", 
     main = 'Prey') # main = Add title

### Overlaying plots ###
hist(log10(MyDF$Predator.mass), # Predator histogram
     xlab="log10(Body Mass (g))", ylab="Count", 
     col = rgb(1, 0, 0, 0.5), # Note 'rgb', fourth value is transparency
     main = "Predator-prey size Overlap") 
hist(log10(MyDF$Prey.mass), col = rgb(0, 0, 1, 0.5), add = T) # Plot prey
legend('topleft',c('Predators','Prey'),   # Add legend
       fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))) # Define legend colors

### Boxplots ###
boxplot(log10(MyDF$Predator.mass), xlab = "Location", ylab = "log10(Predator Mass)", main = "Predator mass")
boxplot(log(MyDF$Predator.mass) ~ MyDF$Location, # the tilde (~) tell R to subdivide or categorize your analysis and plot by the “Factor” location
        xlab = "Location", ylab = "Predator Mass",
        main = "Predator mass by location")
boxplot(log(MyDF$Predator.mass) ~ MyDF$Type.of.feeding.interaction,
        xlab = "Location", ylab = "Predator Mass",
        main = "Predator mass by feeding interaction type")

### Combining plot types ###
par(fig=c(0,0.8,0,0.8)) # specify figure size as proportion
plot(log(MyDF$Predator.mass),log(MyDF$Prey.mass), xlab = "Predator Mass (g)", ylab = "Prey Mass (g)") # Add labels
par(fig=c(0,0.8,0.4,1), new=TRUE)
boxplot(log(MyDF$Predator.mass), horizontal=TRUE, axes=FALSE)
par(fig=c(0.55,1,0,0.8),new=TRUE)
boxplot(log(MyDF$Prey.mass), axes=FALSE)
mtext("Fancy Predator-prey scatterplot", side=3, outer=TRUE, line=-3)


### Save graphics ###
pdf("../results/Pred_Prey_Overlay.pdf", # Open blank pdf page using a relative path
    11.7, 8.3) # These numbers are page dimensions in inches
hist(log(MyDF$Predator.mass), # Plot predator histogram (note 'rgb')
     xlab="Body Mass (g)", ylab="Count", col = rgb(1, 0, 0, 0.5), main = "Predator-Prey Size Overlap") 
hist(log(MyDF$Prey.mass), # Plot prey weights
     col = rgb(0, 0, 1, 0.5), 
     add = T)  # Add to same plot = TRUE
legend('topleft',c('Predators','Prey'), # Add legend
       fill=c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))) 
dev.off()


### qplot ###
require(ggplot2)
qplot(Prey.mass, Predator.mass, data = MyDF)
qplot(log(Prey.mass), log(Predator.mass), data = MyDF)
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, colour = Type.of.feeding.interaction, asp = 1) # categorize with colour
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, shape = Type.of.feeding.interaction, asp = 1) # categorize with shape
qplot(log(Prey.mass), log(Predator.mass), 
      data = MyDF, colour = "red")
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, colour = I("red")) # real red
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, size = 3) #with ggplot size mapping
qplot(log(Prey.mass), log(Predator.mass),  data = MyDF, size = I(3)) #no mapping
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, shape= I(3))
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, colour = Type.of.feeding.interaction, alpha =I(.5))

qplot(log(Prey.mass), log(Predator.mass), data = MyDF, geom = c("point", "smooth"))
qplot(log(Prey.mass), log(Predator.mass), data = MyDF, geom = c("point", "smooth")) + geom_smooth(method = "lm")

qplot(log(Prey.mass), log(Predator.mass), data = MyDF, geom = c("point", "smooth"),
      colour = Type.of.feeding.interaction) + geom_smooth(method = "lm",fullrange = TRUE)

p <- ggplot(MyDF, aes(x = log(Predator.mass),
                      y = log(Prey.mass),
                      colour = Type.of.feeding.interaction))
q <- p + 
  geom_point(size=I(2), shape=I(10)) +
  theme_bw() + # make the background white
  theme(aspect.ratio=1) # make the plot square
q



### Plotting a matrix ###
require(reshape2)
GenerateMatrix <- function(N){
  M <- matrix(runif(N * N), N, N)
  return(M)
}
M <- GenerateMatrix(10)
Melt <- melt(M)
p <- ggplot(Melt, aes(Var1, Var2, fill = value)) + geom_tile() + theme(aspect.ratio = 1)
p
