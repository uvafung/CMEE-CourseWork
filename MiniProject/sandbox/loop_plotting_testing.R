rm(list=ls())
require(ggplot2)
require(minpack.lm)
require(tidyverse)
require(broom)

data = read.csv("../data/ModifiedLogisticGrowthData.csv")

Subset_ID95 <- subset(data, ID_no_Rep == "95_1", select=X:No_datapoints) # Subset data with ID 95
data_subset <- data %>% group_by(ID_no_Rep) # subset data 

data_subset <- data %>% group_by(ID_no_Rep) %>% filter(Medium == "TSB" & Temp == "5")



Plotting <- function(data, ...) {
  
  combineplot <- ggplot(data_subset, aes(x = Time, y = LogPopBio)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", col = "red", se = F) + # line for OLS
  geom_smooth (method= "lm", col = "orange", formula = y ~ x + I(x^2) + I(x^3), se=F) +  # line for cubic equation
  geom_smooth (method= "lm", formula = y ~ x + I(x^2), se=F) + # line for quadratic equation
  theme_bw() + # make the background white
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Population Size)") +
    facet_wrap(data_subset$ID_no_Rep)

  print(combineplot) 

}


Plotting (data_subset = .)
