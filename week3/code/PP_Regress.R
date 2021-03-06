# Author: Uva Fung
# Date: Nov 5 2021
# Description: produce regression results for Predator mass vs Prey mass


rm(list=ls())

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

require(ggplot2)
require(dplyr)
require(tidyr)
require(plyr)
require(broom)
require(purrr)

### Convert all units from mg into grams
MyDF$Prey.mass.g <- ifelse(grepl("mg", MyDF$Prey.mass.unit), MyDF$Prey.mass * 0.001, MyDF$Prey.mass)

### Calculate regression ###
MyDF <- MyDF %>% select("Predator.lifestage","Type.of.feeding.interaction", "Predator.mass", "Prey.mass.g")

NewDF1 <- MyDF %>%  # Regression test 1
  unite("Predlifestage.feeding", Predator.lifestage:Type.of.feeding.interaction) %>% # Merge Predator.lifestage and Feeding interaction columns into a new one
  group_by(Predlifestage.feeding) %>% # Group data by the new column
  do(RegressOutput = tidy(lm(log10(Predator.mass) ~ log10(Prey.mass.g), data = .))) %>% # Regression test that gives slope and intercept
  unnest(RegressOutput)

NewDF2 <- MyDF %>% # Regression test 1
  unite("Predlifestage.feeding", Predator.lifestage:Type.of.feeding.interaction) %>% # Merge Predator.lifestage and Feeding interaction columns into a new one
  group_by(Predlifestage.feeding) %>% # Group data by the new column
  do(RegressOutput = glance(lm(log10(Predator.mass) ~ log10(Prey.mass.g), data = .))) %>% # Regression test that gives slope and intercept
  unnest(RegressOutput)



### Data transformation ###
EditedDF1 <- select(NewDF1, -c(std.error, statistic, p.value)) # remove irrelevant columns

PivotEditedDF1 <- EditedDF1 %>%
  pivot_wider(names_from = term, values_from = estimate)
  # Convert Dataframe from long to wide format -- storing slope and intercept in two columns

EditedDF2 <- select(NewDF2, c(Predlifestage.feeding, r.squared, statistic, p.value)) # only retain useful columns



### Rename columns ###
names(PivotEditedDF1)[2] <- "intercept"
names(PivotEditedDF1)[3] <- "slope"
names(EditedDF2)[3] <- "f.statistic"

### Merge the two dataframes together ###
Regress_results <- merge(PivotEditedDF1, EditedDF2, by="Predlifestage.feeding")
View(Regress_results)

### print output ###
write.csv(Regress_results, "../results/PP_Regress_Results.csv") # save as new file in results folder



##### Regression subplots #####
regress_plot1 <- ggplot(MyDF, aes(x = Prey.mass.g, y = Predator.mass, colour = Predator.lifestage)) + 
  geom_point(shape = 3)  
  
regress_plot2 <- regress_plot1 +
  facet_wrap(Type.of.feeding.interaction ~., dir ="v", ncol = 1, strip.position = 'right') +
  scale_x_log10("Prey Mass in grams") + 
  scale_y_log10("Predator mass in grams") +
  stat_smooth(method="lm",fullrange=TRUE, se = TRUE, size=.5) + 
  guides(colour=guide_legend(nrow=1)) +
  theme_bw()

regress_plot3 <- regress_plot2 + theme(aspect.ratio = 0.67, 
                                       legend.position = "bottom", 
                                       legend.box ="horizontal", 
                                       plot.margin = unit(c(1,3,1,3), "cm"),
                                       strip.text = element_text(size = 5))

regress_plot3

##### Save figure as a separate pdf file #####
pdf("../results/Regression_subplot.pdf", 11.7, 8.3) 
print(regress_plot3)
dev.off()

print("Script completes!")   # print to show that script is working
