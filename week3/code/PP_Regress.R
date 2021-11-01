rm(list=ls())

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

install.packages("tidyverse")
require(ggplot2)
require(dplyr)
require(tidyr)
require(plyr)
require(broom)
require(purrr)


### Calculate regression ###
MyDF <- MyDF %>% select("Predator.lifestage","Type.of.feeding.interaction", "Predator.mass", "Prey.mass")

NewDF1 <- MyDF %>%  # Regression test 1
  unite("Predlifestage.feeding", Predator.lifestage:Type.of.feeding.interaction) %>% # Merge Predator.lifestage and Feeding interaction columns into a new one
  group_by(Predlifestage.feeding) %>% # Group data by the new column
  do(RegressOutput = tidy(lm(log10(Predator.mass) ~ log10(Prey.mass), data = .))) %>% # Regression test that gives slope and intercept
  unnest(RegressOutput)

View(NewDF1)

NewDF2 <- MyDF %>% # Regression test 1
  unite("Predlifestage.feeding", Predator.lifestage:Type.of.feeding.interaction) %>% # Merge Predator.lifestage and Feeding interaction columns into a new one
  group_by(Predlifestage.feeding) %>% # Group data by the new column
  do(RegressOutput = glance(lm(log10(Predator.mass) ~ log10(Prey.mass), data = .))) %>% # Regression test that gives slope and intercept
  unnest(RegressOutput)

View(NewDF2)


### Data transformation ###
EditedDF1 <- select(NewDF1, -c(std.error, statistic, p.value)) # only retain columns for slope and intercept
View(EditedDF1)

PivotEditedDF1 <- EditedDF1 %>%
  pivot_wider(names_from = term, values_from = estimate)
  # Convert Dataframe from long to wide format -- storing slope and intercept in two columns
View(PivotEditedDF1)

EditedDF2 <- select(NewDF2, c(Predlifestage.feeding, r.squared, statistic, p.value)) # remove irrelevant columns
View(EditedDF2)


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
regress_plot1 <- ggplot(MyDF, aes(x = Prey.mass, y = Predator.mass, colour = Predator.lifestage)) + 
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