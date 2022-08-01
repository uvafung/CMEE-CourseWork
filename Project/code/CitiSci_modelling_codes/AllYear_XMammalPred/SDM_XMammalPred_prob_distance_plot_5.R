# Name: SDM_XMammalPred_prob_distance_plot_5.R
# Description: This script is used to: 
# 1) Create a plot that shows the change in presence probability
# with distance from city centre for hedgehogs, foxes and badgers
# Author: Uva Fung uf21@ic.ac.uk
# Date: July 21 2022

rm(list=ls())

require(dplyr)
require(raster)
require(ggplot2)
require(tidyverse)

setwd("Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear_XMammalPred")

# load dataset
badger <- read.csv("../../../results/CitiSci/AllYear_XMammalPred/Badger/enviroplot.csv")
fox <- read.csv("../../../results/CitiSci/AllYear_XMammalPred/Fox/enviroplot.csv")
hog <- read.csv("../../../results/CitiSci/AllYear_XMammalPred/Hedgehog/enviroplot.csv")

# select and rename columns
badger <- badger %>% dplyr::select(Easting, Northing, predictions) %>% dplyr::rename(Badger_pred = predictions)
fox <- fox %>% dplyr::select(Easting, Northing, predictions) %>% dplyr::rename(fox_pred = predictions)
hog <- hog %>% dplyr::select(Easting, Northing, predictions) %>% dplyr::rename(hog_pred = predictions)

# merge columns
a <- merge(badger, fox, by = c('Easting', 'Northing')) %>% distinct(Easting, Northing, .keep_all = TRUE)
b <- merge(a, hog, by = c('Easting', 'Northing')) %>% distinct(Easting, Northing, .keep_all = TRUE)

# save merged df
write.csv(b, "../../../results/CitiSci/AllYear_XMammalPred/enviropred_alltargetspp.csv")



# load merged df
pred <- read.csv("../../../results/CitiSci/AllYear_XMammalPred/enviropred_alltargetspp.csv")

# find centroid of datpoints
E_centroid <- mean(pred$Easting)
N_centroid <- mean(pred$Northing)


max(pred$Northing) - min(pred$Northing)
max(pred$Easting) - min(pred$Easting)



pred <- pred %>% add_column(Grp = NA)


# updated Jul 24 
pred <- read.csv("../../../data/distance_spp_prediction_Jul23.csv")

# check presence of NA
sum(is.na(pred$Badger_pre))
sum(is.na(pred$fox_pred))
sum(is.na(pred$hog_pred))

# calculate mean prediction score for each distance buffer
a <- pred %>% filter(buffer_km %in% 1:32) %>% group_by(buffer_km) %>%
  summarise(mean=mean(Badger_pre)) %>% rename(badger_pred = mean)

b <- pred %>% filter(buffer_km %in% 1:32) %>% group_by(buffer_km) %>%
  summarise(mean=mean(fox_pred)) %>% rename(fox_pred = mean)

c <- pred %>% filter(buffer_km %in% 1:32) %>% group_by(buffer_km) %>%
  summarise(mean=mean(hog_pred, na.rm = TRUE)) %>% rename(hog_pred = mean)

# merge df together
d <- merge(a, b, by = 'buffer_km') %>% distinct(buffer_km, .keep_all = TRUE)
pred_mean <- merge(c, d, by = 'buffer_km') %>% distinct(buffer_km, .keep_all = TRUE)


# pivot longer
pred_mean_long <- pred_mean %>% rename(Hedgehog = hog_pred,
                                       Fox = fox_pred,
                                       Badger = badger_pred) %>% 
  pivot_longer(cols=c('Hedgehog', 'Fox','Badger'),
                    names_to='Species',
                    values_to='Prediction') 

pred_long <- pred %>% dplyr::select(buffer_km, Badger_pre, fox_pred, hog_pred) %>% 
  rename(Hedgehog = hog_pred,
         Fox = fox_pred,
         Badger = Badger_pre) %>% 
  pivot_longer(cols=c('Hedgehog', 'Fox','Badger'),
                   names_to='Species',
                   values_to='Prediction')

# plot graph
p_mean <- ggplot(pred_mean_long, aes(x = buffer_km, y = Prediction, colour = Species)) +
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
p_mean

p_all <- ggplot(pred_long, aes(x = buffer_km, y = Prediction, group = Species)) +
  geom_smooth(method="auto", level=0.95, aes(linetype=Species), color="black") + 
  scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15)) +
  xlab("Distance from centre (m)") + ylab("Predicted habitat suitability score") +
  scale_y_continuous(limits = c(0, 0.8))

  
p_all

ggsave("../../../results/CitiSci/AllYear_XMammalPred/distance_pred_plot.pdf", width=10, height=5, units="in", scale = 1)

 


