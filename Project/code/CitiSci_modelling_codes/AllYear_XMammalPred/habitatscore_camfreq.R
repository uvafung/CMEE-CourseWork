q# Name: habitatscore_camfreq.R
# Description: This script is used to: 
# 1) Plot habitat suitability score vs camera trapping rate

# Author: Uva Fung uf21@ic.ac.uk
# Date: Aug 18 2022

rm(list=ls())

install.packages('gridExtra')
install.packages('cowplot')

require(dplyr)
require(raster)
require(biomod2)
require(dismo)
require(ggplot2)
require(tidyverse)
require(ggeffects) # response curves
require(stargazer) # save eval table
library(stargazer)
require(scales)
require(gridExtra)
require(cowplot)



# ##########################################
# ############## Import data ###############
# ##########################################
setwd("Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear_XMammalPred")

camcells <- read.csv('../../../data/QGIS_csv/Cam_trap_gridcells_Aug18.csv') # Load environmental variable dataset for modelling

hog_score <- read.csv('../../../results/CitiSci/AllYear_XMammalPred/Hedgehog/hog_enviroplot.csv') # Load environmental variable dataset for modelling
fox_score <- read.csv('../../../results/CitiSci/AllYear_XMammalPred/Fox/fox_enviroplot.csv')
bdg_score <- read.csv('../../../results/CitiSci/AllYear_XMammalPred/Badger/badger_enviroplot.csv')

camrate <- read.csv('../../../data/HW_1721_TrappingRates.csv')

hog_score2 <- hog_score %>% mutate(hog_pre = predictions) %>% # select useful columns
  dplyr::select(Easting, Northing, left, top, right, bottom, hog_pre)

fox_score2 <- fox_score %>% mutate(fox_pre = predictions) %>% 
  dplyr::select(Easting, Northing, left, top, right, bottom, fox_pre)

bdg_score2 <- bdg_score %>% mutate(bdg_pre = predictions) %>% 
  dplyr::select(Easting, Northing, left, top, right, bottom, bdg_pre)


merge_bdg_fox <- merge(bdg_score2, fox_score2, by = c('Easting', 'Northing', 'left', 'top', 'right', 'bottom')) %>% 
  distinct(, .keep_all = TRUE)

merge_all <- dplyr::left_join(merge_bdg_fox,hog_score2)

write.csv(merge_all, '../../../data/PreScore_byGridCell_perspp_Aug18.csv')


### add a column for grid cell id
id_badger <- read.csv("../../../results/CitiSci/AllYear_XMammalPred/Badger/id_badger_enviroplot.csv")
merge_all <- read.csv('../../../data/PreScore_byGridCell_perspp_Aug18.csv')

id <- id_badger %>% dplyr::select(id, left, top, right, bottom)

merge_id <- dplyr::left_join(merge_all,id) #merged with id


## Merge camera trap freq and prediction scores together by grid cell id
camtrap_freq <- read.csv('../../../data/QGIS_csv/camtrap_pergridcell_Aug18.csv')


camtrap_freq2 <- camtrap_freq %>% dplyr::select(id, Freq_Hedgehog, Freq_Fox, Freq_Badger)

camfreq_pred <- dplyr::left_join(merge_id,camtrap_freq2)
write.csv(camfreq_pred, '../../../data/camfreq_pred.csv') # save a copy first

camfreq_pred_sites <- camfreq_pred %>% drop_na(Freq_Hedgehog, Freq_Fox, Freq_Badger) # remove NAs that don't have camera traps



write.csv(camfreq_pred_sites, '../../../data/camfreq_prediction_plotting_Aug18.csv') # save another copy without NAs

camfreq_pred_sites <- read.csv('../../../data/camfreq_prediction_plotting_Aug18.csv') # reload it for log transformation
camfreq_pred_sites <- camfreq_pred_sites %>% mutate(log_hog_freq = log(Freq_Hedgehog),
                                                    log_fox_freq = log(Freq_Fox),
                                                    log_bdg_freq = log(Freq_Badger))


camfreq_pred_sites <- camfreq_pred_sites %>% mutate(log_hog_freq_NA = na_if(log_hog_freq, -Inf),
                                                    log_fox_freq_NA = na_if(log_fox_freq, -Inf),
                                                    log_bdg_freq_NA = na_if(log_bdg_freq, -Inf))




########## GLM models ##########

### No log transformation
hog <- glm(hog_pre ~ Freq_Hedgehog, data = camfreq_pred_sites)
summary(hog)

fox <- glm(fox_pre ~ Freq_Fox, data = camfreq_pred_sites)
summary(fox)

bdg <- glm(fox_pre ~ Freq_Fox, data = camfreq_pred_sites)
summary(bdg)

########## plot habitat suitability score vs camera trapping rate 

hog_plot <- ggplot(camfreq_pred_sites, aes(x = hog_pre, y = Freq_Hedgehog)) + # hedgehog
  geom_point(size = 1) + 
  geom_smooth(method="auto", level=0.95,color="red") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15),
                     plot.margin = margin(2,0.5,1,1, "cm")) +
  scale_x_continuous(limits = c(0.05, 0.85)) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")

fox_plot <- ggplot(camfreq_pred_sites, aes(x = fox_pre, y = Freq_Fox)) +
  geom_point(size = 1) + 
  geom_smooth(method="auto", level=0.95,color="red") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15),
                     plot.margin = margin(2,0.5,1,0.5, "cm")) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")

bdg_plot <- ggplot(camfreq_pred_sites, aes(x = bdg_pre, y = Freq_Badger)) +
  geom_point(size = 1) + 
  geom_smooth(method="auto", level=0.95,color="red") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15),
                     plot.margin = margin(2,1,1,0.5, "cm")) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")


regress_plot <- gridExtra::grid.arrange(hog_plot, fox_plot, bdg_plot, nrow=1)
regress_plot
ggsave("../../../results/CitiSci/AllYear_XMammalPred/habitat_score_cam_rate.pdf", 
       width=5, height=7.5, units="in")


plot_grid(hog_plot, fox_plot, bdg_plot, align = "hv", axis = "tblr", ncol=1)

### YES log transformation
log_hog <- glm(hog_pre ~ log_hog_freq_NA, data = camfreq_pred_sites)
summary(log_hog)

log_fox <- glm(fox_pre ~ log_fox_freq_NA, data = camfreq_pred_sites)
summary(log_fox)

log_bdg <- glm(fox_pre ~ log_bdg_freq_NA, data = camfreq_pred_sites)
summary(log_bdg)

########## plot habitat suitability score vs camera trapping rate 

log_hog_plot <- ggplot(camfreq_pred_sites, aes(x = hog_pre, y = log_hog_freq_NA)) + # hedgehog
  geom_point() + 
  geom_smooth(method="auto", level=0.95,color="black") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15)) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")

log_fox_plot <- ggplot(camfreq_pred_sites, aes(x = fox_pre, y = log_fox_freq_NA)) +
  geom_point() + 
  geom_smooth(method="auto", level=0.95,color="black") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15)) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")

log_bdg_plot <- ggplot(camfreq_pred_sites, aes(x = bdg_pre, y = log_bdg_freq_NA)) +
  geom_point() + 
  geom_smooth(method="auto", level=0.95,color="black") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size=15)) +
  xlab("Predicted habitat suitability score") + ylab("Camera trapping rate")


log_regress_plot <- gridExtra::grid.arrange(log_hog_plot, log_fox_plot, log_bdg_plot)
log_regress_plot





