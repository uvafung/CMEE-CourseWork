# Name: SDM_HOG_citisci_AllYear_map_graph_plotting_4.R
# Description: This script is used to: 
# 1) run SDM on CitiSci Hedgehogs All year using Uva Fung's dataset to determine which 
#    model is the best fitting
# 2) Predict habitat suitability across entire dataset and plot the outcome 
# 3) Determine which variable has the greatest effect in habitat suitability 
#    by looking at change in AUC
# Author: Uva Fung uf21@ic.ac.uk
# Date: July 6 2022

rm(list=ls())

### Install useful packages
install.packages(c('dplyr', 'tidyverse', 'raster', 
                   'rgdal', 'dismo', 'rJava', 'maptools', 'rgeos','biomod2'))

install.packages('ggplot2')
install.packages('MASS')
install.packages("PresenceAbsence")
install.packages("ecospat")
install.packages('ggeffects')
install.packages('stargazer')
install.packages('SDMtune')
install.packages('pROC')
install.packages('boot')
install.packages('viridis')
install.packages('scales')
install.packages('gplm')


require(dplyr)
require(raster)
require(rgdal)
require(biomod2)
require(dismo)
require(rJava)
require(maptools)
require(rgeos)
require(ggplot2)
require(tidyverse)
# require(usdm) # vif score
# require(corrplot) # corrplot
require(MASS) #stepAIC
# require(mgcv) #generalized additive models (gam)
# require(randomForest) # random forest model
require(PresenceAbsence) # calculate AUC scores
require(ecospat) # calculate TSS
require(ggeffects) # response curves
require(stargazer) # save eval table
library(stargazer)
require(SDMtune) # calculate TSS
require(pROC) # auc()
require(boot) # inverse logit
library(viridis) # viridis colour scheme for mapping
require(scales)
require(gplm)



# ##########################################
# ############## Import data ###############
# ##########################################
setwd("Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear")

enviro <- read.csv('../../../data/EnviroMammal_df_July4.csv') # Load environmental variable dataset for modelling
enviro <- enviro %>% subset(enviro$HOGns %in% c(0,1)) # Only select presence absence entries

############## Model fitting #######################

###### 3 model -- GLM with Quadratic polynomial and selected variables after stepAIC #####
glm_qua_selected <- glm(HOGns ~  
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + BadgCS100 + FoxCS100, 
               family = binomial(link="logit"), data = enviro)

summary(glm_qua_selected)


############################################
##### Generate maps with GLM Qua Selected ##
############################################

enviro1 <- read.csv('../../../data/EnviroMammal_df_July4.csv')

######### Calculate habitat suitability across entire area ############
# final_threshold <- as.numeric(tss_GLMmod['GLM_Qua_selected_maxThreshold', 'Mean']) # store final threshold value
final_threshold <- 0.5179000


# predict back to the full dataset using predict()
predictions <- as.numeric(predict(glm_qua_selected, enviro1, type = c("response"))) # suitability in % 

CI <- predict.lm(glm_qua_selected, newdata = enviro1, interval = "confidence") # confidence interval # predict.lm gives table of fit/lwr/upr

CIpred <- as.data.frame(CI)
colnames(CIpred) <- c('fit_link', 'lwr_link', 'upr_link')

ilink <- family(glm_qua_selected)$linkinv # inverse link for the model

CIpred$fit_resp<-ilink(CIpred$fit_link) # use ilink to inverse transform the CI values
CIpred$fit_lwr<-ilink(CIpred$lwr_link) # should range 0-1 now
CIpred$fit_upr<-ilink(CIpred$upr_link)

CIpred$CI_int <- CIpred$fit_upr - CIpred$fit_lwr # calculate confidence interval

CI_int_val <- as.numeric(CIpred$CI_int)


# binary conversion based on presence absence
enviro1 <- enviro1 %>% 
  mutate(predictions) %>% # create new column to store prediction score
  mutate(CI_int_val) %>%
  mutate(binary = case_when(predictions >= final_threshold ~ 1, 
                            predictions < final_threshold ~ 0)) # allocate binary presence/absence based on prediction score vs TSS threshold



# convert numbers to factors so presence/absence 
# can be plotted with discrete colour scale
enviro1$binary <- as.factor(enviro1$binary)  


# enviroplot <- enviro1 %>% dplyr::select(Easting, Northing, predictions, CI_int_val, binary)
# write.csv(enviroplot, "../../../results/CitiSci/AllYear/Hedgehog/enviroplot.csv")

### map -- ggplot of predicted habitat suitability across Greater London ###
ggplot() + geom_point(data = enviro1,  
                      aes(x = Easting, y = Northing, 
                          colour = predictions)) + 
  scale_colour_viridis(discrete = F, option="turbo", limits=c(0, 1)) + 
  coord_quickmap() +
  theme_light() + 
  labs(color = "Habitat Suitability")

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_pre_map.pdf", width=10, height=7.5, units="in", scale = 1)


### map -- ggplot of model Confidence Interval across Greater London ###
ggplot() + geom_point(data = enviro1, 
                      aes(x = Easting, y = Northing, 
                          colour = CI_int_val)) +
  scale_colour_viridis(discrete = F, option="turbo", limits=c(0, 1)) +
  coord_quickmap() +
  theme_light() + 
  labs(color = "Confidence Interval")

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_CI_map.pdf", width=10, height=7.5, units="in", scale = 1)



### map -- ggplot of binary presence absence across Greater London ###
ggplot() + geom_point(data = enviro1, 
                      aes(x = Easting, y = Northing, 
                          colour = binary)) +
  scale_color_manual(labels = c("Absent", "Present"), 
                     values = c("blue", "red")) +
  coord_quickmap() +
  theme_light() +
  labs(color = "")

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_bi_map.pdf", width=10, height=7.5, units="in", scale = 1)

################################################################################################
### test relative importance of variables by removing individual variables and assess AUC change
################################################################################################

############## CHECK GLM models performance via AUC ############
auc_change_GLMmod <- as.data.frame(matrix(0, ncol = 100, nrow = 13, # create dataframe to store AUC outputs
                                   dimnames = list(c('GDN100', 'ALT1000', 'AMN750', 'PLPK100', 
                                                     'WAT100', 'WD100', 'poly(IMP100)','DTCH100',
                                                     'SEMI500', 'TERR750', 'poly(logHD100)','BadgCS100',
                                                     'FoxCS100'), NULL)))


# GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
#   poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
#   poly(logHD100, 2) + BadgCS100 + FoxCS100


for (i in 1:100){  # i = 100 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$HOGns, ratio = 0.75) # 75/25 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  # 1 -- GLM without GDN100
  glm_X_GDN100 = glm(HOGns ~  
                       ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[1, i] <- auc(eval$HOGns, predict(glm_X_GDN100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 2 -- GLM without ALT1000
  glm_X_ALT1000 = glm(HOGns ~  
                        GDN100 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[2, i] <- auc(eval$HOGns, predict(glm_X_ALT1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 3 -- GLM without AMN750
  glm_X_AMN750 = glm(HOGns ~  
                       GDN100 + ALT1000 + PLPK100 + WAT100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[3, i] <- auc(eval$HOGns, predict(glm_X_AMN750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 4 -- GLM without PLPK100
  glm_X_PLPK100 = glm(HOGns ~  # GLM without PLPK100
                        GDN100 + ALT1000 + AMN750 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[4, i] <- auc(eval$HOGns, predict(glm_X_PLPK100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 5 -- GLM without WAT100
  glm_X_WAT100 = glm(HOGns ~  # GLM without WAT100
                       GDN100 + ALT1000 + AMN750 + PLPK100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[5, i] <- auc(eval$HOGns, predict(glm_X_WAT100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 6 -- GLM without WD100
  glm_X_WD100 = glm(HOGns ~  
                      GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + 
                      poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                      poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[6, i] <- auc(eval$HOGns, predict(glm_X_WD100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 7 -- GLM without poly(IMP100, 2)
  glm_X_polyIMP100 = glm(HOGns ~  
                           GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          DTCH100 + SEMI500 + TERR750 + 
                           poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[7, i] <- auc(eval$HOGns, predict(glm_X_polyIMP100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 8 -- GLM without DTCH100 
  glm_X_SEMI500 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[8, i] <- auc(eval$HOGns, predict(glm_X_SEMI500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 9 -- GLM without SEMI500 
  glm_X_SEMI500 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[9, i] <- auc(eval$HOGns, predict(glm_X_SEMI500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 10 -- GLM without TERR750
  glm_X_TERR750 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[10, i] <- auc(eval$HOGns, predict(glm_X_TERR750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  
  # 11 -- GLM without poly(logHD100, 2) +
  glm_X_polylogHD100 = glm(HOGns ~  
                             GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                             poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                            BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[11, i] <- auc(eval$HOGns, predict(glm_X_polylogHD100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 12 -- GLM without BadgCS100
  glm_X_BadgCS100 = glm(HOGns ~  # GLM without BadgCS100
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[12, i] <- auc(eval$HOGns, predict(glm_X_BadgCS100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 13 -- GLM without FoxCS100
  glm_X_BadgCS100 = glm(HOGns ~  # GLM without BadgCS100
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + BadgCS100, 
                        family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[13, i] <- auc(eval$HOGns, predict(glm_X_BadgCS100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
}

# best_mod_AUC <- as.numeric(auc_GLMmod['GLM_Qua_selected_AUC', 'Mean']) # store final threshold value
best_mod_AUC <- 0.8826558

auc_change_GLMmod <- auc_change_GLMmod %>% 
  mutate(Mean = rowMeans(auc_change_GLMmod))  # create new column to store the mean AUC

auc_change_GLMmod <- auc_change_GLMmod %>% 
  mutate(AUC_Difference = as.numeric(auc_change_GLMmod[, 'Mean']) - best_mod_AUC) # calculate difference between best model AUC and AUC with variables removed and store output in new column

auc_change_GLMmod$Predictor <- row.names(auc_change_GLMmod) 

auc_change_GLMmod <- auc_change_GLMmod %>% relocate(Predictor)


auc_change_plot <- subset(auc_change_GLMmod, select = -c(Mean, AUC_Difference) ) # select all columns except the mean and AUC Difference column

# convert df into long format
auc_change_plot <- auc_change_plot %>% pivot_longer(
  cols = starts_with("V"),
  names_to='Repeats', values_to='New_AUC')

# plot boxplot
ggplot(auc_change_plot, aes(x=New_AUC, y=as.factor(Predictor))) + 
  geom_boxplot() +
  xlab("AUC Score") + ylab("Predictor") +
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    size = 2, linetype = "solid"),
    axis.line = element_line(colour = "grey")) +
  geom_vline(xintercept = as.numeric(best_mod_AUC), color = "red")

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_AUCchange_map.pdf", width=10, height=7.5, units="in", scale = 1)


auc_change_summary <- auc_change_GLMmod[,c("Predictor", "Mean", "AUC_Difference")]
write.csv(auc_change_summary, '../../../results/CitiSci/AllYear/Hedgehog/hog_AUCchange_summary.csv')



################################################################################################
### test relative importance of variables by removing individual variables and assess TSS change
################################################################################################
tss_change_GLMmod <- as.data.frame(matrix(0, ncol = 100, nrow = 13, # create dataframe to store TSS outputs
                                          dimnames = list(c('GDN100', 'ALT1000', 'AMN750', 'PLPK100', 
                                                            'WAT100', 'WD100', 'poly(IMP100)','DTCH100',
                                                            'SEMI500', 'TERR750', 'poly(logHD100)','BadgCS100',
                                                            'FoxCS100'), NULL)))

for (i in 1:100){  # i = 100 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$HOGns, ratio = 0.75) # 75/25 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  # 1 -- GLM without GDN100
  glm_X_GDN100 = glm(HOGns ~  
                       ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[1, i] <- auc(eval$HOGns, predict(glm_X_GDN100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 2 -- GLM without ALT1000
  glm_X_ALT1000 = glm(HOGns ~  
                        GDN100 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[2, i] <- auc(eval$HOGns, predict(glm_X_ALT1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 3 -- GLM without AMN750
  glm_X_AMN750 = glm(HOGns ~  
                       GDN100 + ALT1000 + PLPK100 + WAT100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[3, i] <- auc(eval$HOGns, predict(glm_X_AMN750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 4 -- GLM without PLPK100
  glm_X_PLPK100 = glm(HOGns ~  # GLM without PLPK100
                        GDN100 + ALT1000 + AMN750 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[4, i] <- auc(eval$HOGns, predict(glm_X_PLPK100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 5 -- GLM without WAT100
  glm_X_WAT100 = glm(HOGns ~  # GLM without WAT100
                       GDN100 + ALT1000 + AMN750 + PLPK100 + WD100 + 
                       poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                       poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[5, i] <- auc(eval$HOGns, predict(glm_X_WAT100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 6 -- GLM without WD100
  glm_X_WD100 = glm(HOGns ~  
                      GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + 
                      poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                      poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[6, i] <- auc(eval$HOGns, predict(glm_X_WD100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 7 -- GLM without poly(IMP100, 2)
  glm_X_polyIMP100 = glm(HOGns ~  
                           GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                           DTCH100 + SEMI500 + TERR750 + 
                           poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                         family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[7, i] <- auc(eval$HOGns, predict(glm_X_polyIMP100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 8 -- GLM without DTCH100 
  glm_X_SEMI500 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + SEMI500 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[8, i] <- auc(eval$HOGns, predict(glm_X_SEMI500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 9 -- GLM without SEMI500 
  glm_X_SEMI500 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + TERR750 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[9, i] <- auc(eval$HOGns, predict(glm_X_SEMI500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 10 -- GLM without TERR750
  glm_X_TERR750 = glm(HOGns ~  
                        GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                        poly(IMP100, 2) + DTCH100 + SEMI500 + 
                        poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[10, i] <- auc(eval$HOGns, predict(glm_X_TERR750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  
  # 11 -- GLM without poly(logHD100, 2) +
  glm_X_polylogHD100 = glm(HOGns ~  
                             GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                             poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                             BadgCS100 + FoxCS100, 
                           family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[11, i] <- auc(eval$HOGns, predict(glm_X_polylogHD100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 12 -- GLM without BadgCS100
  glm_X_BadgCS100 = glm(HOGns ~  # GLM without BadgCS100
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + FoxCS100, 
                        family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[12, i] <- auc(eval$HOGns, predict(glm_X_BadgCS100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 13 -- GLM without FoxCS100
  glm_X_BadgCS100 = glm(HOGns ~  # GLM without BadgCS100
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + BadgCS100, 
                        family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[13, i] <- auc(eval$HOGns, predict(glm_X_BadgCS100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  GLM_tss <- ecospat.max.tss(predict(glm.fit_GLM, eval, type='response'), eval$HOGns)
  
  
  
}

# best_mod_tss <- as.numeric(tss_GLMmod['GLM_Qua_selected_tss', 'Mean']) # store final threshold value
best_mod_tss <- 0.6212851

tss_change_GLMmod <- tss_change_GLMmod %>% 
  mutate(Mean = rowMeans(tss_change_GLMmod))  # create new column to store the mean tss

tss_change_GLMmod <- tss_change_GLMmod %>% 
  mutate(tss_Difference = as.numeric(tss_change_GLMmod[, 'Mean']) - best_mod_tss) # calculate difference between best model tss and tss with variables removed and store output in new column

tss_change_GLMmod$Predictor <- row.names(tss_change_GLMmod) 

tss_change_GLMmod <- tss_change_GLMmod %>% relocate(Predictor)


tss_change_plot <- subset(tss_change_GLMmod, select = -c(Mean, tss_Difference) ) # select all columns except the mean and tss Difference column

# convert df into long format
tss_change_plot <- tss_change_plot %>% pivot_longer(
  cols = starts_with("V"),
  names_to='Repeats', values_to='New_tss')

# plot boxplot
ggplot(tss_change_plot, aes(x=New_tss, y=as.factor(Predictor))) + 
  geom_boxplot() +
  xlab("tss Score") + ylab("Predictor") +
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    size = 2, linetype = "solid"),
    axis.line = element_line(colour = "grey")) +
  geom_vline(xintercept = as.numeric(best_mod_tss), color = "red")

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_tsschange_map.pdf", width=10, height=7.5, units="in", scale = 1)


tss_change_summary <- tss_change_GLMmod[,c("Predictor", "Mean", "tss_Difference")]
write.csv(tss_change_summary, '../../../results/CitiSci/AllYear/Hedgehog/hog_tsschange_summary.csv')




##############################################################
#### Visualize response curves of GLM Qua using ggeffects ####
##############################################################
### Significant variables only

enviro$ALT1000 <- enviro$ALT1000 * 100
enviro$AMN750 <- enviro$AMN750 * 100
enviro$BadgCS100 <- enviro$BadgCS100 * 100
enviro$FoxCS100 <- enviro$FoxCS100 * 100
enviro$GDN100 <- enviro$GDN100 * 100 # x100 to convert into % for variables with 0-1 range
enviro$IMP100 <- enviro$IMP100 * 100
enviro$PLPK100 <- enviro$PLPK100 * 100
enviro$WAT100 <- enviro$WAT100 * 100
enviro$WD100 <- enviro$WD100 * 100

glm_qua_selected <- glm(HOGns ~  
                          GDN100 + ALT1000 + AMN750 + PLPK100 + WAT100 + WD100 + 
                          poly(IMP100, 2) + DTCH100 + SEMI500 + TERR750 + 
                          poly(logHD100, 2) + BadgCS100 + FoxCS100, 
                        family = binomial(link="logit"), data = enviro)

summary(glm_qua_selected)




glm_qua_selected_step <- stepAIC(glm_qua_selected, direction = 'both', trace = FALSE) 
glm_qua_selected_step 

# GDN100
gge_GDN100  <- ggpredict(glm_qua_selected_step, 
                         terms = c('GDN100 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_GDN100$Predictor <- "Garden cover (%)" 

# ggplot(gge_GDN100 , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "GDN100" ) 


# ALT1000
gge_ALT1000 <- ggpredict(glm_qua_selected_step, 
                         terms = c('ALT1000 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_ALT1000$Predictor <- "Allotment cover (%)" 



# ggplot(gge_ALT1000, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "ALT1000")


# AMN750
gge_AMN750 <- ggpredict(glm_qua_selected_step, 
                        terms = c('AMN750 [all]'), ci.lvl = 0.95, 
                        typical = 'mean', back.transform = T)
gge_AMN750$Predictor <- "Amenity cover (%)" 

# ggplot(gge_AMN750, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "AMN750")               


# # CEM1000
# gge_CEM1000 <- ggpredict(glm_qua_selected_step, 
#                          terms = c('CEM1000 [all]'), ci.lvl = 0.95, 
#                          typical = 'mean', back.transform = T)
# gge_CEM1000$Predictor <- "CEM1000" 
# 
# ggplot(gge_CEM1000, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "CEM1000")  


# PLPK100
gge_PLPK100 <- ggpredict(glm_qua_selected_step, 
                         terms = c('PLPK100 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_PLPK100$Predictor <- "Park and play spaces (%)" 

# ggplot(gge_PLPK100, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "PLPK100")  


# # SPRT500 
# gge_SPRT500 <- ggpredict(glm_qua_selected_step, 
#                          terms = c('SPRT500 [all]'), ci.lvl = 0.95, 
#                          typical = 'mean', back.transform = T)
# gge_SPRT500$Predictor <- "SPRT500" 
# 
# ggplot(gge_SPRT500, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "SPRT500") 



# WAT100 
gge_WAT100 <- ggpredict(glm_qua_selected_step, 
                        terms = c('WAT100 [all]'), ci.lvl = 0.95, 
                        typical = 'mean', back.transform = T)
gge_WAT100$Predictor <- "Water cover (%)" 

# ggplot(gge_WAT100, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "WAT100") 


# WD100 
gge_WD100 <- ggpredict(glm_qua_selected_step, 
                       terms = c('WD100 [all]'), ci.lvl = 0.95, 
                       typical = 'mean', back.transform = T)
gge_WD100$Predictor <- "Woodland cover (%)" 

# ggplot(gge_WD100, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "WD100") 


# # TRAF100 
# gge_TRAF100 <- ggpredict(glm_qua_selected_step, 
#                        terms = c('TRAF100 [all]'), ci.lvl = 0.95, 
#                        typical = 'mean', back.transform = T)
# gge_TRAF100$Predictor <- "TRAF100" 
# 
# ggplot(gge_TRAF100, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "TRAF100")


# IMP100
gge_IMP100  <- ggpredict(glm_qua_selected_step, 
                         terms = c('IMP100  [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_IMP100$Predictor <- "Impervious cover (%)" 

# ggplot(gge_IMP100 , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "IMP100 ")


# # DTCH100
# gge_DTCH100 <- ggpredict(glm_qua_selected_step,
#                          terms = c('DTCH100 [all]'), ci.lvl = 0.95,
#                          typical = 'mean', back.transform = T)
# gge_DTCH100$Predictor <- "DTCH100" 
# 
# ggplot(gge_DTCH100  , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "DTCH100 ")


# # SEMI500  
# gge_SEMI500   <- ggpredict(glm_qua_selected_step, 
#                            terms = c('SEMI500   [all]'), ci.lvl = 0.95, 
#                            typical = 'mean', back.transform = T)
# gge_SEMI500$Predictor <- "Semi-detached housing (%)" 

# ggplot(gge_SEMI500  , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "SEMI500 ")


# TERR750
gge_TERR750 <- ggpredict(glm_qua_selected_step,
                         terms = c('TERR750   [all]'), ci.lvl = 0.95,
                         typical = 'mean', back.transform = T)
gge_TERR750$Predictor <- "Terraced housing (%)" 




# logHD100
gge_logHD100 <- ggpredict(glm_qua_selected_step, 
                          terms = c('logHD100 [all]'), ci.lvl = 0.95, 
                          typical = 'mean', back.transform = T)
gge_logHD100$Predictor <- "log human density (km2)" 

# ggplot(gge_logHD100, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "logHD100")

# # HD250
# gge_HD250 <- ggpredict(glm_qua_selected_step, 
#                           terms = c('HD250 [all]'), ci.lvl = 0.95, 
#                           typical = 'mean', back.transform = T)
# gge_HD250$Predictor <- "HD250" 
# 
# ggplot(gge_HD250, aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "HD250")


# BadgCS100 
gge_BadgCS100  <- ggpredict(glm_qua_selected_step, 
                            terms = c('BadgCS100 [all]'), ci.lvl = 0.95, 
                            typical = 'mean', back.transform = T)
gge_BadgCS100$Predictor <- "Badger presence (%)" 

# ggplot(gge_BadgCS100 , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "BadgCS100" )


# FoxCS100 
gge_FoxCS100  <- ggpredict(glm_qua_selected_step, 
                           terms = c('FoxCS100 [all]'), ci.lvl = 0.95, 
                           typical = 'mean', back.transform = T)
gge_FoxCS100$Predictor <- "Fox presence (%)" 

# ggplot(gge_FoxCS100 , aes(x, predicted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
#   labs(y= "Predicted HOGns", x = "FoxCS100" )



# combine all gge dfs into one big df for plotting with facet wrap
gge_all <- rbind(gge_ALT1000, gge_AMN750, gge_BadgCS100, gge_FoxCS100, gge_GDN100,
                 gge_IMP100, gge_logHD100, gge_PLPK100, gge_TERR750,
                 gge_WAT100, gge_WD100)


ggplot(gge_all , aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(y= "Probability of hedgehog presence", x = "Predictor" ) +
  facet_wrap(~Predictor, scales = "free",
             strip.position="bottom") +
  theme_bw()

ggsave("../../../results/CitiSci/AllYear/Hedgehog/hog_predictor_ggplot.pdf", width=10, height=7.5, units="in", scale = 1)

