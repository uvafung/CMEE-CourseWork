# Name: SDM_FOX_citisci_AllYear_map_graph_plotting_4.R
# Description: This script is used to: 
# 1) Print prediction/CI/binary presence absence map for CitiSci AllYear Foxes
# 2) Determine which variable has the greatest effect in habitat suitability 
#    by looking at change in AUC
# 3) Plot how FOXns changes with different predictor variables
# Author: Uva Fung uf21@ic.ac.uk
# Date: July 10 2022

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



# ##########################################
# ############## Import data ###############
# ##########################################
setwd("Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear")

enviro <- read.csv('../../../data/EnviroMammal_df_July4.csv') # Load environmental variable dataset for modelling
enviro <- enviro %>% subset(enviro$FOXns %in% c(0,1)) # Only select presence absence entries

############## Model fitting #######################

###### 3 model -- GLM with NO quadratic polynomial and YES selected variables after stepAIC #####
glm_qua_selected <- glm(FOXns ~  
                          GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                          WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                          logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                        family = binomial(link="logit"), data = enviro)

summary(glm_qua_selected)

# perform stepAIC again to confirm all variables used are necessary
glm_qua_selected_step <- stepAIC(glm_qua_selected, direction = 'both', trace = FALSE) 
glm_qua_selected_step 



############################################
##### Generate maps with GLM Qua Selected ##
############################################

enviro1 <- read.csv('../../../data/EnviroMammal_df_July4.csv')

######### Calculate habitat suitability across entire area ############
# final_threshold <- as.numeric(tss_GLMmod['GLM_Qua_selected_maxThreshold', 'Mean']) # store final threshold value
final_threshold <- 0.4413000 # calculated in step 2 script

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

# enviroplot <- enviro1 %>% dplyr::select(Easting, Northing, predictions, CI_int_val, binary)
# write.csv(enviroplot, "../../../results/CitiSci/AllYear/Fox/enviroplot.csv")




# convert numbers to factors so presence/absence 
# can be plotted with discrete colour scale
enviro1$binary <- as.factor(enviro1$binary)  


### map -- ggplot of predicted habitat suitability across Greater London ###
ggplot() + geom_point(data = enviro1,  
                      aes(x = Easting, y = Northing, 
                          colour = predictions)) + 
  scale_colour_viridis(discrete = F, option="turbo", limits=c(0, 1)) + 
  coord_quickmap() +
  theme_light() + 
  labs(color = "Habitat Suitability")

ggsave("../../../results/CitiSci/AllYear/Fox/fox_pre_map.pdf", width=10, height=7.5, units="in", scale = 1)


### map -- ggplot of model Confidence Interval across Greater London ###
ggplot() + geom_point(data = enviro1, 
                      aes(x = Easting, y = Northing, 
                          colour = CI_int_val)) +
  scale_colour_viridis(discrete = F, option="turbo", limits=c(0, 1)) +
  coord_quickmap() +
  theme_light() + 
  labs(color = "Confidence Interval")

ggsave("../../../results/CitiSci/AllYear/Fox/fox_CI_map.pdf", width=10, height=7.5, units="in", scale = 1)



### map -- ggplot of binary presence absence across Greater London ###
ggplot() + geom_point(data = enviro1, 
                      aes(x = Easting, y = Northing, 
                          colour = binary)) +
  scale_color_manual(labels = c("Absent", "Present"), 
                     values = c("blue", "red")) +
  coord_quickmap() +
  theme_light() +
  labs(color = "")

ggsave("../../../results/CitiSci/AllYear/Fox/fox_bi_map.pdf", width=10, height=7.5, units="in", scale = 1)

################################################################################################
### test relative importance of variables by removing individual variables and assess AUC change
################################################################################################



############## CHECK GLM models performance via AUC ############
auc_change_GLMmod <- as.data.frame(matrix(0, ncol = 100, nrow = 13, # create dataframe to store AUC outputs
                                   dimnames = list(c('GDN1000', 'ALT750','PLPK100', 'WAT750',
                                                     'WD1000', 'IMP750', 'DTCH1000', 'SEMI500', 'TERR1000', 
                                                     'logHD500', 'logTRAF1000', 'BadgCS750', 'HogCS100'), NULL)))


for (i in 1:100){  # i = 100 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.75) # 75/25 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  # 1 -- GLM without GDN1000
  glm_X_GDN1000 = glm(FOXns ~
                        ALT750 + PLPK100 +  WAT750 + 
                        WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                        logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[1, i] <- auc(eval$FOXns, predict(glm_X_GDN1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 2 -- GLM without ALT750
  glm_X_ALT750 = glm(FOXns ~  
                       GDN1000 + PLPK100 +  WAT750 + 
                       WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                       logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[2, i] <- auc(eval$FOXns, predict(glm_X_ALT750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # # 3 -- GLM without CEM500
  # glm_X_CEM500 = glm(FOXns ~  
  #                      GDN1000 + ALT750 + PLPK100 +  WAT750 + 
  #                      WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
  #                      logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
  #                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  # auc_change_GLMmod[3, i] <- auc(eval$FOXns, predict(glm_X_CEM500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  # 
  
  # 3 -- GLM without PLPK100
  glm_X_PLPK100 = glm(FOXns ~  # GLM without PLPK100
                        GDN1000 + ALT750 +  WAT750 + 
                        WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                        logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[3, i] <- auc(eval$FOXns, predict(glm_X_PLPK100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 4 -- GLM without WAT750
  glm_X_WAT750 = glm(FOXns ~  # GLM without WAT750
                       GDN1000 + ALT750 + PLPK100 +
                       WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                       logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[4, i] <- auc(eval$FOXns, predict(glm_X_WAT750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 5 -- GLM without WD1000
  glm_X_WD1000 = glm(FOXns ~  
                       GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                       IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                       logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[5, i] <- auc(eval$FOXns, predict(glm_X_WD1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 6 -- GLM without IMP750
  glm_X_IMP750 = glm(FOXns ~  
                       GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                       WD1000 + DTCH1000 + SEMI500 + TERR1000 + 
                       logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[6, i] <- auc(eval$FOXns, predict(glm_X_IMP750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 7 -- GLM without DTCH1000 
  glm_X_DTCH1000 = glm(FOXns ~  
                         GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                         WD1000 + IMP750 + SEMI500 + TERR1000 + 
                         logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[7, i] <- auc(eval$FOXns, predict(glm_X_DTCH1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 8 -- GLM without SEMI500 
  glm_X_SEMI500 = glm(FOXns ~  
                        GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                        WD1000 + IMP750 + DTCH1000 + TERR1000 + 
                        logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                      family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[8, i] <- auc(eval$FOXns, predict(glm_X_SEMI500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 9 -- GLM without TERR1000
  glm_X_TERR1000 = glm(FOXns ~  
                         GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                         WD1000 + IMP750 + DTCH1000 + SEMI500 +
                         logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[9, i] <- auc(eval$FOXns, predict(glm_X_TERR1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  
  # 10 -- GLM without logHD500
  glm_X_logHD500 = glm(FOXns ~  
                         GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                         WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                         logTRAF1000 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[10, i] <- auc(eval$FOXns, predict(glm_X_logHD500, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 11 -- GLM without logTRAF1000
  glm_X_logTRAF1000 = glm(FOXns ~  # GLM without logTRAF1000
                            GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                            WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                            logHD500 + BadgCS750 + HogCS100, 
                     family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[11, i] <- auc(eval$FOXns, predict(glm_X_logTRAF1000, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  # 12 -- GLM without BadgCS750
  glm_X_BadgCS750 = glm(FOXns ~  # GLM without BadgCS750
                          GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                          WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                          logHD500 + logTRAF1000 + HogCS100, 
                        family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[12, i] <- auc(eval$FOXns, predict(glm_X_BadgCS750, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  # 13 -- GLM without HogCS100
  glm_X_HogCS100 = glm(FOXns ~  # GLM without BadgCS250
                         GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                         WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                         logHD500 + logTRAF1000 + BadgCS750, 
                        family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_change_GLMmod[13, i] <- auc(eval$FOXns, predict(glm_X_HogCS100, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
}

# best_mod_AUC <- as.numeric(auc_GLMmod['GLM_Qua_selected_AUC', 'Mean']) # store final threshold value
best_mod_AUC <- 0.7839766 # calculated in step 2 script


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

ggsave("../../../results/CitiSci/AllYear/Fox/fox_AUCchange_map.pdf", width=10, height=7.5, units="in", scale = 1)


auc_change_summary <- auc_change_GLMmod[,c("Predictor", "Mean", "AUC_Difference")]
write.csv(auc_change_summary, '../../../results/CitiSci/AllYear/Fox/fox_AUCchange_summary.csv')

##############################################################
#### Visualize response curves of GLM Qua using ggeffects ####
##############################################################
enviro$ALT750 <- enviro$ALT750 * 100
enviro$BadgCS750 <- enviro$BadgCS750 * 100
enviro$FoxCS100 <- enviro$FoxCS100 * 100
enviro$GDN1000 <- enviro$GDN1000 * 100 # x100 to convert into % for variables with 0-1 range
enviro$IMP750 <- enviro$IMP750 * 100
enviro$PLPK100 <- enviro$PLPK100 * 100
enviro$WAT750 <- enviro$WAT750 * 100
enviro$WD1000 <- enviro$WD1000 * 100
enviro$HogCS100 <- enviro$HogCS100 * 100


glm_qua_selected <- glm(FOXns ~  
                          GDN1000 + ALT750 + PLPK100 +  WAT750 + 
                          WD1000 + IMP750 + DTCH1000 + SEMI500 + TERR1000 + 
                          logHD500 + logTRAF1000 + BadgCS750 + HogCS100, 
                        family = binomial(link="logit"), data = enviro)

summary(glm_qua_selected)

# perform stepAIC again to confirm all variables used are necessary
glm_qua_selected_step <- stepAIC(glm_qua_selected, direction = 'both', trace = FALSE) 
glm_qua_selected_step 


# GDN1000
gge_GDN1000  <- ggpredict(glm_qua_selected_step, 
                         terms = c('GDN1000 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_GDN1000$Predictor <- "Garden cover (%)" 



# ALT750
gge_ALT750 <- ggpredict(glm_qua_selected_step, 
                         terms = c('ALT750 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_ALT750$Predictor <- "Allotment cover (%)" 


# # AMN750
# gge_AMN750 <- ggpredict(glm_qua_selected_step, 
#                         terms = c('AMN750 [all]'), ci.lvl = 0.95, 
#                         typical = 'mean', back.transform = T)
# gge_AMN750$Predictor <- "Amenity cover (%)" 

        


# # CEM1000
# gge_CEM1000 <- ggpredict(glm_qua_selected_step, 
#                          terms = c('CEM1000 [all]'), ci.lvl = 0.95, 
#                          typical = 'mean', back.transform = T)
# gge_CEM1000$Predictor <- "CEM1000" 



# PLPK100
gge_PLPK100 <- ggpredict(glm_qua_selected_step, 
                         terms = c('PLPK100 [all]'), ci.lvl = 0.95, 
                         typical = 'mean', back.transform = T)
gge_PLPK100$Predictor <- "Park and play spaces (%)" 




# # SPRT500 
# gge_SPRT500 <- ggpredict(glm_qua_selected_step, 
#                          terms = c('SPRT500 [all]'), ci.lvl = 0.95, 
#                          typical = 'mean', back.transform = T)
# gge_SPRT500$Predictor <- "SPRT500" 
# 



# WAT750 
gge_WAT750 <- ggpredict(glm_qua_selected_step, 
                        terms = c('WAT750 [all]'), ci.lvl = 0.95, 
                        typical = 'mean', back.transform = T)
gge_WAT750$Predictor <- "Water cover (%)" 




# WD1000 
gge_WD1000 <- ggpredict(glm_qua_selected_step, 
                       terms = c('WD1000 [all]'), ci.lvl = 0.95, 
                       typical = 'mean', back.transform = T)
gge_WD1000$Predictor <- "Woodland cover (%)" 



# # IMP750
gge_IMP750  <- ggpredict(glm_qua_selected_step,
                         terms = c('IMP750  [all]'), ci.lvl = 0.95,
                         typical = 'mean', back.transform = T)
gge_IMP750$Predictor <- "Impervious cover (%)"



# # DTCH1000
# gge_DTCH1000 <- ggpredict(glm_qua_selected_step,
#                          terms = c('DTCH1000 [all]'), ci.lvl = 0.95,
#                          typical = 'mean', back.transform = T)
# gge_DTCH1000$Predictor <- "Detached housing (%)"
# 



# # SEMI500  
# gge_SEMI500   <- ggpredict(glm_qua_selected_step, 
#                            terms = c('SEMI500   [all]'), ci.lvl = 0.95, 
#                            typical = 'mean', back.transform = T)
# gge_SEMI500$Predictor <- "Semi-detached housing (%)" 
# 
# 


# TERR1000
gge_TERR1000 <- ggpredict(glm_qua_selected_step,
                         terms = c('TERR1000   [all]'), ci.lvl = 0.95,
                         typical = 'mean', back.transform = T)
gge_TERR1000$Predictor <- "Terraced housing (%)" 




# logHD500
gge_logHD500 <- ggpredict(glm_qua_selected_step, 
                          terms = c('logHD500 [all]'), ci.lvl = 0.95, 
                          typical = 'mean', back.transform = T)
gge_logHD500$Predictor <- "log human density (km2)" 


# # logTRAF1000
# gge_logTRAF1000 <- ggpredict(glm_qua_selected_step,
#                        terms = c('logTRAF1000 [all]'), ci.lvl = 0.95,
#                        typical = 'mean', back.transform = T)
# gge_logTRAF1000$Predictor <- "log traffic volume"
# 


# BadgCS750 
gge_BadgCS750  <- ggpredict(glm_qua_selected_step, 
                            terms = c('BadgCS750 [all]'), ci.lvl = 0.95, 
                            typical = 'mean', back.transform = T)
gge_BadgCS750$Predictor <- "Badger presence (%)" 




# HogCS100 
gge_HogCS100  <- ggpredict(glm_qua_selected_step, 
                           terms = c('HogCS100 [all]'), ci.lvl = 0.95, 
                           typical = 'mean', back.transform = T)
gge_HogCS100$Predictor <- "Hedgehog presence (%)" 




# combine all gge dfs into one big df for plotting with facet wrap
gge_all <- rbind(gge_GDN1000, gge_ALT750, gge_PLPK100, gge_WAT750, gge_WD1000,
                 gge_IMP750, gge_TERR1000, gge_logHD500, gge_BadgCS750, gge_HogCS100
                 )


ggplot(gge_all , aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(y= "Probability of fox presence", x = "Predictor" ) +
  facet_wrap(~Predictor, scales = "free",
             strip.position="bottom") +
  theme_bw()

ggsave("../../../results/CitiSci/AllYear/Fox/fox_predictor_ggplot.pdf", width=10, height=7.5, units="in", scale = 1)

