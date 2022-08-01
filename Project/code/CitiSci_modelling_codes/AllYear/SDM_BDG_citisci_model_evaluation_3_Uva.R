# Name: SDM_BDG_citisci_AllYear_model_evaluation_3.R
# Description: This script is used to: 
# 1) run SDM on CitiSci Badgers All year using Uva Fung's dataset to determine which 
#    model is the best fitting
# 2) Predict habitat suitability across entire dataset and plot the outcome 
# 3) Determine which variable has the greatest effect in habitat suitability 
#    by looking at change in AUC
# Author: Uva Fung uf21@ic.ac.uk
# Date: July 11 2022

rm(list=ls())

### Install useful packages
install.packages(c('dplyr', 'tidyverse', 'raster', 
                   'rgdal', 'dismo', 'rJava', 'maptools', 'rgeos'))

install.packages('ggplot2')
install.packages('usdm')
install.packages('corrplot')
install.packages('MASS')
install.packages('mgcv')
install.packages('randomForest')
install.packages('biomod2')
install.packages("PresenceAbsence")
install.packages("ecospat")
install.packages('ggeffects')
install.packages('stargazer')
install.packages('SDMtune')
install.packages('pROC')
install.packages('boot')

require(dplyr)
require(raster)
require(rgdal)
require(dismo)
require(rJava)
require(maptools)
require(rgeos)
require(ggplot2)
require(tidyverse)
require(usdm) # vif score
require(corrplot) # corrplot
require(MASS) #stepAIC
require(mgcv) #generalized additive models (gam)
require(randomForest) # random forest model
require(biomod2) # multiple modelling
require(PresenceAbsence) # calculate AUC scores
require(ecospat) # calculate TSS
require(ggeffects) # response curves
require(stargazer) # save eval table
library(stargazer)
require(SDMtune) # calculate TSS
require(pROC) # auc()
require(boot) # inverse logit



##########################################
############## Import data ###############
##########################################
setwd("Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear")
enviro <- read.csv('../../../data/EnviroMammal_df_July4.csv') # Load environmental variable dataset for modelling

# Only select presence absence entries
enviro <- enviro %>% subset(enviro$BADGERns %in% c(0,1)) 



##################################################
############## Check for normality ###############
##################################################

### Cleveland plots to visualize each environmental variable to check for normal distribution
ggplot(enviro, aes(ALT1000, id)) + geom_point() # % Allotment cover
ggplot(enviro, aes(AMN1000, id)) + geom_point() # % Amenity cover
ggplot(enviro, aes(CEM1000, id)) + geom_point() # % Cemetery cover
ggplot(enviro, aes(PLPK500, id)) + geom_point() # Park & play-space
ggplot(enviro, aes(SPRT1000, id)) + geom_point() # Sport cover
ggplot(enviro, aes(WAT1000, id)) + geom_point() # Water cover 
ggplot(enviro, aes(WD1000, id)) + geom_point() # Woodland
ggplot(enviro, aes(TRAF1000, id)) + geom_point() # Traffic
ggplot(enviro, aes(IMP1000, id)) + geom_point() # Impervious cover
ggplot(enviro, aes(DTCH1000, id)) + geom_point() # Detached housing 
ggplot(enviro, aes(SEMI250, id)) + geom_point() # Semi-detached housing
ggplot(enviro, aes(TERR1000, id)) + geom_point() # Terraced housing
ggplot(enviro, aes(DWL1000, id)) + geom_point() # Housing density
ggplot(enviro, aes(HD1000, id)) + geom_point() # Human population density
ggplot(enviro, aes(FoxCS100, id)) + geom_point() # Badger presence
ggplot(enviro, aes(GDN100, id)) + geom_point() # Garden cover
ggplot(enviro, aes(HogCS100, id)) + geom_point() # Fox presence

# Log transformation applied to human pop density, housing density and traffic
ggplot(enviro, aes(logHD1000, id)) + geom_point() # log Human population density
ggplot(enviro, aes(logDWL1000, id)) + geom_point() # log housing density
ggplot(enviro, aes(logTRAF1000, id)) + geom_point() # log Traffic

####################################################
############## Check for colinearity ###############
####################################################
### Identify and remove highly colinear variables

# Select predictor variables
enviro_scaled <- enviro[, c('ALT1000', 'AMN1000', 'CEM1000', 'PLPK500', 'SPRT1000', 
                            'WAT1000', 'WD1000', 'logTRAF1000', 'IMP1000', 'DTCH1000', 
                            'SEMI250', 'TERR1000', 'logDWL1000', 'logHD1000', 
                            'FoxCS100', 'GDN100', 'HogCS100')]

# Corrplot
corr_enviroscaled = cor(enviro_scaled)
corrplot(corr_enviroscaled, method = 'number') # colorful number

vif(enviro_scaled) # calculate vif score -- note that the values vary slightly everytime you rerun it


# Remove predictor with highest vif (logDWL1000)
enviro_var16 <- enviro[, c('ALT1000', 'AMN1000', 'CEM1000', 'PLPK500', 'SPRT1000', 
                           'WAT1000', 'WD1000', 'logTRAF1000', 'IMP1000', 'DTCH1000', 
                           'SEMI250', 'TERR1000', 'logHD1000', 
                           'FoxCS100', 'GDN100', 'HogCS100')]

vif(enviro_var16) # calculate vif score


####################################################
############## Model fitting #######################
####################################################


###### 1st model -- basic glm model ######
glm_vifchecked <- glm(BADGERns ~  
                        GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                        WAT1000 + WD1000 + IMP1000 + DTCH1000 + SEMI250 + TERR1000 + 
                        logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
              family = binomial(link="logit"), data = enviro)

summary(glm_vifchecked)


### Stepwise model selection using stepAIC
glm_step <- stepAIC(glm_vifchecked, direction = 'both', trace = FALSE)
glm_step # select predictors that gives the most parsimonious model eval using AIC


###### 2nd model -- GLM with Quadratic polynomial #####
glm_qua <- glm(BADGERns ~  
                 GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                 WAT1000 + WD1000 + poly(IMP1000, 2) + DTCH1000 + SEMI250 + TERR1000 + 
                 poly(logHD1000, 2) + logTRAF1000 + FoxCS100 + HogCS100, 
                      family = binomial(link="logit"), data = enviro)

summary(glm_qua)


### Stepwise model selection using stepAIC
glm_qua_step <- stepAIC(glm_qua, direction = 'both', trace = FALSE)
glm_qua_step


###### 3 model -- GLM with NO quadratic polynomial and YES selected variables after stepAIC #####
glm_qua_selected <- glm(BADGERns ~  
                          GDN100 + ALT1000 + AMN1000 + PLPK500 + 
                          WAT1000 + IMP1000 + DTCH1000 + TERR1000 + 
                          logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
               family = binomial(link="logit"), data = enviro)

summary(glm_qua_selected)

# perform stepAIC again to confirm all variables used are necessary
glm_qua_selected_step <- stepAIC(glm_qua_selected, direction = 'both', trace = FALSE) 
glm_qua_selected_step 





############## CHECK GLM models performance via AUC ############
auc_GLMmod <- as.data.frame(matrix(0, ncol = 100, nrow = 3, # create dataframe to store AUC outputs
                                       dimnames = list(c('GLM_AUC', 'GLM_Qua_AUC', 'GLM_Qua_selected_AUC'), NULL)))

for (i in 1:100){  # i = 100 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$BADGERns, ratio = 0.75) # 75/25 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_GLM = glm(BADGERns ~  # simple GLM
                      GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                      WAT1000 + WD1000 + IMP1000 + DTCH1000 + SEMI250 + TERR1000 + 
                      logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  auc_GLMmod[1, i] <- auc(eval$BADGERns, predict(glm.fit_GLM, eval, type='response')) # use eval dataset to predict to -- auc(response, predictor)
  
  
  glm.fit_GLM_Qua = glm(BADGERns ~  # GLM with quadratic variables
                          GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                          WAT1000 + WD1000 + poly(IMP1000, 2) + DTCH1000 + SEMI250 + TERR1000 + 
                          poly(logHD1000, 2) + logTRAF1000 + FoxCS100 + HogCS100,
                        family='binomial'(link = 'logit'), data = calib) # calib
  auc_GLMmod[2, i] <- auc(eval$BADGERns, predict(glm.fit_GLM_Qua, eval, type='response')) #eval
  
  
  glm.fit_GLM_Qua_selected <- glm(BADGERns ~  # GLM with selected quadratic variables after stepAIC
                                    GDN100 + ALT1000 + AMN1000 + PLPK500 + 
                                    WAT1000 + IMP1000 + DTCH1000 + TERR1000 + 
                                    logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
                          family = binomial(link="logit"), data = calib)
  auc_GLMmod[3, i] <- auc(eval$BADGERns, predict(glm.fit_GLM_Qua_selected, eval, type='response'))
  
}

auc_GLMmod <- auc_GLMmod %>% 
  mutate(Mean = rowMeans(auc_GLMmod)) # create new column to store the mean AUC




############## CHECK GLM models performance via TSS ############
tss_GLMmod <- as.data.frame(matrix(0, ncol = 100, nrow = 6, # create dataframe to store TSS outputs
                                           dimnames = list(c('GLM_TSS', 'GLM_Qua_TSS', 'GLM_Qua_selected_TSS',
                                                             'GLM_maxThreshold', 'GLM_Qua_maxThreshold',
                                                             'GLM_Qua_selected_maxThreshold')))) 
           
for (i in 1:100) { # i = 100 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$BADGERns, ratio = 0.75) # 75/25 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_GLM = glm(BADGERns ~  # simple GLM
                      GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                      WAT1000 + WD1000 + IMP1000 + DTCH1000 + SEMI250 + TERR1000 + 
                      logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
                    family='binomial'(link = 'logit'), data = calib) #  use calib dataset to built the model
  
  glm.fit_GLM_Qua = glm(BADGERns ~  # GLM with quadratic variables
                          GDN100 + ALT1000 + AMN1000 + CEM1000 + PLPK500 + SPRT1000 + 
                          WAT1000 + WD1000 + poly(IMP1000, 2) + DTCH1000 + SEMI250 + TERR1000 + 
                          poly(logHD1000, 2) + logTRAF1000 + FoxCS100 + HogCS100,
                        family='binomial'(link = 'logit'), data = calib) # calib
  
  glm.fit_GLM_Qua_selected <- glm(BADGERns ~  # GLM with selected quadratic variables after stepAIC
                                    GDN100 + ALT1000 + AMN1000 + PLPK500 + 
                                    WAT1000 + IMP1000 + DTCH1000 + TERR1000 + 
                                    logHD1000 + logTRAF1000 + FoxCS100 + HogCS100, 
                                  family = binomial(link="logit"), data = calib)
  
  
  GLM_tss <- ecospat.max.tss(predict(glm.fit_GLM, eval, type='response'), eval$BADGERns) # use eval to predict
  tss_GLMmod[1, i] <- GLM_tss$max.TSS
  tss_GLMmod[4, i] <- GLM_tss$max.threshold
  
  GLM_Qua_tss <- ecospat.max.tss(predict(glm.fit_GLM_Qua, eval, type='response'), eval$BADGERns)
  tss_GLMmod[2, i] <- GLM_Qua_tss$max.TSS
  tss_GLMmod[5, i] <- GLM_Qua_tss$max.threshold
  
  GLM_Qua_selected_tss <- ecospat.max.tss(predict(glm.fit_GLM_Qua_selected, eval, type='response'), eval$BADGERns)
  tss_GLMmod[3, i] <- GLM_Qua_selected_tss$max.TSS
  tss_GLMmod[6, i] <- GLM_Qua_selected_tss$max.threshold
  
  
}                                                 

tss_GLMmod <- tss_GLMmod %>% 
  mutate(Mean = rowMeans(tss_GLMmod)) # create new column to store the mean TSS and threshold


write.csv(auc_GLMmod, "../../../results/CitiSci/AllYear/Badger/badger_auc_GLMmod.csv")
write.csv(tss_GLMmod, "../../../results/CitiSci/AllYear/Badger/badger_tss_GLMmod.csv")


#######################################################################
###### Models 4-7 -- GAM, randomForest, ANN, Maxent using BIOMOD2 #####
#######################################################################

### Data preprataion for running biomod2

# load the dataset
enviro_biomod2 <- enviro[, c('Easting','Northing','BADGERns', 
                             'GDN100', 'ALT1000', "AMN1000", 'PLPK500', 'WAT1000',
                             'IMP1000', 'DTCH1000', 'TERR1000', 'logHD1000', 'logTRAF1000', 
                             'FoxCS100', 'HogCS100')] # all 12 variables



# convert response variable (BADGERns) to numeric
enviro_biomod2$BADGERns <- as.numeric(enviro_biomod2$BADGERns) 

# convert badger presence (FoxCS100) and hedgehog presence (HogCS100) to factor
enviro_biomod2$FoxCS100 <- as.factor(enviro_biomod2$FoxCS100) 
enviro_biomod2$HogCS100 <- as.factor(enviro_biomod2$HogCS100) 

# subset only the sampled_data with hedgehog presence/absence records
sampled_data <- enviro_biomod2 %>%
  subset(enviro_biomod2$BADGERns %in% c(0,1))

# define the explanatory variables
vars = c('GDN100', 'ALT1000', "AMN1000", 'PLPK500', 'WAT1000',
         'IMP1000', 'DTCH1000', 'TERR1000', 'logHD1000', 'logTRAF1000', 
         'FoxCS100', 'HogCS100')


# Format data using BIOMOD_FormatingData
biomod2_formatdata <- BIOMOD_FormatingData(resp.var = sampled_data$BADGERns,
                     expl.var = as.data.frame(sampled_data[, vars]),
                     resp.xy = cbind(sampled_data$Easting, sampled_data$Northing),
                     resp.name = 'Badger')

# maxent needed to be download separately to run -- locate maxent stored in file
myBiomodOptions <- BIOMOD_ModelingOptions(
  MAXENT.Phillips = list(path_to_maxent.jar = "~/Documents/CMEECourseWork/Project/code/maxent.jar")
  )

myBiomodModelOutPut <-  BIOMOD_Modeling(biomod2_formatdata, # run biomod analysis
                                      models = c('GLM', 'GAM', 'RF', 'ANN', 'MAXENT.Phillips'),
                                      models.options = myBiomodOptions,
                                      NbRunEval = 10, # Number of evaluations of each model
                                      DataSplit = 75, # Data split 75/25 train/test
                                      VarImport = 1,
                                      models.eval.meth = c('TSS','ROC'), # Stats to evaluate models
                                      do.full.models = FALSE,
                                      modeling.id = "badgermap"
)


# Check which models have been run and if all models run successfully
myBiomodModelOutPut

# Extract and print TSS and ROC scores
scores_all <- get_evaluations(myBiomodModelOutPut)


# Save a 'pretty' table with scores
eval_table = arrayhelpers::array2df(scores_all) # covert multidimensional array to dataframe
stargazer(eval_table,                
          summary = FALSE,
          type = "text",
          out = "../../../results/CitiSci/AllYear/Badger/badger_evaluation_scores_UF.txt")


eval_table_mean_score <- eval_table %>% 
  group_by(d1, d2, d3) %>% dplyr::summarize(cover = mean(scores_all)) %>% arrange(d1, d2)


# Plot scores
ggplot(subset(eval_table, d2 == "Testing.data"), aes(x=d3, y = scores_all, group=d3, fill=d3)) +
  geom_boxplot(position="dodge") + facet_wrap(~d1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("../../../results/CitiSci/AllYear/Badger/badger_evaluation_plot_UF.pdf")

# Only select TSS scores for testing data
scores_TSS <- as.numeric(scores_all["TSS","Testing.data",,,])


# Get variable important as df
var.imp <- data.frame(drop(get_variables_importance(myBiomodModelOutPut))) # return a variable importance value for each variable involved within your model
var.imp$variable = rownames(var.imp)

# Plot variable importance across models/runs
var.imp_melt = reshape2::melt(var.imp, "variable", variable.name="model")

# Bit of messing around to get run and model name separately
var.imp_melt$run = as.numeric(unlist(regmatches(var.imp_melt$model, gregexpr("[[:digit:]]+", var.imp_melt$model))))
txt = regmatches(var.imp_melt$model, gregexpr("[[:alpha:]]+", var.imp_melt$model))
var.imp_melt$model = unlist(lapply(txt, paste, collapse = "."))


# Boxplot of importance values

# Group by model using facet wrap
ggplot(var.imp_melt, aes(x = variable, fill = model, y=value)) +
  geom_boxplot(position="dodge") +
  facet_wrap(~model) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("../../../results/CitiSci/AllYear/Badger/badger_varimp_UF.pdf")



# All models clustered in one plot 
ggplot(var.imp_melt, aes(x = variable, fill=model, y=value)) +
  geom_boxplot(position="dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("../../../results/CitiSci/AllYear/Badger/badger_varimpB_UF.pdf")



#################################################################################
#### Perform spatially blocked cross validation (SI) on models run on biomod ####
#################################################################################

# Start by creating a blocked CV dataset
biomod_blocked_data_x = BIOMOD_cv(biomod2_formatdata,
                                  stratified.cv = TRUE,
                                  do.full.models = FALSE,
                                  stratify = "x",
                                  k = 5)

biomod_blocked_data_y = BIOMOD_cv(biomod2_formatdata,
                                  stratified.cv = TRUE,
                                  do.full.models = FALSE,
                                  stratify = "y",
                                  k = 5)

colnames(biomod_blocked_data_x) = paste0("x_", colnames(biomod_blocked_data_x))
colnames(biomod_blocked_data_y) = paste0("y_", colnames(biomod_blocked_data_y))
DataSplitTable <- cbind(biomod_blocked_data_x, biomod_blocked_data_y)


# Plot to visualize which section is used in blocking
temp = data.frame(cbind(Easting = sampled_data$Easting, Northing = sampled_data$Northing, DataSplitTable))
temp_melt = tidyr::pivot_longer(temp, !c(Easting, Northing))
ggplot(temp_melt, aes(x=Easting, y=Northing, color = value)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free_y")
ggsave("../../../results/CitiSci/AllYear/Badger/badger_blocking_map.pdf")

# Run models with spatial blocking
myBiomodModelOutPut_Blocking <- 
  BIOMOD_Modeling(
    biomod2_formatdata,
    DataSplitTable = DataSplitTable,
    models = c('GLM', 'GAM', 'RF', 'ANN', 'MAXENT.Phillips'), # Model types
    models.options = myBiomodOptions,
    VarImport = 1,
    models.eval.meth = c('TSS','ROC'), # Stats to evaluate models
    do.full.models = TRUE,
    modeling.id = "badgermap_blocking"
  )

myBiomodModelOutPut_Blocking # check if all models are run successfully
scores_all_blocking <- get_evaluations(myBiomodModelOutPut_Blocking, # extract scores
                                       as.data.frame = T)

# add a column that stores the name of the model
scores_all_blocking$model = str_sub(scores_all_blocking$Model.name, 1, 
                                    (str_locate(scores_all_blocking$Model.name, "_")[, 1] - 1))

# save another table with scores
stargazer(scores_all_blocking,                
          summary = FALSE,
          type = "text",
          out = "../../../results/CitiSci/AllYear/Badger/badger_blocking_evaluation_scores_UF.txt")


# saves a table which shows the TSS/ROC score by model for Testing data
scores_all_blocking_Testing.data <- scores_all_blocking %>% 
  group_by(Eval.metric, model) %>% dplyr::summarize(score = mean(Testing.data)) 


ggplot(scores_all_blocking, aes(x=Eval.metric, y = Testing.data, group=Eval.metric, fill=model)) + 
  geom_boxplot(position="dodge") + geom_point(position="jitter") + ylim(0, 1) + facet_grid(~model) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_fixed(ratio = 8)

ggsave("../../../results/CitiSci/AllYear/Badger/badger_blocking_evaluation_plot_UF.pdf")


# Restrict scores to TSS on testing data
scores_TSS_blocking <- as.numeric(subset(scores_all_blocking, Eval.metric == "TSS")$Testing.data)

# Sort by [mean across runs?]
score_thresh_blocking <- mean(tail(sort(scores_TSS_blocking),2)) #Not using this for ensemble below, just choosing 0.5 TSS


# Get variable important as df
var.imp.blocking <- data.frame(drop(get_variables_importance(myBiomodModelOutPut_Blocking)))
var.imp.blocking$variable = rownames(var.imp.blocking)

# Plot variable importance across models/runs
var.imp_melt_blocking = reshape2::melt(var.imp.blocking, "variable", variable.name="model")

runs_blocking = as.numeric(unlist(regmatches(var.imp_melt_blocking$model, 
                                             gregexpr("[[:digit:]]+", var.imp_melt_blocking$model))))

# add a column to include the number of runs
if (length(runs_blocking) == 0) runs_blocking = 1
var.imp_melt_blocking$run = runs_blocking


txt_blocking = regmatches(var.imp_melt_blocking$model, 
                          gregexpr("[[:alpha:]]+", var.imp_melt_blocking$model))

var.imp_melt_blocking$model = unlist(lapply(txt, paste, collapse = "."))

# Boxplot of importance values
ggplot(var.imp_melt_blocking, aes(x = variable, fill = model, y=value)) + 
  geom_boxplot(position="dodge") + 
  geom_point() + 
  facet_wrap(~model) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste0("../../../results/CitiSci/AllYear/Badger/badger_blocked_varimp_UF.pdf"))




################################################################################
####### Assess the use of selected absence vs random absence using Maxent ######
################################################################################

# Add a new column to store randomly generated background points 
enviro_random_bg <- 
  enviro_biomod2 %>% add_column(random_bg = NA)

# Randomly generate 2000 absence points (= 0)
enviro_absence_df <- enviro_random_bg[sample(nrow(enviro_random_bg), 2000), ]

# store random absence points as 0
enviro_absence_df$random_bg <- 0

# combine absence dataframe and enviro_random_bg dataframe tgt and remove duplicates
enviro_random_bg <- rbind(enviro_absence_df, enviro_random_bg) # df with absence must go first
                                                               # otherwise rows with 0 will be 
                                                               # removed in subsequent distinct()

enviro_random_bg <- enviro_random_bg %>% 
  distinct(Easting,Northing,BADGERns,
           GDN100, ALT1000, AMN1000, PLPK500, WAT1000,
           IMP1000, DTCH1000, TERR1000, logHD1000, logTRAF1000, 
           FoxCS100, HogCS100, .keep_all = TRUE)


# Add 1=presence in the new column based on BADGERns = 1
enviro_random_bg$random_bg [enviro_random_bg$BADGERns == 1] <- 1

# Check that the number of presence (1) are the same in both columns
table(enviro_random_bg$random_bg)
table(enviro_random_bg$BADGERns)


### Run Maxent on dataframe with randomly generated background points

# convert response variable (random_bg) to numeric
enviro_random_bg$random_bg <- as.numeric(enviro_random_bg$random_bg) 

# convert badger presence and hedgehog presence to factor
enviro_random_bg$FoxCS100 <- as.factor(enviro_random_bg$FoxCS100) 
enviro_random_bg$HogCS100 <- as.factor(enviro_random_bg$HogCS100) 

# subset only the sampled_data_random with badger presence/absence records
sampled_data_random <- enviro_random_bg %>%
  subset(enviro_random_bg$random_bg %in% c(0,1))


# Format data using BIOMOD_FormatingData
biomod2_formatdata_random <- BIOMOD_FormatingData(resp.var = sampled_data_random$random_bg,
                                           expl.var = as.data.frame(sampled_data_random[, vars]),
                                           resp.xy = cbind(sampled_data_random$Easting, sampled_data_random$Northing),
                                           resp.name = 'Badger_random')

myBiomodMaxentOutPut_random <-  BIOMOD_Modeling(biomod2_formatdata_random, # run biomod analysis 
                                        models = 'MAXENT.Phillips',
                                        models.options = myBiomodOptions,
                                        NbRunEval = 10, # Number of evaluations of each model
                                        DataSplit = 75, # Data split 75/25 train/test
                                        VarImport = 1,
                                        models.eval.meth = c('TSS','ROC'), # Stats to evaluate models
                                        do.full.models = FALSE,
                                        modeling.id = "badgermap_random"
)


myBiomodMaxentOutPut_random # check if biomod has run successfully
scores_all_randomMaxent <- get_evaluations(myBiomodMaxentOutPut_random) # Extract and print TSS and ROC scores


# Save a 'pretty' table with scores
eval_table_random = arrayhelpers::array2df(scores_all_randomMaxent) # covert multidimensional array to dataframe
eval_table_random


eval_table_mean_random <- eval_table_random %>% 
  group_by(d1, d2) %>% dplyr::summarize(cover = mean(scores_all_randomMaxent)) %>% arrange(d1, d2)


# Plot scores
ggplot(subset(eval_table_random, d2 == "Testing.data"), aes(x=d1, y = scores_all_randomMaxent, group=d1, fill=d1)) +
  geom_boxplot(position="dodge") + facet_wrap(~d2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Only select ROC (AUC) scores for testing data
# scores_AUC_random <- as.numeric(scores_all_randomMaxent["ROC","Testing.data",,,])
# sum(scores_AUC_random)/10 # average AUC over 10 runs

# Get variable important as df
var.imp.random <- data.frame(drop(get_variables_importance(myBiomodMaxentOutPut_random))) # return a variable importance value for each variable involved within your model
var.imp.random$variable = rownames(var.imp.random)



# Boxplot of importance values

# Group by model using facet wrap
ggplot(var.imp_melt, aes(x = variable, fill = model, y=value)) +
  geom_boxplot(position="dodge") +
  facet_wrap(~model) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("../../../results/CitiSci/AllYear/Badger/badger_varimp_UF.pdf")




############################################
##### Best model identified as GLM Qua #####
############################################
