# Name: SDM_FOX_citisci_scale_optimize.R
# Description: This script is used to check which distance scale is most  
# suitable for hedgehog citizen science data (all years) modelling using Uva Fung's dataset
# Author: Uva Fung uf21@ic.ac.uk
# Date: June 20 2022

rm(list=ls())

### Install useful packages
install.packages(c('dplyr', 'tidyverse', 'raster', 
                   'rgdal', 'dismo', 'rJava', 'maptools', 'rgeos'))

install.packages('ggplot2')
install.packages('pROC')

install.packages(c('MASS', 'mgcv', 'earth', 
                   'rpart', 'mda', 'Hmisc'))

install.packages('biomod2')

require(dplyr)
require(raster)
require(rgdal)
require(dismo)
require(rJava)
require(maptools)
require(rgeos)
require(ggplot2)
require(tidyverse)
require(pROC)

require(MASS)
require(mgcv)
require(earth)
require(rpart)
require(mda)
require(Hmisc)

require(biomod2)


### Load environmental variable dataset for modelling
setwd("/Users/Uva/Documents/CMEECourseWork/Project/code/CitiSci_modelling_codes/AllYear")

enviro <- read.csv('../../../data/EnviroMammal_df_July4.csv')

enviro <- enviro %>% 
  subset(enviro$FOXns %in% c(0,1)) # only select presence absence entries 

############################################################################################
### 20 repeat split sample cross validation to determine which scale has the highest AUC ###
############################################################################################


### 1. ALT 
auc_result_ALT <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                    dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation

a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
calib <- enviro[a$calibration, ] # enviro data for calibration
eval <- enviro[a$evaluation, ] # enviro data for eval

glm.fit_ALT100 = glm(FOXns ~ ALT100, family='binomial'(link = 'logit'), data = calib) # calib
auc_result_ALT[1, i] <- auc(eval$FOXns, predict(glm.fit_ALT100, eval, type='response')) # auc(response, predictor)

glm.fit_ALT250 = glm(FOXns ~ ALT250, family='binomial'(link = 'logit'), data = calib) # calib
auc_result_ALT[2, i] <- auc(eval$FOXns, predict(glm.fit_ALT250, eval, type='response'))

glm.fit_ALT500 = glm(FOXns ~ ALT500, family='binomial'(link = 'logit'), data = calib) # calib
auc_result_ALT[3, i] <- auc(eval$FOXns, predict(glm.fit_ALT500, eval, type='response'))

glm.fit_ALT750 = glm(FOXns ~ ALT750, family='binomial'(link = 'logit'), data = calib) # calib
auc_result_ALT[4, i] <- auc(eval$FOXns, predict(glm.fit_ALT750, eval, type='response'))

glm.fit_ALT1000 = glm(FOXns ~ ALT1000, family='binomial'(link = 'logit'), data = calib) # calib
auc_result_ALT[5, i] <- auc(eval$FOXns, predict(glm.fit_ALT1000, eval, type='response'))

}

auc_result_ALT <- auc_result_ALT %>% 
  mutate(Mean = rowMeans(auc_result_ALT)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Allotment cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column



#####################################

### 2. AMN 
auc_result_AMN <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_AMN100 = glm(FOXns ~ AMN100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_AMN[1, i] <- auc(eval$FOXns, predict(glm.fit_AMN100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_AMN250 = glm(FOXns ~ AMN250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_AMN[2, i] <- auc(eval$FOXns, predict(glm.fit_AMN250, eval, type='response'))
  
  glm.fit_AMN500 = glm(FOXns ~ AMN500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_AMN[3, i] <- auc(eval$FOXns, predict(glm.fit_AMN500, eval, type='response'))
  
  glm.fit_AMN750 = glm(FOXns ~ AMN750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_AMN[4, i] <- auc(eval$FOXns, predict(glm.fit_AMN750, eval, type='response'))
  
  glm.fit_AMN1000 = glm(FOXns ~ AMN1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_AMN[5, i] <- auc(eval$FOXns, predict(glm.fit_AMN1000, eval, type='response'))
  
}

auc_result_AMN <- auc_result_AMN %>% 
  mutate(Mean = rowMeans(auc_result_AMN)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Amenity cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 3. CEM 
auc_result_CEM <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_CEM100 = glm(FOXns ~ CEM100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_CEM[1, i] <- auc(eval$FOXns, predict(glm.fit_CEM100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_CEM250 = glm(FOXns ~ CEM250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_CEM[2, i] <- auc(eval$FOXns, predict(glm.fit_CEM250, eval, type='response'))
  
  glm.fit_CEM500 = glm(FOXns ~ CEM500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_CEM[3, i] <- auc(eval$FOXns, predict(glm.fit_CEM500, eval, type='response'))
  
  glm.fit_CEM750 = glm(FOXns ~ CEM750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_CEM[4, i] <- auc(eval$FOXns, predict(glm.fit_CEM750, eval, type='response'))
  
  glm.fit_CEM1000 = glm(FOXns ~ CEM1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_CEM[5, i] <- auc(eval$FOXns, predict(glm.fit_CEM1000, eval, type='response'))
  
}

auc_result_CEM <- auc_result_CEM %>% 
  mutate(Mean = rowMeans(auc_result_CEM)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Cemetery cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 4. PLPK 
auc_result_PLPK <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_PLPK100 = glm(FOXns ~ PLPK100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_PLPK[1, i] <- auc(eval$FOXns, predict(glm.fit_PLPK100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_PLPK250 = glm(FOXns ~ PLPK250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_PLPK[2, i] <- auc(eval$FOXns, predict(glm.fit_PLPK250, eval, type='response'))
  
  glm.fit_PLPK500 = glm(FOXns ~ PLPK500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_PLPK[3, i] <- auc(eval$FOXns, predict(glm.fit_PLPK500, eval, type='response'))
  
  glm.fit_PLPK750 = glm(FOXns ~ PLPK750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_PLPK[4, i] <- auc(eval$FOXns, predict(glm.fit_PLPK750, eval, type='response'))
  
  glm.fit_PLPK1000 = glm(FOXns ~ PLPK1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_PLPK[5, i] <- auc(eval$FOXns, predict(glm.fit_PLPK1000, eval, type='response'))
  
}

auc_result_PLPK <- auc_result_PLPK %>% 
  mutate(Mean = rowMeans(auc_result_PLPK)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Park and play spaces') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 5. SPRT
auc_result_SPRT <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                        dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_SPRT100 = glm(FOXns ~ SPRT100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SPRT[1, i] <- auc(eval$FOXns, predict(glm.fit_SPRT100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_SPRT250 = glm(FOXns ~ SPRT250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SPRT[2, i] <- auc(eval$FOXns, predict(glm.fit_SPRT250, eval, type='response'))
  
  glm.fit_SPRT500 = glm(FOXns ~ SPRT500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SPRT[3, i] <- auc(eval$FOXns, predict(glm.fit_SPRT500, eval, type='response'))
  
  glm.fit_SPRT750 = glm(FOXns ~ SPRT750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SPRT[4, i] <- auc(eval$FOXns, predict(glm.fit_SPRT750, eval, type='response'))
  
  glm.fit_SPRT1000 = glm(FOXns ~ SPRT1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SPRT[5, i] <- auc(eval$FOXns, predict(glm.fit_SPRT1000, eval, type='response'))
  
}

auc_result_SPRT <- auc_result_SPRT %>% 
  mutate(Mean = rowMeans(auc_result_SPRT)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Sport cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 6. WAT
auc_result_WAT <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                        dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_WAT100 = glm(FOXns ~ WAT100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WAT[1, i] <- auc(eval$FOXns, predict(glm.fit_WAT100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_WAT250 = glm(FOXns ~ WAT250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WAT[2, i] <- auc(eval$FOXns, predict(glm.fit_WAT250, eval, type='response'))
  
  glm.fit_WAT500 = glm(FOXns ~ WAT500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WAT[3, i] <- auc(eval$FOXns, predict(glm.fit_WAT500, eval, type='response'))
  
  glm.fit_WAT750 = glm(FOXns ~ WAT750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WAT[4, i] <- auc(eval$FOXns, predict(glm.fit_WAT750, eval, type='response'))
  
  glm.fit_WAT1000 = glm(FOXns ~ WAT1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WAT[5, i] <- auc(eval$FOXns, predict(glm.fit_WAT1000, eval, type='response'))
  
}

auc_result_WAT <- auc_result_WAT %>% 
  mutate(Mean = rowMeans(auc_result_WAT)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Water cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 7. WD
auc_result_WD <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_WD100 = glm(FOXns ~ WD100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WD[1, i] <- auc(eval$FOXns, predict(glm.fit_WD100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_WD250 = glm(FOXns ~ WD250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WD[2, i] <- auc(eval$FOXns, predict(glm.fit_WD250, eval, type='response'))
  
  glm.fit_WD500 = glm(FOXns ~ WD500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WD[3, i] <- auc(eval$FOXns, predict(glm.fit_WD500, eval, type='response'))
  
  glm.fit_WD750 = glm(FOXns ~ WD750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WD[4, i] <- auc(eval$FOXns, predict(glm.fit_WD750, eval, type='response'))
  
  glm.fit_WD1000 = glm(FOXns ~ WD1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_WD[5, i] <- auc(eval$FOXns, predict(glm.fit_WD1000, eval, type='response'))
  
}

auc_result_WD <- auc_result_WD %>% 
  mutate(Mean = rowMeans(auc_result_WD)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Woodland cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 8. TRAF
auc_result_TRAF <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                      dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_TRAF100 = glm(FOXns ~ TRAF100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TRAF[1, i] <- auc(eval$FOXns, predict(glm.fit_TRAF100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_TRAF250 = glm(FOXns ~ TRAF250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TRAF[2, i] <- auc(eval$FOXns, predict(glm.fit_TRAF250, eval, type='response'))
  
  glm.fit_TRAF500 = glm(FOXns ~ TRAF500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TRAF[3, i] <- auc(eval$FOXns, predict(glm.fit_TRAF500, eval, type='response'))
  
  glm.fit_TRAF750 = glm(FOXns ~ TRAF750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TRAF[4, i] <- auc(eval$FOXns, predict(glm.fit_TRAF750, eval, type='response'))
  
  glm.fit_TRAF1000 = glm(FOXns ~ TRAF1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TRAF[5, i] <- auc(eval$FOXns, predict(glm.fit_TRAF1000, eval, type='response'))
  
}

auc_result_TRAF <- auc_result_TRAF %>% 
  mutate(Mean = rowMeans(auc_result_TRAF)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Traffic volume') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 9. IMP
auc_result_IMP <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                          dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_IMP100 = glm(FOXns ~ IMP100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_IMP[1, i] <- auc(eval$FOXns, predict(glm.fit_IMP100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_IMP250 = glm(FOXns ~ IMP250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_IMP[2, i] <- auc(eval$FOXns, predict(glm.fit_IMP250, eval, type='response'))
  
  glm.fit_IMP500 = glm(FOXns ~ IMP500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_IMP[3, i] <- auc(eval$FOXns, predict(glm.fit_IMP500, eval, type='response'))
  
  glm.fit_IMP750 = glm(FOXns ~ IMP750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_IMP[4, i] <- auc(eval$FOXns, predict(glm.fit_IMP750, eval, type='response'))
  
  glm.fit_IMP1000 = glm(FOXns ~ IMP1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_IMP[5, i] <- auc(eval$FOXns, predict(glm.fit_IMP1000, eval, type='response'))
  
}

auc_result_IMP <- auc_result_IMP %>% 
  mutate(Mean = rowMeans(auc_result_IMP)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Impervious cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 10. DTCH 

auc_result_DTCH <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_DTCH100 = glm(FOXns ~ DTCH100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DTCH[1, i] <- auc(eval$FOXns, predict(glm.fit_DTCH100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_DTCH250 = glm(FOXns ~ DTCH250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DTCH[2, i] <- auc(eval$FOXns, predict(glm.fit_DTCH250, eval, type='response'))
  
  glm.fit_DTCH500 = glm(FOXns ~ DTCH500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DTCH[3, i] <- auc(eval$FOXns, predict(glm.fit_DTCH500, eval, type='response'))
  
  glm.fit_DTCH750 = glm(FOXns ~ DTCH750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DTCH[4, i] <- auc(eval$FOXns, predict(glm.fit_DTCH750, eval, type='response'))
  
  glm.fit_DTCH1000 = glm(FOXns ~ DTCH1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DTCH[5, i] <- auc(eval$FOXns, predict(glm.fit_DTCH1000, eval, type='response'))
  
}

auc_result_DTCH <- auc_result_DTCH %>% 
  mutate(Mean = rowMeans(auc_result_DTCH)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Detached housing') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 11. SEMI

auc_result_SEMI <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                        dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_SEMI100 = glm(FOXns ~ SEMI100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SEMI[1, i] <- auc(eval$FOXns, predict(glm.fit_SEMI100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_SEMI250 = glm(FOXns ~ SEMI250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SEMI[2, i] <- auc(eval$FOXns, predict(glm.fit_SEMI250, eval, type='response'))
  
  glm.fit_SEMI500 = glm(FOXns ~ SEMI500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SEMI[3, i] <- auc(eval$FOXns, predict(glm.fit_SEMI500, eval, type='response'))
  
  glm.fit_SEMI750 = glm(FOXns ~ SEMI750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SEMI[4, i] <- auc(eval$FOXns, predict(glm.fit_SEMI750, eval, type='response'))
  
  glm.fit_SEMI1000 = glm(FOXns ~ SEMI1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_SEMI[5, i] <- auc(eval$FOXns, predict(glm.fit_SEMI1000, eval, type='response'))
  
}

auc_result_SEMI <- auc_result_SEMI %>% 
  mutate(Mean = rowMeans(auc_result_SEMI)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Semi-detached housing') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 12. TERR

auc_result_TERR <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                        dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_TERR100 = glm(FOXns ~ TERR100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TERR[1, i] <- auc(eval$FOXns, predict(glm.fit_TERR100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_TERR250 = glm(FOXns ~ TERR250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TERR[2, i] <- auc(eval$FOXns, predict(glm.fit_TERR250, eval, type='response'))
  
  glm.fit_TERR500 = glm(FOXns ~ TERR500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TERR[3, i] <- auc(eval$FOXns, predict(glm.fit_TERR500, eval, type='response'))
  
  glm.fit_TERR750 = glm(FOXns ~ TERR750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TERR[4, i] <- auc(eval$FOXns, predict(glm.fit_TERR750, eval, type='response'))
  
  glm.fit_TERR1000 = glm(FOXns ~ TERR1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_TERR[5, i] <- auc(eval$FOXns, predict(glm.fit_TERR1000, eval, type='response'))
  
}

auc_result_TERR <- auc_result_TERR %>% 
  mutate(Mean = rowMeans(auc_result_TERR)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Terraced housing') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 13. DWL

auc_result_DWL <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                        dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_DWL100 = glm(FOXns ~ DWL100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DWL[1, i] <- auc(eval$FOXns, predict(glm.fit_DWL100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_DWL250 = glm(FOXns ~ DWL250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DWL[2, i] <- auc(eval$FOXns, predict(glm.fit_DWL250, eval, type='response'))
  
  glm.fit_DWL500 = glm(FOXns ~ DWL500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DWL[3, i] <- auc(eval$FOXns, predict(glm.fit_DWL500, eval, type='response'))
  
  glm.fit_DWL750 = glm(FOXns ~ DWL750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DWL[4, i] <- auc(eval$FOXns, predict(glm.fit_DWL750, eval, type='response'))
  
  glm.fit_DWL1000 = glm(FOXns ~ DWL1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_DWL[5, i] <- auc(eval$FOXns, predict(glm.fit_DWL1000, eval, type='response'))
  
}

auc_result_DWL <- auc_result_DWL %>% 
  mutate(Mean = rowMeans(auc_result_DWL)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Housing density') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 14. HD

auc_result_HD <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                       dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_HD100 = glm(FOXns ~ HD100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_HD[1, i] <- auc(eval$FOXns, predict(glm.fit_HD100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_HD250 = glm(FOXns ~ HD250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_HD[2, i] <- auc(eval$FOXns, predict(glm.fit_HD250, eval, type='response'))
  
  glm.fit_HD500 = glm(FOXns ~ HD500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_HD[3, i] <- auc(eval$FOXns, predict(glm.fit_HD500, eval, type='response'))
  
  glm.fit_HD750 = glm(FOXns ~ HD750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_HD[4, i] <- auc(eval$FOXns, predict(glm.fit_HD750, eval, type='response'))
  
  glm.fit_HD1000 = glm(FOXns ~ HD1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_HD[5, i] <- auc(eval$FOXns, predict(glm.fit_HD1000, eval, type='response'))
  
}

auc_result_HD <- auc_result_HD %>% 
  mutate(Mean = rowMeans(auc_result_HD)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Human density') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 15. Badger Presence

auc_result_Bpres <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                      dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_Bpres_100 = glm(FOXns ~ BadgCS100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Bpres[1, i] <- auc(eval$FOXns, predict(glm.fit_Bpres_100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_Bpres_250 = glm(FOXns ~ BadgCS250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Bpres[2, i] <- auc(eval$FOXns, predict(glm.fit_Bpres_250, eval, type='response'))
  
  glm.fit_Bpres_500 = glm(FOXns ~ BadgCS500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Bpres[3, i] <- auc(eval$FOXns, predict(glm.fit_Bpres_500, eval, type='response'))
  
  glm.fit_Bpres_750 = glm(FOXns ~ BadgCS750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Bpres[4, i] <- auc(eval$FOXns, predict(glm.fit_Bpres_750, eval, type='response'))
  
  glm.fit_Bpres_1000 = glm(FOXns ~ BadgCS1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Bpres[5, i] <- auc(eval$FOXns, predict(glm.fit_Bpres_1000, eval, type='response'))
  
}

auc_result_Bpres <- auc_result_Bpres %>% 
  mutate(Mean = rowMeans(auc_result_Bpres)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Badger presence') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################

### 16. GDN

auc_result_GDN <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                      dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_GDN100 = glm(FOXns ~ GDN100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_GDN[1, i] <- auc(eval$FOXns, predict(glm.fit_GDN100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_GDN250 = glm(FOXns ~ GDN250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_GDN[2, i] <- auc(eval$FOXns, predict(glm.fit_GDN250, eval, type='response'))
  
  glm.fit_GDN500 = glm(FOXns ~ GDN500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_GDN[3, i] <- auc(eval$FOXns, predict(glm.fit_GDN500, eval, type='response'))
  
  glm.fit_GDN750 = glm(FOXns ~ GDN750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_GDN[4, i] <- auc(eval$FOXns, predict(glm.fit_GDN750, eval, type='response'))
  
  glm.fit_GDN1000 = glm(FOXns ~ GDN1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_GDN[5, i] <- auc(eval$FOXns, predict(glm.fit_GDN1000, eval, type='response'))
  
}

auc_result_GDN <- auc_result_GDN %>% 
  mutate(Mean = rowMeans(auc_result_GDN)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Garden cover') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column

#####################################
### 17. Hedgehog Presence

auc_result_Hpres <- as.data.frame(matrix(0, ncol = 20, nrow = 5, # create dataframe to store AUC outputs
                                         dimnames = list(c('100m', '250m', '500m', '750m', '1000m'), NULL)))

for (i in 1:20){  # i = 20 repeat split sample cross validation
  
  a <- SampleMat2(ref = enviro$FOXns, ratio = 0.7) # 70/30 split for calibration and evaluation
  calib <- enviro[a$calibration, ] # enviro data for calibration
  eval <- enviro[a$evaluation, ] # enviro data for eval
  
  glm.fit_Hpres_100 = glm(FOXns ~ HogCS100, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Hpres[1, i] <- auc(eval$FOXns, predict(glm.fit_Hpres_100, eval, type='response')) # auc(response, predictor)
  
  glm.fit_Hpres_250 = glm(FOXns ~ HogCS250, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Hpres[2, i] <- auc(eval$FOXns, predict(glm.fit_Hpres_250, eval, type='response'))
  
  glm.fit_Hpres_500 = glm(FOXns ~ HogCS500, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Hpres[3, i] <- auc(eval$FOXns, predict(glm.fit_Hpres_500, eval, type='response'))
  
  glm.fit_Hpres_750 = glm(FOXns ~ HogCS750, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Hpres[4, i] <- auc(eval$FOXns, predict(glm.fit_Hpres_750, eval, type='response'))
  
  glm.fit_Hpres_1000 = glm(FOXns ~ HogCS1000, family='binomial'(link = 'logit'), data = calib) # calib
  auc_result_Hpres[5, i] <- auc(eval$FOXns, predict(glm.fit_Hpres_1000, eval, type='response'))
  
}

auc_result_Hpres <- auc_result_Hpres %>% 
  mutate(Mean = rowMeans(auc_result_Hpres)) %>% # create new column to store the mean AUC
  mutate(Predictor = 'Hedgehog presence') %>% # create new column to store the predictor name 
  relocate(Predictor) %>% # relocate Predictor to the first column
  mutate(Scale = c(100, 250, 500, 750, 1000)) %>% # add new column for scale
  relocate(Scale) # move Scale to first column




#####################################
# combine all auc result dfs together for plotting
auc_result_all <- rbind(auc_result_ALT, auc_result_AMN, auc_result_CEM, auc_result_DTCH,
                        auc_result_DWL, auc_result_GDN, auc_result_HD, auc_result_IMP,
                        auc_result_PLPK, auc_result_SEMI, auc_result_SPRT, auc_result_TERR,
                        auc_result_TRAF, auc_result_WAT, auc_result_WD, auc_result_Bpres, 
                        auc_result_Hpres)
                      

auc_results_plot <- subset(auc_result_all, select = -c(Mean) ) # select all columns except the mean column

# convert df into long format
auc_results_plot <- auc_results_plot %>% pivot_longer(
  cols=c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10',
         'V11', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V18', 'V19', 'V20'),
  names_to='Repeats', values_to='AUC')


# plot boxplot
ggplot(auc_results_plot, aes(x=as.factor(Scale), y=AUC)) + 
  geom_boxplot() +
  facet_wrap(~Predictor, scales = "free_y") +
  xlab("Scale (m)") + ylab("AUC Score") +
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    size = 2, linetype = "solid"),
    axis.line = element_line(colour = "grey")
                   
  )

ggsave("../../../results/CitiSci/AllYear/Fox_scale.pdf", width=12, height=8)

write.csv(auc_result_all, "../../../results/CitiSci/AllYear/Fox_scale.csv") 

