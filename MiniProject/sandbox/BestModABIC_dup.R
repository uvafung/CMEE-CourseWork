# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 29, 2021
# File name: BestModAIC.R
# Description: This script imports modified data to determine the best fitting model based on AIC

rm(list=ls())
require(tidyverse)

Glance_OLS_results <- read.csv("../data/Glance_OLS_results.csv")
Glance_Qua_results <- read.csv("../data/Glance_Qua_results.csv")
Glance_Cub_results <- read.csv("../data/Glance_Cub_results.csv")
Glance_logistic_results <- read.csv("../data/Glance_logistic_results.csv")
Glance_gompertz_results <- read.csv("../data/Glance_gompertz_results.csv")
Glance_baranyi_results <- read.csv("../data/Glance_baranyi_results.csv")
Glance_gompertz_multstart_results <- read.csv("../data/Glance_gompertz_multstart_results.csv")
Glance_baranyi_multstart_results <- read.csv("../data/Glance_baranyi_multstart_results.csv")


#################################################
###### Identify model with lowest AIC ###########
#################################################

# Remove AIC and BIC for datasets that did not converge
Glance_logistic_results <- Glance_logistic_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"), BIC = replace(BIC, isConv == "FALSE", "NA"))

Glance_gompertz_results <- Glance_gompertz_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"), BIC = replace(BIC, isConv == "FALSE", "NA"))

Glance_baranyi_results <- Glance_baranyi_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"), BIC = replace(BIC, isConv == "FALSE", "NA"))

Glance_gompertz_multstart_results <- Glance_gompertz_multstart_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"), BIC = replace(BIC, isConv == "FALSE", "NA"))

Glance_baranyi_multstart_results <- Glance_baranyi_multstart_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"), BIC = replace(BIC, isConv == "FALSE", "NA"))


# extract IDs, AIC and BIC of each model
OLS_ABIC <- select(Glance_OLS_results, c("ID_no_Rep_dup", "AIC", "BIC")) 
Qua_ABIC <- select(Glance_Qua_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Cub_ABIC <- select(Glance_Cub_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Logistic_ABIC <- select(Glance_logistic_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Gompertz_ABIC <- select(Glance_gompertz_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Baranyi_ABIC <- select(Glance_baranyi_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Gompertz_mult_ABIC <- select(Glance_gompertz_multstart_results, c("ID_no_Rep_dup", "AIC", "BIC"))
Baranyi_mult_ABIC <- select(Glance_baranyi_multstart_results, c("ID_no_Rep_dup", "AIC", "BIC"))


# rename column names to include the model name
colnames(OLS_ABIC)[2:3] <- paste(colnames(OLS_ABIC[,c(2:3)]), "_OLS", sep = "") 
colnames(Qua_ABIC)[2:3] <- paste(colnames(Qua_ABIC[,c(2:3)]), "_Qua", sep = "")
colnames(Cub_ABIC)[2:3] <- paste(colnames(Cub_ABIC[,c(2:3)]), "_Cub", sep = "")
colnames(Logistic_ABIC)[2:3] <- paste(colnames(Logistic_ABIC[,c(2:3)]), "_Log", sep = "")
colnames(Gompertz_ABIC)[2:3] <- paste(colnames(Gompertz_ABIC[,c(2:3)]), "_Gom", sep = "")
colnames(Baranyi_ABIC)[2:3] <- paste(colnames(Baranyi_ABIC[,c(2:3)]), "_Bar", sep = "")
colnames(Gompertz_mult_ABIC)[2:3] <- paste(colnames(Gompertz_mult_ABIC[,c(2:3)]), "_Gom_mult", sep = "")
colnames(Baranyi_mult_ABIC)[2:3] <- paste(colnames(Baranyi_mult_ABIC[,c(2:3)]), "_Bar_mult", sep = "")


# merge all ABIC dataframes and save as a new dataframe
ABIC_all_nLS <- Reduce(function(x, y) merge(x, y, by = "ID_no_Rep_dup", all = TRUE), 
                       list(OLS_ABIC, Qua_ABIC, Cub_ABIC, Logistic_ABIC, Gompertz_ABIC, Baranyi_ABIC, Gompertz_mult_ABIC, Baranyi_mult_ABIC))


ABIC_all_nLS <- ABIC_all_nLS[,order(colnames(ABIC_all_nLS))] %>% # arrange columns in alphabetical order
  relocate(ID_no_Rep_dup, .before = AIC_Bar) # move ID column to the front

# create new columns to store output of best model based on AIC output
ABIC_all_nLS["Best_model_AIC"] = ""   
ABIC_all_nLS["Best_model_BIC"] = ""

# output model with lowest AIC into new column
ABIC_all_nLS$Best_model_AIC <- colnames(ABIC_all_nLS[, c(2:9)])[apply(ABIC_all_nLS[, c(2:9)],1,which.min)] 

write.csv(ABIC_all_nLS, "../data/ABIC_all_nLS.csv")



table(ABIC_all_nLS$Best_model_AIC)