# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 29, 2021
# File name: SearchBestMod.R
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
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"))

Glance_gompertz_results <- Glance_gompertz_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"))

Glance_baranyi_results <- Glance_baranyi_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"))

Glance_gompertz_multstart_results <- Glance_gompertz_multstart_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"))

Glance_baranyi_multstart_results <- Glance_baranyi_multstart_results %>% 
  mutate(AIC = replace(AIC, isConv == "FALSE", "NA"))


# extract IDs, AIC and BIC of each model
OLS_AIC <- select(Glance_OLS_results, c("ID_no_Rep_dup", "AIC"))
Qua_AIC <- select(Glance_Qua_results, c("ID_no_Rep_dup", "AIC"))
Cub_AIC <- select(Glance_Cub_results, c("ID_no_Rep_dup", "AIC"))
Logistic_AIC <- select(Glance_logistic_results, c("ID_no_Rep_dup", "AIC"))
Gompertz_AIC <- select(Glance_gompertz_results, c("ID_no_Rep_dup", "AIC"))
Baranyi_AIC <- select(Glance_baranyi_results, c("ID_no_Rep_dup", "AIC"))
Gompertz_mult_AIC <- select(Glance_gompertz_multstart_results, c("ID_no_Rep_dup", "AIC"))
Baranyi_mult_AIC <- select(Glance_baranyi_multstart_results, c("ID_no_Rep_dup", "AIC"))

# rename column names to include the model name
OLS_AIC <- OLS_AIC %>% rename(AIC_OLS = AIC)
Qua_AIC <- Qua_AIC %>% rename(AIC_Qua = AIC)
Cub_AIC <- Cub_AIC %>% rename(AIC_Cub = AIC)
Logistic_AIC <- Logistic_AIC %>% rename(AIC_Log = AIC)
Gompertz_AIC <- Gompertz_AIC %>% rename(AIC_Gom = AIC)
Baranyi_AIC <- Baranyi_AIC %>% rename(AIC_Bar = AIC)
Gompertz_mult_AIC <- Gompertz_mult_AIC %>% rename(AIC_Gom_m = AIC)
Baranyi_mult_AIC <- Baranyi_mult_AIC %>% rename(AIC_Bar_m = AIC)


# merge all AIC dataframes and save as a new dataframe
AIC_all_nLS <- Reduce(function(x, y) merge(x, y, by = "ID_no_Rep_dup", all = TRUE), 
                       list(OLS_AIC, Qua_AIC, Cub_AIC, Logistic_AIC, Gompertz_AIC, Baranyi_AIC, Gompertz_mult_AIC, Baranyi_mult_AIC))

# remove AIC for datasets that do not fit assumptions
AIC_all_nLS["193", "AIC_OLS"] = "NA"  

# create new columns to store output of best model based on AIC output
AIC_all_nLS["Best_model_AIC"] = ""   


# output model with lowest AIC into new column
AIC_all_nLS$Best_model_AIC <- colnames(AIC_all_nLS[, c(2:9)])[apply(AIC_all_nLS[, c(2:9)],1,which.min)] 


table(AIC_all_nLS$Best_model_AIC)

write.csv(AIC_all_nLS, "../results/AIC_all_nLS.csv")
