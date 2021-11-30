### Useful in data wrangling
Species_list <- unique(data$Species) # list all species
print(Species_list)



############# Logistic model - Loop through multiple datasets ##### 
data_subset <- data %>% group_by(ID_no_Rep)

Tidy_logistic <- function(data, ...) {
  nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = min(data_subset$LogPopBio),
             r_max = r_val), control = list(maxiter = 500)) %>% 
    tidy() %>% # constructs tibble that stores coefficient and p-value outputs
    pivot_wider(names_from = "term", values_from = c(estimate, std.error, statistic, p.value))
}

Glance_logistic <- function(data, ...) {
  nlsLM(LogPopBio ~ logistic(t = Time, r, K, N0), data = data,
        list(K = max(data_subset$LogPopBio),
             N0 = data_subset$LogPopBio[which.min(data_subset$Time_series)],
             r_max = r_val), control = list(maxiter = 500)) %>% 
    glance() # constructs tibble that stores AIC and BIC outputs
}

data_subset <- data %>% group_by(ID_no_Rep)
Tidy_logistic_output <- data_subset %>% group_modify(~ Tidy_logistic (data = .))
Glancelogistic_output <- data_subset %>% group_modify(~ Glance_logistic (data = .))

######################################################################
################ Loop for calculating best model based on AIC ################
Max_AIC <- apply(ABIC_all_nLS[1, c(2:9)], 1, FUN = max, na.rm=TRUE) # for first row and column 2-7 (AIC), save the largest AIC value
Second_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[2], na.last = TRUE)
Third_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[3])
Fourth_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[4])
Fifth_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[5])
Sixth_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[6])
Seventh_Max <- as.numeric(sort(ABIC_all_nLS[1, c(2:9)], TRUE)[7])
Min_AIC <- apply(ABIC_all_nLS[c(2:9)], 1, FUN = min) # for first row and column 2-7 (AIC), save the smallest AIC value

abs(Min_AIC - Seventh_Max) > 2

ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:9)])[apply(ABIC_all_nLS[1, c(2:9)],1,which.min)]


All_Max <- sort(ABIC_all_nLS[ , c(2:9)], TRUE, na.last = TRUE)


for (i in 2:9){
  
  Max_AIC <- apply(ABIC_all_nLS[i], 1, FUN = max, na.rm=TRUE) # for first row and column 2-7 (AIC), save the largest AIC value
  Min_AIC <- apply(ABIC_all_nLS[i], 1, FUN = min, na.rm=TRUE) # for first row and column 2-7 (AIC), save the smallest AIC value
  
  for (j in 1:284){
    vec_sort_order = as.numeric(sort(ABIC_all_nLS[j,2:9], na.last=NA))
    Second_Max <- vec_sort_order[1]
    Third_Max <- vec_sort_order[2]
    Fourth_Max <- vec_sort_order[3]
    Fifth_Max <- vec_sort_order[4]
    Sixth_Max <- vec_sort_order[5]
    Seventh_Max <- vec_sort_order[6]
    
    if (Second_Max[j] = NA){
      Second_Max[j] = Min_AIC[j]
    }
    if (Third_Max[j] = NA){
      Third_Max[j] = Min_AIC[j]
    }
    if (Fourth_Max[j] = NA){
      Fourth_Max[j] = Min_AIC[j]
    }
    if (Fifth_Max[j] = NA){
      Fifth_Max[j] = Min_AIC[j]
    }
    if (Sixth_Max[j] = NA){
      Sixth_Max[j] = Min_AIC[j]
    }
    if (Seventh_Max[j] = NA){
      Seventh_Max[j] = Min_AIC[j]
    }
    
    if (abs(Min_AIC[j] - Seventh_Max) > 2 ){
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)] # save name of model with lowest AIC in new column
    } else if (abs(Min_AIC - Sixth_Max) > 2) {
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)]
    } else if (abs(Min_AIC - Fifth_Max) > 2) {
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)]
    } else if (abs(Min_AIC - Fourth_Max) > 2) {
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)]
    } else if (abs(Min_AIC - Third_Max) > 2) {
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)]
    } else if (abs(Min_AIC - Second_Max) > 2) {
      ABIC_all_nLS$Best_model_AIC[j] <- colnames(ABIC_all_nLS[2:9])[apply(ABIC_all_nLS[j, 2:9],1,which.min)]
    } else{
      ABIC_all_nLS$Best_model_AIC[j] <- print("No")
    }
    
  }
}







if (abs(Min_AIC - Second_Min) > 2 ){
  ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:7)])[apply(ABIC_all_nLS[1, c(2:7)],1,which.min)] # save name of model with lowest AIC in new column
} else if (abs(Min_AIC - Third_Min) > 2) {
  ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:7)])[apply(ABIC_all_nLS[1, c(2:7)],1,which.min)]
} else if (abs(Min_AIC - Third_Max) > 2) {
  ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:7)])[apply(ABIC_all_nLS[1, c(2:7)],1,which.min)]
} else if (abs(Min_AIC - Second_Max) > 2) {
  ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:7)])[apply(ABIC_all_nLS[1, c(2:7)],1,which.min)]
} else if (abs(Min_AIC - Max_AIC) > 2) {
  ABIC_all_nLS$Best_model_AIC[1] <- colnames(ABIC_all_nLS[1, c(2:7)])[apply(ABIC_all_nLS[1, c(2:7)],1,which.min)]
} else{
  ABIC_all_nLS$Best_model_AIC[1] <- print("No")
}


