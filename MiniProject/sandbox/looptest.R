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

