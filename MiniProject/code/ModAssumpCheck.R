# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 29, 2021
# File name: ModAssumpCheck.R
# Description: This script imports fitted valeus and residuals of each fitted 
# model to check that assumptions are met. 

rm(list=ls())
require(ggplot2)
require(ggforce) # for separating facets in mutiple pages for easy visualization

Aug_OLS = read.csv("../data/Augment_OLS_results.csv")
Aug_Qua = read.csv("../data/Augment_Qua_results.csv")
Aug_Cub = read.csv("../data/Augment_Cub_results.csv")
Aug_Log = read.csv("../data/Augment_logistic_results.csv")
Aug_Gom = read.csv("../data/Augment_gompertz_results.csv")
Aug_Bar = read.csv("../data/Augment_baranyi_results.csv")
Aug_Gom_m = read.csv("../data/Augment_gompertz_multstart_results.csv")
Aug_Bar_m = read.csv("../data/Augment_baranyi_multstart_results.csv")


###### Test assumptions in OLS #####
# 1 - Residual vs Fitted #
ggplot(Aug_OLS, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_OLS$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_OLS, aes(x = .std.resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Standard residuals", y = "Count") +
  facet_wrap_paginate(Aug_OLS$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 3 - Scale Location
Aug_OLS$sqrt.st.resid <- sqrt(Aug_OLS$.std.resid)

ggplot(Aug_OLS, aes(x = .fitted , y = sqrt.st.resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_OLS$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Quadratic model #####
# 1 - Residual vs Fitted #
ggplot(Aug_Qua, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Qua$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Qua, aes(x = .std.resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Standard residuals", y = "Count") +
  facet_wrap_paginate(Aug_Qua$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 3 - Scale Location
Aug_Qua$sqrt.st.resid <- sqrt(Aug_Qua$.std.resid)

ggplot(Aug_Qua, aes(x = .fitted , y = sqrt.st.resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Qua$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Cubic equation #####
# 1 - Residual vs Fitted #
ggplot(Aug_Cub, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Cub$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Cub, aes(x = .std.resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Standard residuals", y = "Count") +
  facet_wrap_paginate(Aug_Cub$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 3 - Scale Location
Aug_Cub$sqrt.st.resid <- sqrt(Aug_Cub$.std.resid)

ggplot(Aug_Cub, aes(x = .fitted , y = sqrt.st.resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Cub$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Logistic equation #####
# 1 - Residual vs Fitted 
ggplot(Aug_Log, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Log$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Log, aes(x = .resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Residuals", y = "Count") +
  facet_wrap_paginate(Aug_Log$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Gompertz equation #####
# 1 - Residual vs Fitted 
ggplot(Aug_Gom, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Gom$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Gom, aes(x = .resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Residuals", y = "Count") +
  facet_wrap_paginate(Aug_Gom$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Baranyi equation #####
# 1 - Residual vs Fitted 
ggplot(Aug_Bar, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Bar$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Bar, aes(x = .resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Residuals", y = "Count") +
  facet_wrap_paginate(Aug_Bar$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Gompertz equation -- mulstart #####
# 1 - Residual vs Fitted 
ggplot(Aug_Gom_m, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Gom_m$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)

# 2 - Normality
ggplot(Aug_Gom_m, aes(x = .resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Residuals", y = "Count") +
  facet_wrap_paginate(Aug_Gom_m$ID_no_Rep_dup, ncol = 5, nrow = 2, page = 20)


###### Test assumptions in Baranyi equation -- mulstart #####
# 1 - Residual vs Fitted 
ggplot(Aug_Bar_m, aes(x = .fitted , y = .resid)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", col = "red", se = F) + 
  theme_bw() + # make the background white
  labs(x = "Residual", y = "Fitted") +
  facet_wrap_paginate(Aug_Bar_m$ID_no_Rep_dup, ncol = 4, nrow = 1, page = 20)

# 2 - Normality
ggplot(Aug_Bar_m, aes(x = .resid)) + 
  geom_histogram(binwidth=1) + 
  theme_bw() + # make the background white
  labs(x = "Residuals", y = "Count") +
  facet_wrap_paginate(Aug_Bar_m$ID_no_Rep_dup, ncol = 5, nrow = 1, page = 20)

