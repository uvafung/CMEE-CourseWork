# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 19, 2021
# File name: DataPrep.R
# Description: This script imports data for the miniproject and prepares 
# it for model fitting.

rm(list=ls())
require("tidyverse")
   
data = read.csv("../data/LogisticGrowthData.csv") # import raw data into script


##### Data wrangling #### KEEP
data <- data %>% 
  relocate(Citation, .before = Rep) %>%  # move Citation column before Rep 
  relocate(Temp, .before = Citation) %>% # move Temp column before Citation
  unite(ID, Species:Citation, sep = "_", remove = FALSE, na.rm = FALSE) %>% # unite Species, Medium and Citation columns and save the output in a new ID column
  transform(ID_no = as.numeric(factor(ID))) %>% # convert ID into unique ID numbers and save in a new column ID_no
  filter(Time > 0) %>%            # Only keep data with time > 0
  filter(PopBio > 0) %>%          # Only keep data with Population Size > 0
  mutate(LogPopBio = log(PopBio), .after = "PopBio") %>% # make new column storing Log Population Size 
  relocate(ID_no, .before = Rep) %>%
  unite(ID_no_Rep, ID_no:Rep, sep = "_", remove = FALSE, na.rm = FALSE) %>% # make a new column storing unique ID and no. of repeats
  transform(ID_no_Rep = as.character(factor(ID_no_Rep))) 
  

data <- data %>%
  group_by(ID_no_Rep) %>% # group by ID_no_Rep
  arrange(Time, .by_group = T) %>%   # arrange in ascending order by time
  group_by(ID_no_Rep, Grouping = cumsum(ID != lag(ID, default = first(ID)))) %>%
  mutate(Time_series = row_number()) %>% 
  ungroup()


Datapoint_counts <- data %>%
  group_by(ID_no_Rep) %>%
  summarise(No_datapoints = n())

data <- left_join(data, Datapoint_counts) # add column into data

data <- data %>% 
  filter(No_datapoints > 5) %>% # only keep data subsets with more than 5 measurements
  mutate(ID_no_Rep_dup = ID_no_Rep) # make a duplicate column for group_by function in model fitting


write.csv(data, "../data/ModifiedLogisticGrowthData.csv")





