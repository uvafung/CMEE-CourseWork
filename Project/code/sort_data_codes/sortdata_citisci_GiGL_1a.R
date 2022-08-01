# Name: sortdata_citisci_1.R
# Description: This script is used to tidy the GiGL data and remove unnecessary 
# or duplicated data entries
# Author: Uva Fung uf21@ic.ac.uk
# Date: 29 Mar 2022

rm(list=ls())

require(dplyr)

data <- read.csv("../data/Mammalpost2005_point.csv")

### Check data
str(data)
head(data)
table(data$CommonName)
summary(data)
table(data$TaxonName)
table(data$CommonGrp)
table(data$RecYear)
table(data$SampleType)
table(data$SurveyName)

### Check column names and only keep necessary columns
colnames(data)
data_allfil<- data %>%
  select(TaxonName, CommonName, CommonGrp, Abundance, AbundCount, 
         RecYear, Recorder, GridRef, RefSystem, Grid10k, Grid2k, 
         Grid1k, GRPrec, Easting, Northing, RecType, SurveyName, 
         SurveyBy, NegativeRe, SampleType)

table(data_allfil$NegativeRe) #check the data

### Remove duplicate datasets and ZSL HogWatch cam trap data
data_allfil_nodup <- data_allfil %>%
  filter(!SurveyName %in% c('PTES Living With Mammals Survey 2003 - 2017', 
                            'PTES Hedgehog Hibernation Survey 2012 - 2014',
                            'PTES Big Hedgehog Map 2018 - 2019',
                            'PTES Big Hedgehog Map 2015 - 2018',
                            'PTES Living With Mammals Survey 2018',
                            'PTES Living With Mammals Survey 2019',
                            'ZSL Camera Trap Survey 2017',
                            'ZSL London Hogwatch Survey Hampstead Heath 2017 - 2018'))

table(data_allfil_nodup$SurveyName)


### Remove Negative Records and only keep the positive ones
data_allfil_pos <- data_allfil_nodup %>%
  filter(NegativeRe == 'N')

table(data_allfil_pos$NegativeRe) #check the data

table(data_allfil_pos$RecType)


### Remove longworth trap data for consistency
data_allfil_pos_met <- data_allfil_pos %>%
  filter(!RecType == 'Dormouse box checks') %>%
  filter(!RecType == 'trapped (other)') %>%
  filter(!RecType == 'Longworth Trap') %>%
  filter(!RecType == 'trapped in Longworth trap')
           

table(data_allfil_pos_met$RecType)
table(data_allfil_pos_met$SurveyName)



### Rename column from 'SurveyBy' to Dataset
colnames(data_allfil_pos_met)[which(names(data_allfil_pos_met) == "SurveyBy")] <- "Dataset"
data_allfil_pos_met



### Classify data into London Wildlife Trust, PTES, or GiGL based on Dataset
data_allfilter_nonrep_PTES <- data_allfil_pos_met %>%
  filter(Dataset == 'PTES, Peoples Trust for Endangered Species')

data_allfilter_nonrep_LWT <- data_allfil_pos_met %>%
  filter(Dataset == 'London Wildlife Trust')

data_allfilter_nonrep_GiGL <- data_allfil_pos_met %>%
  filter(!Dataset %in% c('PTES, Peoples Trust for Endangered Species',
                         'London Wildlife Trust')
         )

### Standardize Dataset name in GiGL to 'Available from GiGL'
data_allfilter_nonrep_GiGL$Dataset <- 'Available from GiGL'


### Merge the three PTES/LWT/GiGL dataframes together
sorted_data_allspp <- rbind(data_allfilter_nonrep_GiGL, data_allfilter_nonrep_LWT,
                            data_allfilter_nonrep_PTES)


### Only retain entries with target species and background species
table(sorted_data_allspp$CommonName)

sorted_data_targetspp <- sorted_data_allspp %>%
  filter(CommonName %in% c('Brown Rat', 
                           'Chinese Muntjac',
                           'Eurasian Badger', # target
                           'Red Fox', # target
                           'Roe Deer',
                           'West European Hedgehog', # target
                           'European Rabbit'))


### Save the data as csv
write.csv(sorted_data_targetspp, '../data/GiGLFiltered.csv')


### Filter data according to taxa
data_fox <- filter(data_allfilter, grepl('Vulpes vulpes', TaxonName))
data_badger <- filter(data_allfilter, grepl('Meles meles', TaxonName))
data_hedgehog <- filter(data_allfilter, grepl('Erinaceus europaeus', TaxonName))


data_fox <- write.csv("../data/GiGL_fox.csv")
data_badger <- write.csv("../data/GiGL_badger.csv")
data_hedgehog <- write.csv("../data/GiGL_hedgehog.csv")


a <- read.csv('../data/GiGLFiltered.csv')


