# Name: sortdata_citisci_NBN_1.R
# Description: This script is used to tidy the NBN Atlas data and remove unnecessary 
# or duplicated data entries
# Author: Uva Fung uf21@ic.ac.uk
# Date: 30 Mar 2022

rm(list=ls())

require(dplyr)

badger <- read.csv("../data/NBN_Atlas_RawData/Badger_NBN.csv")
rat <- read.csv("../../data/NBN_Atlas_RawData/BrownRat_NBN.csv")
muntjac <- read.csv("../../data/NBN_Atlas_RawData/ChineseMuntjac_NBN.csv")
rabbit <- read.csv("../../data/NBN_Atlas_RawData/EuropeanRabbit_NBN.csv")
fox <- read.csv("../../data/NBN_Atlas_RawData/Fox_NBN.csv")
roedeer <- read.csv("../../data/NBN_Atlas_RawData/EuropeanRoeDeer_NBN.csv")
hedgehog <- read.csv("../../data/NBN_Atlas_RawData/Hedgehog_NBN.csv")

### Check data
str(badger)
head(badger)
table(badger$Start.date.year)
summary(data)
table(badger$Individual.count)
table(badger$Abundance)
table(badger$Dataset.name)
table(data$SampleType)
table(data$SurveyName)


###### 1. Badger ######

# Keep datasets from National Mammal Atlas and PTES Living with Mammals Survey
badger_fil <- badger %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording'))

table(badger_fil$Dataset.name)


# Remove Negative Records and only keep the positive ones
badger_fil  <- badger_fil  %>%
  filter(!Individual.count == '')

table(badger_fil$Individual.count) #check the data


# Keep records collected between 2005 - 2021
badger_fil <- badger_fil %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021'))

table(badger_fil$Start.date.year) #check the data

# Remove duplicate records and only retain distinct ones based on coord, dataset name, date of survey
badger_fil <- badger_fil %>% distinct(Latitude..WGS84., Longitude..WGS84., 
                                               Dataset.name, Start.date, .keep_all = TRUE)


# Keep records with coordinate uncertainty of <=100m
badger_fil <- badger_fil %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(badger_fil$Coordinate.uncertainty..m.) #check the data, no records have uncertainty <= 100m



###### 2. Rat ######
rat_fil <- rat %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(rat_fil$Dataset.name) #check the data
table(rat_fil$Individual.count)
table(rat_fil$Start.date.year)
table(rat_fil$Coordinate.uncertainty..m.)



###### 3. Muntjac ######
muntjac_fil <- muntjac %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(muntjac_fil$Dataset.name) #check the data
table(muntjac_fil$Individual.count)
table(muntjac_fil$Start.date.year)
table(muntjac_fil$Coordinate.uncertainty..m.)


###### 4. Rabbit ######
rabbit_fil <- rabbit %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(rabbit_fil$Dataset.name) #check the data
table(rabbit_fil$Individual.count)
table(rabbit_fil$Start.date.year)
table(rabbit_fil$Coordinate.uncertainty..m.)


###### 5. Fox ######
fox_fil <- fox %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(fox_fil$Dataset.name) #check the data
table(fox_fil$Individual.count)
table(fox_fil$Start.date.year)
table(fox_fil$Coordinate.uncertainty..m.)


###### 6. Roe deer ######
roedeer_fil <- roedeer %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(roedeer_fil$Dataset.name) #check the data
table(roedeer_fil$Individual.count)
table(roedeer_fil$Start.date.year)
table(roedeer_fil$Coordinate.uncertainty..m.)


###### 7. Hedgehog ######
hedgehog_fil <- hedgehog %>%
  filter(Dataset.name %in% c('Living with Mammals survey', 
                             'National Mammal Atlas Project, online recording')) %>%
  filter(!Individual.count == '') %>%
  filter(Start.date.year %in% c('2005', '2006', '2007', '2008', '2009', '2010',
                                '2011', '2012', '2013', '2014', '2015', '2016',
                                '2017', '2018', '2019', '2020', '2021')) %>%
  distinct(Latitude..WGS84., Longitude..WGS84., 
           Dataset.name, Start.date, .keep_all = TRUE) %>%
  filter(Coordinate.uncertainty..m. <= 100)

table(hedgehog_fil$Dataset.name) #check the data
table(hedgehog_fil$Individual.count)
table(hedgehog_fil$Start.date.year)
table(hedgehog_fil$Coordinate.uncertainty..m.)


########## Check all records are considered correct
table(hedgehog_fil$Identification.verification.status)
table(muntjac_fil$Identification.verification.status)
table(rabbit_fil$Identification.verification.status)
table(rat_fil$Identification.verification.status)
table(roedeer_fil$Identification.verification.status)


######## Only retain columns that I need for subsequent rbind ########
fox_fil<- fox_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)

hedgehog_fil<- hedgehog_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)

rabbit_fil<- rabbit_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)

rat_fil<- rat_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)

roedeer_fil<- roedeer_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)

muntjac_fil<- muntjac_fil %>%
  select(NBN.Atlas.record.ID, Occurrence.ID, Licence, Rightsholder, Scientific.name, 
         Common.name, Start.date, Start.date.year, Locality, OSGR, Latitude..WGS84., 
         Longitude..WGS84., Coordinate.uncertainty..m., Individual.count, Identification.verification.status, 
         Basis.of.record, Dataset.name, OSGR.100km, OSGR.10km, OSGR.2km, OSGR.1km,
         Ordnance.Survey.National.Grids...100km, Ordnance.Survey.National.Grids...10km,
         Ordnance.Survey.National.Grids...50km)


### Merge the different mammal dataframes together
NBN_all <- rbind(badger_fil, fox_fil, hedgehog_fil, muntjac_fil, rabbit_fil, rat_fil, roedeer_fil,
                 fill = TRUE)


### Save the data as csv
write.csv(NBN_all, '../../data/NBN_All_Filtered.csv')
write.csv(fox_fil, '../../data/NBN_Fox_Filtered.csv')
write.csv(hedgehog_fil, '../../data/NBN_Hedgehog_Filtered.csv')
write.csv(rabbit_fil, '../../data/NBN_Rabbit_Filtered.csv')
write.csv(rat_fil, '../../data/NBN_Rat_Filtered.csv')
write.csv(muntjac_fil, '../../data/NBN_Muntjac_Filtered.csv')
write.csv(roedeer_fil, '../../data/NBN_Roedeer_Filtered.csv')





