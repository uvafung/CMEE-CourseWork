# Name: sortdata_citisci_combine_GiGLNBN_1b.R
# Description: This script is used to combine the filtered csv of GiGL and NBN datasets 
# to load it into QIGIS for enviromental variables extraction
# Author: Uva Fung uf21@ic.ac.uk
# Date: 6 June 2022

rm(list=ls())

install.packages("rnrfa")

require(rnrfa)
require(dplyr)

GiGL <- read.csv("../../data/GiGLFiltered.csv")
NBN <- read.csv("../../data/NBN_All_Filtered.csv")
table(NBN$Common.name)
table(NBN$Dataset.name)

# Only select columns that I need for analysis
GiGL_fil <- GiGL %>%
  select(TaxonName, CommonName, Easting, Northing, RecYear) %>% rename(Year= RecYear)

NBN_fil <- NBN %>%
  select(Scientific.name, Common.name, OSGR, Start.date.year) %>% rename(Year = Start.date.year)

# Check common name and scientific name to make sure 
# there are no spelling mistakes/naming discrepancy 
table(GiGL_fil$TaxonName)
table(NBN_fil$Scientific.name)
table(GiGL_fil$CommonName)
table(NBN_fil$Common.name)

# remove row in NBN_fil with common name and scientific name = TRUE
NBN_fil <- NBN_fil[!grepl('TRUE', NBN_fil$Common.name),]
table(NBN_fil$Common.name)
table(NBN_fil$Scientific.name)

# rename columns
GiGL_fil <- GiGL_fil %>% rename(ScientificName = TaxonName)
NBN_fil <- NBN_fil %>% rename(ScientificName = Scientific.name,
                              CommonName = Common.name)


# Convert OSGR to Easting and Northing
x = osg_parse(NBN_fil$OSGR)

# Extract Easting and Northing from the list, x
NBN_fil$Easting = x[[1]]
NBN_fil$Northing = x[[2]]


# Only select columns with Easting/Northing as coordinates
NBN_fil <- NBN_fil %>%
  select(ScientificName, CommonName, Easting, Northing, Year)

# rbind both dataframes for QGIS
Combined_GiGL_NBN <- rbind(GiGL_fil, NBN_fil, fill = TRUE)

# remove weird row with TRUE
Combined_GiGL_NBN <- Combined_GiGL_NBN[!grepl('TRUE', Combined_GiGL_NBN$CommonName),]

table(Combined_GiGL_NBN$CommonName)
table(Combined_GiGL_NBN$ScientificName)


# Filter into different df according to species

badger_GiGL_NBN <- Combined_GiGL_NBN %>% filter(CommonName == 'Eurasian Badger') # Badger
badger_bg_GiGL_NBN <- Combined_GiGL_NBN %>% filter(!CommonName == 'Eurasian Badger') # Background dataset for badger
hedgehog_GiGL_NBN <- Combined_GiGL_NBN %>% filter(CommonName == 'West European Hedgehog') # Hedgehog
hedgehog_bg_GiGL_NBN <- Combined_GiGL_NBN %>% filter(!CommonName == 'West European Hedgehog') # Background dataset 
fox_GiGL_NBN <- Combined_GiGL_NBN %>% filter(CommonName == 'Red Fox') # Red Fox
fox_bg_GiGL_NBN <- Combined_GiGL_NBN %>% filter(!CommonName == 'Red Fox') # Background dataset 


# filter into different years -- added June 6 2022
table(badger_GiGL_NBN$Year)
table(badger_bg_GiGL_NBN$Year)

table(hedgehog_GiGL_NBN$Year)
table(hedgehog_bg_GiGL_NBN$Year)

table(fox_GiGL_NBN$Year)
table(fox_bg_GiGL_NBN$Year)

# select records -- group by year
# 2005 - 2009
badger_GiGL_NBN_0509 <- badger_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009)) # Badger
badger_bg_GiGL_NBN_0509 <- badger_bg_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009))
hedgehog_GiGL_NBN_0509 <- hedgehog_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009)) # hedgehog
hedgehog_bg_GiGL_NBN_0509 <- hedgehog_bg_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009))
fox_GiGL_NBN_0509 <- fox_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009)) # fox
fox_bg_GiGL_NBN_0509 <- fox_bg_GiGL_NBN %>% filter(Year %in% c(2005, 2006, 2007, 2008, 2009))


# select records -- group by year
# 2010 - 2013
badger_GiGL_NBN_1013 <- badger_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013)) # Badger
badger_bg_GiGL_NBN_1013 <- badger_bg_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013))
hedgehog_GiGL_NBN_1013 <- hedgehog_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013)) # hedgehog
hedgehog_bg_GiGL_NBN_1013 <- hedgehog_bg_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013))
fox_GiGL_NBN_1013 <- fox_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013)) # fox
fox_bg_GiGL_NBN_1013 <- fox_bg_GiGL_NBN %>% filter(Year %in% c(2010, 2011, 2012, 2013))


# select records -- group by year
# 2014 - 2017
badger_GiGL_NBN_1417 <- badger_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017)) # Badger
badger_bg_GiGL_NBN_1417 <- badger_bg_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017))
hedgehog_GiGL_NBN_1417 <- hedgehog_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017)) # hedgehog
hedgehog_bg_GiGL_NBN_1417 <- hedgehog_bg_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017))
fox_GiGL_NBN_1417 <- fox_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017)) # fox
fox_bg_GiGL_NBN_1417 <- fox_bg_GiGL_NBN %>% filter(Year %in% c(2014, 2015, 2016, 2017))


# select records -- group by year
# 2018 - 2021
badger_GiGL_NBN_1821 <- badger_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021)) # Badger
badger_bg_GiGL_NBN_1821 <- badger_bg_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021))
hedgehog_GiGL_NBN_1821 <- hedgehog_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021)) # hedgehog
hedgehog_bg_GiGL_NBN_1821 <- hedgehog_bg_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021))
fox_GiGL_NBN_1821 <- fox_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021)) # fox
fox_bg_GiGL_NBN_1821 <- fox_bg_GiGL_NBN %>% filter(Year %in% c(2018, 2019, 2020, 2021))


# checking
table(badger_GiGL_NBN_0509$Year)
table(badger_bg_GiGL_NBN_0509$Year)
table(fox_GiGL_NBN_0509$Year)
table(fox_bg_GiGL_NBN_0509$Year)
table(hedgehog_GiGL_NBN_0509$Year)
table(hedgehog_bg_GiGL_NBN_0509$Year)

table(badger_GiGL_NBN_1013$Year)
table(badger_bg_GiGL_NBN_1013$Year)
table(fox_GiGL_NBN_1013$Year)
table(fox_bg_GiGL_NBN_1013$Year)
table(hedgehog_GiGL_NBN_1013$Year)
table(hedgehog_bg_GiGL_NBN_1013$Year)

table(badger_GiGL_NBN_1417$Year)
table(badger_bg_GiGL_NBN_1417$Year)
table(fox_GiGL_NBN_1417$Year)
table(fox_bg_GiGL_NBN_1417$Year)
table(hedgehog_GiGL_NBN_1417$Year)
table(hedgehog_bg_GiGL_NBN_1417$Year)

table(badger_GiGL_NBN_1821$Year)
table(badger_bg_GiGL_NBN_1821$Year)
table(fox_GiGL_NBN_1821$Year)
table(fox_bg_GiGL_NBN_1821$Year)
table(hedgehog_GiGL_NBN_1821$Year)
table(hedgehog_bg_GiGL_NBN_1821$Year)


# Write clean file
write.csv(Combined_GiGL_NBN, "../../data/Combined_GiGL_NBN.csv", row.names=F)

write.csv(badger_GiGL_NBN, "../../data/badger_GiGL_NBN.csv", row.names=F)
write.csv(hedgehog_GiGL_NBN, "../../data/hedgehog_GiGL_NBN.csv", row.names=F)
write.csv(fox_GiGL_NBN, "../../data/fox_GiGL_NBN.csv", row.names=F)

write.csv(badger_bg_GiGL_NBN, "../../data/badger_bg_GiGL_NBN.csv", row.names=F)
write.csv(hedgehog_bg_GiGL_NBN, "../../data/hedgehog_bg_GiGL_NBN.csv", row.names=F)
write.csv(fox_bg_GiGL_NBN, "../../data/fox_bg_GiGL_NBN.csv", row.names=F)


# 05/09
write.csv(badger_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/badger_GiGL_NBN_0509.csv", row.names=F)
write.csv(badger_bg_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/badger_bg_GiGL_NBN_0509.csv", row.names=F)
write.csv(fox_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/fox_GiGL_NBN_0509.csv", row.names=F)
write.csv(fox_bg_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/fox_bg_GiGL_NBN_0509.csv", row.names=F)
write.csv(hedgehog_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_GiGL_NBN_0509.csv", row.names=F)
write.csv(hedgehog_bg_GiGL_NBN_0509, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_bg_GiGL_NBN_0509.csv", row.names=F)


# 10/13
write.csv(badger_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/badger_GiGL_NBN_1013.csv", row.names=F)
write.csv(badger_bg_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/badger_bg_GiGL_NBN_1013.csv", row.names=F)
write.csv(fox_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/fox_GiGL_NBN_1013.csv", row.names=F)
write.csv(fox_bg_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/fox_bg_GiGL_NBN_1013.csv", row.names=F)
write.csv(hedgehog_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_GiGL_NBN_1013.csv", row.names=F)
write.csv(hedgehog_bg_GiGL_NBN_1013, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_bg_GiGL_NBN_1013.csv", row.names=F)

# 14/17
write.csv(badger_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/badger_GiGL_NBN_1417.csv", row.names=F)
write.csv(badger_bg_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/badger_bg_GiGL_NBN_1417.csv", row.names=F)
write.csv(fox_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/fox_GiGL_NBN_1417.csv", row.names=F)
write.csv(fox_bg_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/fox_bg_GiGL_NBN_1417.csv", row.names=F)
write.csv(hedgehog_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_GiGL_NBN_1417.csv", row.names=F)
write.csv(hedgehog_bg_GiGL_NBN_1417, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_bg_GiGL_NBN_1417.csv", row.names=F)


# 18/21
write.csv(badger_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/badger_GiGL_NBN_1821.csv", row.names=F)
write.csv(badger_bg_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/badger_bg_GiGL_NBN_1821.csv", row.names=F)
write.csv(fox_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/fox_GiGL_NBN_1821.csv", row.names=F)
write.csv(fox_bg_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/fox_bg_GiGL_NBN_1821.csv", row.names=F)
write.csv(hedgehog_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_GiGL_NBN_1821.csv", row.names=F)
write.csv(hedgehog_bg_GiGL_NBN_1821, "../data/Sorted_GiGL_NBN_forQGIS/hedgehog_bg_GiGL_NBN_1821.csv", row.names=F)
