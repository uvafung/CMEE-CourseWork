# Author: Uva Fung
# Date: Nov 5 2021
# Description: Wrangling the Pound Hill Dataset 

rm(list = ls())


############# Load the dataset ###############
# header = false because the raw data don't have headers
# treat MyData as matrix
MyData <- as.matrix(read.csv("../data/PoundHillData.csv", header = FALSE))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv", header = TRUE, sep = ";")

############# Inspect the dataset ###############
head(MyData) # shows first few rows of MyData matrix
dim(MyData) # shows dimesions of MyData matrix
str(MyData) # gives list of items in the matrix
fix(MyData) # opens in RStudio Editor
fix(MyMetaData) # opens in RStudio Editor

############# Transpose ###############
# To get those species into columns and treatments into rows 
#This is where header = FALSE comes in. If header is TRUE then only the numbers will be flipped
#but not the column names -- chaos!
MyData <- t(MyData) # transpose (reversing rows and columns). 
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] = 0
head(MyData) # confirm that "space"" is replaced by 0

############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #MyData[-1,] = does not include first row, stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] # assign column names from original data (MyData)
head(TempData)

############# Convert from wide to long format  ###############
#require() returns a logical value -- returns (invisibly) TRUE if the package is available, FALSE if the package is not.
require(reshape2) # load the reshape2 package

?melt #check out the melt function

#melt() convert an object into a molten data frame
#melt() convert a data frame with several measurement columns into a data frame in this canonical format,
#which has one row for every observed (measured) value
MyWrangledData <- melt(TempData, id=c("Cultivation", "Block", "Plot", "Quadrat"), variable.name = "Species", value.name = "Count")

#You need to tell melt() which of your variables are identifying variables (id.vars), 
#and which are measured variables (measure.vars). 
#If you only supply one of id.vars or measure.vars, 
#melt() will assume the remainder of the variables in the data set belong to the other. 
#If you supply neither, melt() will assume factor and character variables are id variables, 
#and all others are measured.

MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
MyWrangledData[, "Count"] <- as.integer(MyWrangledData[, "Count"])

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)

############# Exploring the data (extend the script below)  ###############


print("Script completes!")   # print to show that script is working

