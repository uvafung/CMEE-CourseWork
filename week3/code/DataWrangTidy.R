################## Wrangling the Pound Hill Dataset ############
MyData1 <- as.matrix(read.csv("../data/PoundHillData.csv", header = FALSE)) # treat MyData1 as matrix
MyMetaData1 <- read.csv("../data/PoundHillMetaData.csv", header = TRUE, sep = ";")

install.packages("tidyverse") #install packages
require(tidyverse)

MyData1 <- tibble::as_tibble(data.frame(t(MyData1), stringsAsFactors = F)) # convert into tibble and transpose it
glimpse(MyData1) # shows first few rows of MyData matrix
view(MyData1)
dim(MyData1) # shows dimesions of MyData matrix


MyData1[MyData1 == ""] = 0 # replace absent species with 0
view(MyData1) # confirm that "space"" is replaced by 0


TempData1 <- as_tibble(MyData1[-1,],stringsAsFactors = F) #convert to tibble, does not include 1st row
view(TempData1)

MyWrangledData1 <- pivot_longer(TempData1, cols=5:45, names_to="Species", values_to="Count")
MyWrangledData1
view(MyWrangledData1)
colnames(MyWrangledData1) <- MyData1[1,1:4]
colnames(MyWrangledData1)[5] <- "Species"
colnames(MyWrangledData1)[6] <- "Count"
MyWrangledData1

MyWrangledData1$"Cultivation" <- as.factor(MyWrangledData1$"Cultivation")
MyWrangledData1$"Block" <- as.factor(MyWrangledData1$"Block")
MyWrangledData1$"Plot" <- as.factor(MyWrangledData1$"Plot")
MyWrangledData1$"Quadrat" <- as.factor(MyWrangledData1$"Quadrat")
MyWrangledData1$"Count" <- as.integer(MyWrangledData1$"Count")

glimpse(MyWrangledData1)
view(MyWrangledData1)
dim(MyWrangledData1)

