################## Wrangling the Pound Hill Dataset ############
MyData1 <- as.matrix(read.csv("../data/PoundHillData.csv", header = FALSE)) # treat MyData1 as matrix
MyMetaData1 <- read.csv("../data/PoundHillMetaData.csv", header = TRUE, sep = ";")

install.packages("tidyverse") #install packages
require(tidyverse)

MyData1 <- tibble::as_tibble(data.frame(t(MyData1), stringsAsFactors = F)) # convert into tibble and transpose it
glimpse(MyData1) # shows first few rows of MyData matrix
view(MyData1)
dim(MyData1) # shows dimesions of MyData matrix

MyData1 <- MyData1 %>%
  set_names(slice(.,1)) %>% #set row 1 as column name
  slice(-1) #remove row 1 from MyData1

MyWrangledData1 <- pivot_longer(MyData1, cols=5:45, names_to="Species", values_to="Count") # convert to wide format
view(MyWrangledData1)

MyWrangledData1 <- MyWrangledData1 %>%
  mutate_all(list(~na_if(.,""))) %>% #replace blank with NA
  mutate_all(funs(replace_na(.,0))) # replace NA with 0

view(MyWrangledData1) # check if blank has been replaced with 0


MyWrangledData1 <- MyWrangledData1 %>%
  dplyr::mutate_at(1:4, as.factor) # save columns 1:4 as factors
MyWrangledData1$"Count" <- as.integer(MyWrangledData1$"Count") # save Count as integer

str(MyWrangledData1)
glimpse(MyWrangledData1)
view(MyWrangledData1)
dim(MyWrangledData1)

