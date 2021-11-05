# Author: Uva Fung
# Date: Nov 5 2021
# Description: illustrate R input-output

rm=(list=ls())

MyData <- read.csv("../data/trees.csv", header = TRUE) # import with headers
write.csv(MyData, "../results/MyData.csv") # write it out as a new file
write.table(MyData[1,], file = "../results/MyData.csv", append = TRUE) # append to it
write.csv(MyData, "../results/MyData.csv", row.names = TRUE) # write row names
write.table(MyData, "../results/MyData.csv", col.names = FALSE) #ignore column names
print("Script complete!")
