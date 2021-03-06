# Author: Uva Fung
# Date: Oct 20 2021
# Description: This function calculates height of trees given distance of each tree
# from its base and angle to its top, using the trigonometric formula

# height = distance * tan(radians)

# ARGUMENTS
# degrees: the angle of elevation of tree
# distance: the distance from base of tree (eg. meters)

#OUTPUT
# the height of the tree, same units as "distance"

rm=(list=ls())

trees <- read.csv("../data/trees.csv", header = TRUE)


degrees <- trees$Angle.degrees   # save Angle.degrees column as a new variable - degrees
distance <- trees$Distance.m     # save Distance.m column as a new variable - distance

# Function to calculate tree height
TreeHeight <- function(degrees, distance){
    radians <- degrees * pi / 180
    height <- distance * tan(radians)
    print(height)
    return(height)
}

head(trees)

trees$Tree.Height.m <- TreeHeight(degrees, distance) #make new column named Tree.Height.m and store TreeHeight output in it
print(trees)
write.csv(trees, "../results/TreeHts.csv") # save as new file in results folder

print("Script completes!")   # print when run with source() to show that script is working

