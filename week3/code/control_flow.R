# Author: Uva Fung
# Date: Oct 19 2021
# Description: control flow testing for R

rm=(list=ls())

# if statements
a <- TRUE
if (a == TRUE){
    print ("a is TRUE")
    } else {
    print ("a is FALSE")
}

z <- runif(1)   # generate a uniformly distributed random number
if (z <= 0.5) {
    print ("Less than a half")
    }

# for loops with numbers
for (i in 1:10) {
    j <- i * i
    print(paste(i, " squared is", j))
}

# for loops with vectors of strings
for(species in c('Heliodoxa rubinoides', 'Boissonneaua jardini', 'Sula nebouxii')){
    print(paste('The species is', species))
}

# for loop using pre-existing vector
v1 <- c("a", "bc", "def")
for (i in v1){
    print(i)
}

# while loops perform an operation till some condition is met
i <- 0
while (i < 10){
    i <- i + 1
    print(i^2)
}


print("Script complete!")   # this line is printed when this script is run using source()

