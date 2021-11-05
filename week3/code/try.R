# Author: Uva Fung
# Date: Oct 21 2021
# Description: catching errors in R with try()

rm(list = ls())

doit <- function(x){
    temp_x <- sample(x, replace = TRUE)   # take a random sample of elements from a dataset or a vector, with replacement
    if(length(unique(temp_x)) > 30) {   #only take mean if sample was sufficient (>30)
    print(paste("Mean of this sample was:", as.character(mean(temp_x))))
    }
    else{
        stop("Couldn't calculate mean: too few unique values!")   # stops execution of the current expression and executes an error action
    }
}


set.seed(1345) # set the seed for random number generation
popn <- rnorm(50)
hist(popn)

result <- lapply(1:15, function(i) try(doit(popn), FALSE)) # FALSE modifier for the try command suppresses any error messages and keep it running
result


result <- vector("list", 15) # Preallocate/Initialize
for (i in 1:15){
    result[[i]] <- try(doit(popn), FALSE)
}

result

print("Script completes!")   # print when run with source() to show that script is working
