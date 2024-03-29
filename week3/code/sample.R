# Author: Uva Fung
# Date: Oct 20 2021
# Description: testing lapply and sapply in R

rm=(list=ls())

####### Functions ########
## A function to take a sample of size n from a population "popn" and returns its mean
myexperiment <- function(popn,n){      #set as function
  pop_sample <- sample(popn, n, replace = FALSE) # take a random sample of elements from a dataset or a vector, without replacement
  return(mean(pop_sample)) #returns the mean of pop_sample
}

## Calculate means using a FOR loop on a vector without preallocation:
loopy_sample1 <- function(popn, n, num){
  result1 <- vector()   #Initialize empty vector of size 1
  for(i in 1:num){
    result1 <- c(result1, myexperiment(popn, n))
  }
  return(result1)
}

## To run "num" iterations of the experiment using a FOR loop on a vector with preallocation:
loopy_sample2 <- function(popn, n, num){
  result2 <- vector(,num)     #Preallocate expected size, vector -- left blank
  for(i in 1:num){
    result2[i] <- myexperiment(popn, n)
  }
  return(result2)
}

## To run "num" iterations of the experiment using a FOR loop on a list with preallocation:
loopy_sample3 <- function(popn, n, num){
  result3 <- vector("list", num) #Preallocate expected size, list -- "list" added
  for(i in 1:num){
    result3[[i]] <- myexperiment(popn, n)
  }
  return(result3)
}

## To run "num" iterations of the experiment using vectorization with lapply:
lappy_sample <- function(popn, n, num){
  result4 <- lapply(1:num, function(i) myexperiment(popn, n))
  return(result4)
}

## To run "num" iterations of the experiment using vectorization with sapply:
sapply_sample <- function(popn, n, num){
  result5 <- sapply(1:num, function(i) myexperiment(popn, n))
  return(result5)
}

set.seed(12345)
popn <- rnorm(10000) # Generate the population
hist(popn)

## Run and time the different functions
n <- 100 # sample size per experiment
num <- 10000 # No. times to rerun the experiment

print("Using loops without preallocation on a vector took:")
print(system.time(loopy_sample1(popn, n, num)))

print("Using loops with preallocation on a vector took:")
print(system.time(loopy_sample2(popn, n, num)))

print("Using loops with preallocation on a list took:")
print(system.time(loopy_sample3(popn, n, num)))

print("Using the vectorized sapply function (on a list) took:")
print(system.time(sapply_sample(popn, n, num)))

print("Using the vectorized lappy function (on a list) took:")
print(system.time(lappy_sample(popn, n, num)))

print("Script completes!")
