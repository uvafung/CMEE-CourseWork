# Author: Uva Fung
# Date: Oct 20 2021
# Description: Vectorization in R

rm=(list=ls())

M <- matrix(runif(1000000),1000,1000) # in-built vectorized function 

SumAllElements <- function(M){     # using for loops 
    Dimensions <- dim(M)
    Tot <- 0
    for (i in 1:Dimensions[1]){
        for (j in 1:Dimensions[2]){
            Tot <- Tot + M[i,j]
        }
    }
    return(Tot)
}

print("Using loops, the time taken is:")
print(system.time(SumAllElements(M)))

print("Using the in-built vectorized function, the time taken is:")
print(system.time(sum(M)))

print("Script completes!")   # print when run with source() to show that script is working

