# Author: Uva Fung
# Date: Oct 21 2021
# Description: debugging using browser()

rm(list = ls())

Exponential <- function(N0 = 1, r = 1, generations = 10){
    # Runs a simulation of exponential growth
    # Returns a vector of length generations

    N <- rep(NA, generations)   # Creates a vector of NA

    N[1] <- N0
    for (t in 2:generations){
        N[t] <- N[t-1] * exp(r)
        browser()
    }
    return(N)
}

plot(Exponential(), type="l", main="Exponential growth")

print("Script completes!")   # print when run with source() to show that script is working

