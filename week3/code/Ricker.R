# Author: Uva Fung
# Date: Oct 21 2021
# Description: Function modelling Ricker model in R

rm=(list=ls())

Ricker <- function(N0=1, r=1, K=10, generations=50)
{
    # Runs a simulation of the Ricker model
    # Returns a vector of length generations

    N <- rep(NA, generations) # Creates a vector of NA

    N[1] <- N0
    for (t in 2:generations)
    {
        N[t] <- N[t-1] * exp(r*(1.0-(N[t-1]/K))) # Ricker model equation
    }
    return (N)
}

plot(Ricker(generations=10), type="l")


print("Script completes!")   # print when run with source() to show that script is working
