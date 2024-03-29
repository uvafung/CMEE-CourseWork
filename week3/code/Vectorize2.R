# Author: Uva Fung
# Date: Oct 21 2021
# Description: Runs the stochastic Ricker equation with gaussian fluctuations

rm(list = ls())

stochrick <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2, numyears = 100)
{

  N <- matrix(NA, numyears, length(p0))  #initialize empty matrix

  N[1, ] <- p0

  for (pop in 1:length(p0)) { #loop through the populations

    for (yr in 2:numyears){ #for each pop, loop through the years

      N[yr, p0] <- N[yr-1, pop] * exp(r * (1 - N[yr - 1, pop] / K) + rnorm(1, 0, sigma)) # add one fluctuation from normal distribution
    
    }
  
  }
 return(N)

}

# Now write another function called stochrickvect that vectorizes the above to
# the extent possible, with improved performance: 

# print("Vectorized Stochastic Ricker takes:")
# print(system.time(res2<-stochrickvect()))


stochrickvect <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2, numyears = 100)
{
  
  N <- matrix(NA, numyears, length(p0))  #initialize empty matrix
  
  N[1, ] <- p0
  
  for (yr in 2:numyears){ #removes the population for loop, only the year loop is kept
    
    N[yr, 1:length(p0)] <- N[yr-1, 1:length(p0)] * exp(r * (1 - N[yr - 1, 1:length(p0)] / K) + rnorm(1, 0, sigma)) # repeats population 1:length within the equation
      
  }
    

  return(N)
  
}


# compares system time for both functions
print("Stochastic Ricker takes:")
print(system.time(res2 <- stochrick()))


print("Vectorized Stochastic Ricker takes:")
print(system.time(res2 <- stochrickvect()))

print("Script completes!") 


