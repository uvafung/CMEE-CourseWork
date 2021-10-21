# *apply in R -- own functions
# Date: Oct 20 2021

SomeOperation <- function(v){
  if (sum(v) > 0){
    return (v*100)
  }
  return(v)
}

M <- matrix(rnorm(100), 10, 10)
print(apply(M, 1, SomeOperation)) # if sum of one row is >0, multiply each value in the row by 100


