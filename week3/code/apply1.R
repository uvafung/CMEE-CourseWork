# *apply in R -- in-built functions
# Date: Oct 20 2021

## Build a random matrix
M <- matrix(rnorm(100), 10, 10)

## Take the mean of each row
RowMeans <- apply(M, 1, mean)
print(RowMeans)

## Take variance of each row
RowVars <- apply(M, 1, var)
print(RowVars)

## Take mean of each column
ColMeans <- apply(M, 2, mean)
print(ColMeans)



