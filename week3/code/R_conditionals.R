# Author: Uva Fung
# Date: Oct 19 2021
# Description: R conditionals script

rm=(list=ls())

# Check if integer is even
is.even <- function(n=2){
    if (n %% 2 == 0)
    {
        return(paste(n, 'is even!'))
    }
    return(paste(n, 'is odd!'))
}

is.even(6)


# Check if a number is the power of 2
is.power2 <- function(n = 2){
    if (log2(n) %% 1==0)
    {
        return(paste(n, 'is a power of 2!'))
    }
    return(paste(n, 'is not a power of 2!'))
}

is.power2(4)


# Check if a number is prime
is.prime <- function(n){
    if (n == 0){
        return(paste(n, 'is a zero!'))
    }
    if (n == 1){
        return(paste(n, 'is just a unit!'))
    }
    ints <- 2:(n-1)
    if (all(n%%ints!=0)){
        return(paste(n, 'is a prime!'))
    }
    return(paste(n, "is a composite!"))
    }

is.prime(3)

# print when run with source() to show that script is working
print(is.even(6))
print(is.power2(4))
print(is.prime(3))

