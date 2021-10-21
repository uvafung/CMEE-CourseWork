# Pre-allocation in R
# Date: Oct 20 2021

NoPreallocFun <- function(x){
    a <- vector()       # empty vector
    for (i in 1:x){
        a <- c(a,i)    #c() combines values into a vector
        print(a)
        print(object.size(a))
    }
}

system.time(NoPreallocFun(10))



PreallocFun <- function(x){
    a <- rep(NA, x) # rep() replicate NA for x times
    for (i in 1:x) {
        a[i] <- i
        print(a)
        print(object.size(a))
    }
}

system.time(PreallocFun(10))



