# breaking out of loops in R
# Date: Oct 19 2021

i <- 0 #Initialize i
    while(i < Inf) {
        if (i == 10) {
            break
            } # break out of the while loop
        else {
            cat("i equals ", i , "\n")
            i <- i + 1 #update i
    }
}
