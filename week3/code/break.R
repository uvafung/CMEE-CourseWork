# Author: Uva Fung
# Date: Oct 19 2021
# Description: breaking out of loops in R

rm=(list=ls())

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

print("Script completes!")   # print when run with source() to show that script is working
