# Author: Uva Fung
# Date: Oct 19 2021
# Description: skip to next iteration of a loop in R


for (i in 1:10){
    if ((i %% 2) == 0) # if the number is even, go to next number in the list (i)
        next
    print(i) # if odd, print i
}

print("Script completes!")   # print when run with source() to show that script is working
