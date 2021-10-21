# A boilerplate R script
# Date: Oct 19 2021

MyFunction <- function(Arg1, Arg2){

    #Statements involving Arg1, Arg2:
    print(paste("Argument", as.character(Arg1), "is a", class(Arg1))) # print Arg1
    print(paste("Argument", as.character(Arg2), "is a", class(Arg2))) # print Arg2

    return (c(Arg1, Arg2)) # optional, but very useful
}

MyFunction(1,2) # Test the function
MyFunction("Riki", "Tiki") # A different test

