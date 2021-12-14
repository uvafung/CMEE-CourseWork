# CMEE 2021 HPC exercises R code HPC run code proforma


rm(list=ls()) # good practice in this instance
source("uf21_HPC_2021_main.R")

A <- c(0,0)
B <- c(3,4)
C <- c(4,1)
X <- c(0,0)
df <- as.data.frame(rbind(A, B, C), stringsAsFactors = FALSE)
names(df) <- c('x' , 'y') # store values of A/B/C in dataframe for plotting
plot(df, pch = 16, cex = 0.3)

turtle(c(0,0), pi/3, length = 1)
elbow(c(0,0), pi/3, length = 1)
spiral(c(0,0), pi/3, length = 1)
graphics.off()

draw_tree()
draw_fern()
draw_fern2()


new_position = turtle(c(2.5,0), pi/2, 0.5) # return the new position of the vector after drawing the first line

fern2(c(2.5,0), pi/2 + pi/4, 0/.5* 0.38, 1) # turn left
  
fern2(c(2.5,0), pi/2 - pi/4, 0.5 * 0.38, 1) # turn right

  
  
  