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
spiral(c(1,2), 14, length = 1)
graphics.off()

draw_spiral()
draw_tree()
draw_fern()
draw_fern2()


new_position = turtle(c(2.5,0), pi/2, 0.5) # return the new position of the vector after drawing the first line

fern2(c(2.5,0), pi/2 + pi/4, 0/.5* 0.38, 1) # turn left
  
fern2(c(2.5,0), pi/2 - pi/4, 0.5 * 0.38, 1) # turn right
#################################

simu_data <- list()
octave_all_simu <- list()
for (i in 1:100){
  file <- paste0("../../results/Neutral_cluster_simulation_", i, ".rda")
  load(file,.GlobalEnv)
  simu_data[[i]] <- list(list_interval_spp_abun_oct, burn_in_generations, end_com, 
                         interval_burnin_spp_richness, interval_oct, interval_rich, 
                         size, speciation_rate, time_spent, wall_time)
  octave_all_simu[[i]] <- simu_data[[i]][[1]] # save all burn-in generations into a list
}


###### Community size = 500
octave_com500 <- list() # create a list of octave for community size = 500
octave_sum_com500 = unlist(octave_all_simu[[1]][81]) # convert octaves from list into vectors -- extract simulation 1, octave
total_com500 <- c()

for (a in 1:25){
  max_500 <- length(octave_all_simu[[a]]) # total number of generations per simulation
  no_gen_postburnin_com500 = max_500 - 82 # number of generations after burn-in
  total_com500 <- c(total_com500, no_gen_postburnin_com500) # number of post-burn-in generations per simulation and add them up
  for (b in 82:max_500){
    octave_sum_com500 = sum_vect(octave_sum_com500, unlist(octave_all_simu[[a]][[b]])) # sum vectors -- # retrieve a-th sequence of octave_all_simu
    octave_com500[[a]] = list(octave_sum_com500) # for each simulation, store the sum of each octave per gen
  }
}
octave_com500 = octave_com500[1:25]

###### Community size = 1000
octave_com1000 <- list() # create a list of octave for community size = 1000
octave_sum_com1000 = unlist(octave_all_simu[[26]][81]) # simulation 26, octave 81
total_com1000 <- c()

for (a in 26:50){
  max_1000 <- length(octave_all_simu[[a]])
  no_gen_postburnin_com1000 = max_1000 - 82
  total_com1000 <- c(total_com1000, no_gen_postburnin_com1000)
  for (b in 82:max_1000){
    octave_sum_com1000 = sum_vect(octave_sum_com1000, unlist(octave_all_simu[[a]][[b]]))
    octave_com1000[[a]] = list(octave_sum_com1000)
  }
}
octave_com1000 = octave_com1000[26:50]

###### Community size = 2500
octave_com2500 <- list() # create a list of octave for community size = 2500
octave_sum_com2500 = unlist(octave_all_simu[[51]][81]) # simulation 51, octave gen 81
total_com2500 <- c()

for (a in 51:75){
  max_2500 <- length(octave_all_simu[[a]])
  no_gen_postburnin_com2500 = max_2500 - 82
  total_com2500 <- c(total_com2500, no_gen_postburnin_com2500)
  for (b in 82:max_2500){
    octave_sum_com2500 = sum_vect(octave_sum_com2500, unlist(octave_all_simu[[a]][[b]]))
    octave_com2500[[a]] = list(octave_sum_com2500)
  }
}
octave_com2500 = octave_com2500[51:75]

###### Community size = 5000
octave_com5000 <- list() # create a list of octave for community size = 5000
octave_sum_com5000 = unlist(octave_all_simu[[76]][81]) # simulation 76, octave gen 81
total_com5000 <- c()

for (a in 76:100){
  max_5000 <- length(octave_all_simu[[a]])
  no_gen_postburnin_com5000 = max_5000 - 82
  total_com5000 <- c(total_com5000, no_gen_postburnin_com5000)
  for (b in 82:max_5000){
    octave_sum_com5000 = sum_vect(octave_sum_com5000, unlist(octave_all_simu[[a]][[b]])) 
    octave_com5000[[a]] = list(octave_sum_com5000)
  }
}
octave_com5000 = octave_com5000[76:100]

############ sum all octaves
final_sum_com500 = unlist(octave_com500[[1]])
final_sum_com1000 = unlist(octave_com1000[[1]])
final_sum_com2500 = unlist(octave_com2500[[1]])
final_sum_com5000 = unlist(octave_com5000[[1]])

for (s in 2:25){
  final_sum_com500 = sum_vect(final_sum_com500, unlist(octave_com500[[s]]))
  final_sum_com1000 = sum_vect(final_sum_com1000, unlist(octave_com1000[[s]]))
  final_sum_com2500 = sum_vect(final_sum_com2500, unlist(octave_com2500[[s]]))
  final_sum_com5000 = sum_vect(final_sum_com5000, unlist(octave_com5000[[s]]))
}

vector_com500 = final_sum_com500 / sum(total_com500)
vector_com1000 = final_sum_com1000 / sum(total_com1000)
vector_com2500 = final_sum_com2500 / sum(total_com2500)
vector_com5000 = final_sum_com5000 / sum(total_com5000)

combined_results <- list(vector_com500, vector_com1000, vector_com2500, vector_com5000)

save(combined_results , file = "vector_all_com.rda")
combined_results <- paste0("vector_all_com.rda")
load(combined_results,.GlobalEnv)
return(combined_results)

combined_results

graphics.off() #clear any existing graphs and plot your graph within the R window
load("vector_all_com.rda")
par(mfrow = c(2, 2))

load("vector_all_com.rda")
return("vector_all_com.rda")

###### Plot community size = 500
barplot(unlist(combined_results[[1]]), 
        cex.main = 0.8,
        main = "Equilibrium state in community size = 500",
        xlab = "Number of individuals per species", ylab = "Number of species",
        names.arg = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511"))


###### Plot community size = 1000
barplot(unlist(combined_results[[2]]), 
        cex.main = 0.8,
        main = "Equilibrium state in community size = 1000",
        xlab = "Number of individuals per species", ylab = "Number of species",
        names.arg = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511", "512-1023"))

###### Plot community size = 2500
barplot(unlist(combined_results[[3]]), 
        cex.main = 0.8,
        main = "Equilibrium state in community size = 2500",
        xlab = "Number of individuals per species", ylab = "Number of species",
        names.arg = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511", "512-1023", "1024-2047"))

###### Plot community size = 5000
barplot(unlist(combined_results[[4]]), 
        cex.main = 0.8,
        main = "Equilibrium state in community with size = 5000",
        xlab = "Number of individuals per species", ylab = "Number of species",
        names.arg = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511"))



print(combined_results)







com_500 <- as.data.frame(combined_results[[1]])
com_500$octave <- c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511")


com_size <- "Community size = 500"


com_500 <- data.frame(group = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127","128-255", "256-511"))

plot_matrix = matrix(, nrow = 2, ncol = 7) # create empty matrix
plot_matrix[1,] = df$max  # store max and min values in matrix
plot_matrix[2,] = df$min

barplot(plot_matrix, names.arg = df$group, beside = TRUE, col = c("skyblue", "pink"), ylim=range(pretty(c(0, plot_matrix))),
        cex.main = 0.6, main = "Species abundance distribution at equilibrium with different initial maximum and minimum number of species", 
        xlab = "Number of individuals per species", ylab = "Number of species")
legend("topright", legend=c("Initial community with maximum 100 species", "Initial community with minimum 100 species"), 
       fill = c("skyblue", "pink"), cex=0.5, bty = "n")



  