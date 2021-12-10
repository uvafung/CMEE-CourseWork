# CMEE 2021 HPC excercises R code main pro forma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Uva Fung"
preferred_name <- "Uva"
email <- "u.fung21@imperial.ac.uk"
username <- "uf21"


# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  x <- length(unique(community))
  return(x)
}

# Question 2
init_community_max <- function(size){
  if (size > 0) {
    x <- seq(size)
    return(x)
  }
  else{
    return(NA)
  }
}

# Question 3
init_community_min <- function(size){
  if (size > 0) {
    x <- rep(1, times = size)
    return(x)
  }
  else{
    return(NA)
  }
}

# Question 4
choose_two <- function(max_value){
  x <- sample(max_value, 2)
  return(x)
}

# Question 5
neutral_step <- function(community){
  x = choose_two(length(community))
  community[x[1]] <- community[x[2]]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  x <- length(community) # count number of community
  gen <- x / 2 
  
  if (gen %% 2 != 0){ # if x is not an even number
    y <- runif(1)
    if (y > 0.5) {
      gen <- ceiling(gen) # round up
    }
    else{
      gen <- floor(gen) # round down
    }
  }
  
  for (i in 1:gen){
    community <- neutral_step(community)
  }
  return(community)
  
}

# Question 7
neutral_time_series <- function(community,duration)  {
  time_series <- c()
  initial_richness <- species_richness(community)
  time_series <- append(time_series, initial_richness)
  
  for (i in 1:duration) {
    community <- neutral_generation(community)
    time_series <- append(time_series, species_richness(community))
  }
  return(time_series)
}

# Question 8
question_8 <- function() {
  abundance <- neutral_time_series (community = init_community_max(100), duration = 200)
  dev.off() # clear any existing graphs
  plot(abundance ~ seq(length(abundance)), xlab = "Time(Generation)", ylab = "Species Richness", cex = 0.5,cex.main = 0.6, main = "Change in species richness over time in a closed system")
  return("The system will always converge to one species only. Due to random birth and death, species richness will fluctuate and behave as in an ecological drift. Without speciation events, the total number of species will only remain the same or decrease. Over generations, the species richness will decrease to one. Since this is a closed system, there will always be a minimum of 100 individuals in the system and the minimum possible number of remaining species is one.")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
  x = choose_two(length(community))
  
  if (runif(1) < speciation_rate){
    community[x[1]] <- max(community) +1 # replace index with a number equals to max+1
    return(community)
  }
  else{
    community <- neutral_step(community)
    return(community)
  }
}


# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  x <- length(community) # count number of community
  gen <- x / 2 
  
  if (gen %% 2 != 0){ # if x is not an even number
    y <- runif(1)
    if (y > 0.5) {
      gen <- ceiling(gen) # round up
    }
    else{
      gen <- floor(gen) # round down
    }
  }
  
  for (i in 1:gen){
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  time_series <- c()
  initial_richness <- species_richness(community)
  time_series <- append(time_series, initial_richness)
  
  for (i in 1:duration) {
    community <- neutral_generation_speciation(community,speciation_rate)
    time_series <- append(time_series, species_richness(community))
  }
  return(time_series)
}

# Question 12
question_12 <- function()  {
  richness_max <- neutral_time_series_speciation(init_community_max(100), 0.1, 200)
  richness_min <- neutral_time_series_speciation(init_community_min(100), 0.1, 200)
  dev.off() # clear any existing graphs
  plot(richness_max ~ seq(length(richness_max)), xlab = "Time(Generation)", ylab = "Species Richness", cex.main = 0.6, main = "Change in species richness over time in communities with different initial conditions")  
  points(seq(length(richness_min)), richness_min, col = "red") 
  legend(50, 95, legend=c("Initial community = 100 species", "Initial community = 1 species"), col = c("black", "red"), pch = 20, cex=0.8)
  
  return("The initial conditions would only affect the number of starting species richness but not the final species richness as both communities would converge towards the equilibrium. In the neutral model, both systems undergo random speciation and extinction. These random stochastic variations would cause the species richness to fluctuate in both systems until an equilibrium is reached. In generation 200, both communities have reached an equilibirum.")
}

# Question 13
species_abundance <- function(community)  {
  df <- as.data.frame(table(community))
  x <- sort(df[,2], decreasing = TRUE)
  return(x)
}

# Question 14
octaves <- function(abundance_vector) {
  b = floor(log2(abundance_vector)) 
  bin <- b + 1  #start bin index at 1
  tabulate(c(bin))
}

# Question 15
sum_vect <- function(x, y) {
  a <- length(x)
  b <- length(y)
  
  if (a < b){
    diff1 <- b - a
    for (i in 1:diff1){
      x <- append(x, 0)
    }
    return(x+y)
  }
  else if (a > b){
    diff2 <- a - b
    for (i in 1:diff2){
      y <- append(y, 0)
    }
    return(x+y)
  }
  else{
    return(x+y)
  }
}

# Question 16 
question_16 <- function()  {
  for (i in 1:200) { # burn in period of 200 generations
    community_max_200 <- neutral_generation_speciation(init_community_max(100), 0.1)
    community_min_200 <- neutral_generation_speciation(init_community_min(100), 0.1)
  }
  
  abun_max <- species_abundance(community_max_200) # oabundance after 200 generations
  abun_min <- species_abundance(community_min_200)
  sum_max <- octaves(abun_max) # octave after 200 generations
  sum_min <- octaves(abun_min)
  
  for (i in 1:2000){ # further 2000 generations
    community_max_200 <- neutral_generation_speciation(community_max_200, 0.1) # loop and replace with new value
    community_min_200 <- neutral_generation_speciation(community_min_200, 0.1)
    
    if (i %% 20 == 0){
      abun_max_20 <- species_abundance(community_max_200) # calculate abundance vector
      abun_min_20 <- species_abundance(community_min_200)
      oct_max_20 <- octaves(abun_max_20) # calculate abundance vector
      oct_min_20 <- octaves(abun_min_20)
      
      sum_max <- sum_vect(sum_max, oct_max_20) # sum the new octave and the octave at gen 200
      sum_min <- sum_vect(sum_min, oct_min_20)
    }
    
  }
  mean_oct_max <- sum_max / 100
  mean_oct_min <- sum_min / 100
  
  max_length <- length(mean_oct_max)
  min_length <- length(mean_oct_min)
  
  # add zero to the vector if the length of both vectors don't add up -- for plotting
  if (max_length < min_length){
    diff1 <- min_length - max_length
    for (i in 1:diff1){
      mean_oct_max <- append(mean_oct_max, 0)
    }
  }
  
  if (max_length > min_length){
    diff2 <- max_length - min_length
    for (i in 1:diff2){
      mean_oct_min <- append(mean_oct_min, 0)
    }
  }
  
  df = data.frame(group = c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127"), max = mean_oct_max, min = mean_oct_min)

  plot_matrix = matrix(, nrow = 2, ncol = 7) # create empty matrix
  plot_matrix[1,] = df$max  # store max and min values in matrix
  plot_matrix[2,] = df$min
  
  barplot(plot_matrix, names.arg = df$group, beside = TRUE, col = c("skyblue", "pink"), ylim=range(pretty(c(0, plot_matrix))),
          cex.main = 0.6, main = "Species abundance distribution at equilibrium with different initial maximum and minimum number of species", 
          xlab = "Number of individuals per species", ylab = "Number of species")
  legend("topright", legend=c("Initial community with maximum 100 species", "Initial community with minimum 100 species"), 
         fill = c("skyblue", "pink"), cex=0.5, bty = "n")
  
  return("No, the initial conditions of the communities don't matter. This neutral model includes both speciation and extinction. The speciation rate is constant in both communities as it is specified with a rate of 0.1. The extinction rate would differ in both communities depending on the number of individuals per species in the community. In the initial max community, there is 100 individuals each of a different species. In the initial min community, there is 100 individuals and all are the same species. Since there is only one individual per species in the max community, the starting extinction rate would be high. As speciation causes new species to arise and the abundance per species increases, it becomes less likely for one species to go extinct, and so the extinction rate decreases until it converges with the rate of speciation. At that point the community would reach an equilibirum with a stable number of species and abundance. Similarily, in min community, the starting extinction rate is low because the death of one individual will not immediately cause one species to go extinct. As speciation causes the number of species to increase, the extinction rate increases over generations until it converges with speciation rate. At this point the community would reach an equilibirum. Regardless of the initial state of the community, both the extinction rate and speciation rate will eventually converge, leading to equilbirum within the community, hence the similarity in species distribution. ")
    
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  starting_com <- init_community_min(size)
  wall_time_sec = wall_time * 60 # convert time to sec
  gen = 0
  start_time <- proc.time()[["elapsed"]] # start timer
  list_interval_spp_abun_oct <- c() # define variables
  interval_spp_abun_oct <- c()
  interval_burnin_spp_richness <- c()
  
  while ((proc.time()[["elapsed"]] - start_time) < wall_time_sec ){
    starting_com <- neutral_generation_speciation(starting_com, speciation_rate)
    gen <- gen + 1
    print(gen) # showing how many generations have been run
    if (gen <= burn_in_generations){ # within burn-in period
      if (gen %% interval_rich == 0){ # at each interval within burn-in
        interval_burnin_spp_richness <- c(interval_burnin_spp_richness, species_richness(starting_com)) # store the series of species richness per interval
      }
    }
    if (gen %% interval_oct == 0){ # throughout the whole simulation
      spp_abun <- species_abundance(starting_com)
      interval_spp_abun_oct <- list(octaves(spp_abun))
      list_interval_spp_abun_oct <- c(interval_spp_abun_oct, list_interval_spp_abun_oct)
    }
    else{
     next
    }
  }
  end_time <- proc.time()[["elapsed"]] # stop the timer and mark end time
  time_spent <- (end_time - start_time)/60 # convert time spent from sec to min
  end_com <- starting_com # save the final community state with a new name
  
  save(interval_burnin_spp_richness, list_interval_spp_abun_oct, end_com, time_spent,
       speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations,
       file = output_file_name)
}
  

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_cluster_results <- function()  {
    # clear any existing graphs and plot your graph within the R window
    # load combined_results from your rda file
    # plot the graphs
    
    return(combined_results)
}

# Question 21
question_21 <- function()  {
    
  return("type your written answer here")
}

# Question 22
question_22 <- function()  {
    
  return("type your written answer here")
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Question 24
turtle <- function(start_position, direction, length)  {
    
  return() # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  
}

# Question 26
spiral <- function(start_position, direction, length)  {
  
  return("type your written answer here")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
}

# Question 28
tree <- function(start_position, direction, length)  {
  
}

draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Question 29
fern <- function(start_position, direction, length)  {
  
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Question 30
fern2 <- function(start_position, direction, length, dir)  {
  
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.


