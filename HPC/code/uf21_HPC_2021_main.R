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
  plot(abundance ~ seq(length(abundance)), xlab = "Time(Generation)", ylab = "Abundance")
  return("The system will always converge to one with one species only. Due to random birth and death, the change in species richness behaves as in an ecological drift. This would cause species richness to fluctuate randomly. As more abundant species will have a higher chance of production, the proportion of more abundant species will increase until it becomes the only species in the environment.")
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
  
  return("The initial conditions would affect the number of starting species richness but not the final species richness as both communities would converge towards the equilibrium. In the neutral model, both systems undergo random speciation and extinction. These random stochastic variations would cause the species richness to fluctuate in both systems until an equilibrium is reached. In generation 200, both communities have reached an equilibirum.")
}

# Question 13
species_abundance <- function(community)  {
  df <- as.data.frame(table(community))
  x <- sort(df[,2], decreasing = TRUE)
  return(x)
}

# Question 14
octaves <- function(abundance_vector) {
  b = floor(log2(abundance_vector)) #bin
  bin <- b + 1
  tabulate(c(bin))
}

# Question 15
sum_vect <- function(x, y) {
  
}

# Question 16 
question_16 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
    
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


