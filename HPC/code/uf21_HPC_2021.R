# CMEE 2021 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice in this instance
source("uf21_HPC_2021_main.R")
# it should take a faction of a second to source your file
# if it takes longer you're using the main file to do actual simulations
# it should be used only for defining functions that will be useful for your cluster run and which will be marked automatically

# do what you like here to test your functions (this won't be marked)
# for example
species_richness(c(1,4,4,5,1,6,1,3))
# should return 4 when you've written the function correctly for question 1

# you may also like to use this file for playing around and debugging
# but please make sure it's all tidied up by the time it's made its way into the main.R file or other files.

length(unique(c(1,4,4,5,1,6,1)))
seq(1)
init_community_max(12)

rep(3, times = 3)

init_community_min(3)

species_richness(init_community_max(8))
species_richness(init_community_max(2))
species_richness(init_community_max(1))
species_richness(init_community_max(0))

init_community_max(7)

sample(3, 2)

choose_two(100)

x = c(3, 2, 1, 0, 4, 0)
y <- choose_two(x)
y[1]
y[2]


neutral_step(c(10,5,13))


neutral_step(c(1,2,13))

?seq()

neutral_generation(c(1,2,3))

6 %% 2
runif(1)

community <- init_community_max(7)

species_richness(init_community_max(7))
time_series <- c()
time_series <- append(time_series, species_richness(community))
time_series

neutral_time_series (community = init_community_max(7) , duration = 20)

neutral_time_series (community = init_community_max(100), duration = 200)

choose_two(c(2,4,77))

abundance <- neutral_time_series(community = init_community_max(100), duration = 200)
abundance
seq(200)
length(abundance)
seq(length(abundance))

plot(abundance ~ seq(length(abundance)))

question_8()

x = choose_two(c(1,2,3))
x
x[1]
x[2]

community[x[1]] <- community[x[2]]


x = choose_two(length(community))
community[x[1]] <- community[x[2]]
return(community)

x

community = c(1,2,3)
x = choose_two(length(community))
x
community[x[1]] <- community[x[2]]
community


community = init_community_max(100)

neutral_step(c(1,2,3))

x = choose_two(length(community))
community[x[1]] <- max(community) +1
return(community)

?runif
!community
?max
max(community)+1

runif(1)

neutral_generation_speciation(c(10,5,13), 0.8)

neutral_time_series_speciation(c(10,5,13), 0.2, 20)

rm(list=ls()) # good practice in this instance
source("uf21_HPC_2021_main.R")
neutral_time_series_speciation(init_community_max(100), 0.2, 200)

neutral_generation_speciation(c(10,5,13), 0.2)

neutral_time_series_speciation(init_community_max(100), 0.1, 200)
neutral_time_series_speciation(init_community_min(100), 0.1, 200)
question_12()
neutral_time_series_speciation(init_community_min(100), 0.1, 200)

?table

length(unique(c(1,5,3,6,5,6,1)))

df <- as.data.frame(table(c(1,5,3,6,5,6,1,1)))
x <- sort(df[,2], decreasing = TRUE)
return(x)
?sort

species_abundance(c(1,5,3,6,5,6,1,1))

?log
?floor#round up
?tabulate
log(1)
log(2)
log(1)/log(2)
log(9)/log(2)
floor(3)

ceiling(log(9, base=2))
floor(log(5, base=2))

species_abundance=(c(100,64,63,5,4,3,2,2,1,1,1,1))

b = floor(log2(species_abundance)) #bin
bin <- b +1
bin


tabulate(c(bin))

octaves(c(100,64,63,5,4,3,2,2,1,1,1,1))

