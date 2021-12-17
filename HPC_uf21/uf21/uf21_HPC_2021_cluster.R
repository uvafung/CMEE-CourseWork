# CMEE 2021 HPC exercises R code HPC run code pro forma

rm(list=ls()) # good practice 
graphics.off()
source("uf21_HPC_2021_main.R")

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

if (iter <= 25){
  com_size = 500
} else if (iter > 25 && iter <= 50){
  com_size = 1000
} else if (iter > 50 && iter <= 75){
  com_size = 2500
} else if (iter > 75 && iter <= 100){
  com_size = 5000
}

set.seed(iter) # set number of iterations as seeds

cluster_run(speciation_rate = 0.0061575, size = com_size, wall_time = 11.5*60, interval_rich = 1,
            interval_oct = com_size/10, burn_in_generations = 8*com_size,
            output_file_name= paste("Neutral_cluster_simulation_", iter, ".rda", sep = ""))
