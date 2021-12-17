#!/bin/bash​
#PBS -l walltime=12:00:00​
#PBS -l select=1:ncpus=1:mem=1gb​

module load anaconda3/personal​

cp $HOME/uf21_HPC_2021_cluster.R $TMPDIR
cp $HOME/uf21_HPC_2021_main.R $TMPDIR

echo "R is about to run"​

R --vanilla $HOME/uf21_HPC_2021_cluster.R 
mv Neutral_cluster_simulation_* $HOME​

echo "R has finished running"​

# this is a comment at the end of the file​