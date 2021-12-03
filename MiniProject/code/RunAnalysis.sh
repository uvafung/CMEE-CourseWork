#!/bin/bash
# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Dec 3 2021
# Name: RunAnalysis.sh
# Description: This bash script runs all the necessary R scripts for analysis and 
# scripts for compiling the report

# Run all the R analysis scripts
Rscript DataPrep.R 
echo "Finish data wrangling"

echo "Model fitting starts. This will take some time, so feel free to go get coffee or some snacks and come back later! While you're waiting, please don't forget to read the Readme file as it contains important information for compiling the report using terminal."
Rscript ModFitting.R 
echo "Finish model fitting"

Rscript ModAssumpCheck.R 
echo "Finish assumption checking"

Rscript SearchBestMod.R 
echo "Finish searching for best model"

Rscript ModPlotting_para.R 
echo "Finish graph plotting"

sh CompileLatex.sh 
echo "Finish compiling the report"

echo "All files have been run!"


