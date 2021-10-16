#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 16 2021

# if no file is entered, show error message
if [[ -z "$1" ]]; then
   printf '%s\n' "No file entered"
   exit 1
fi

# if a file is entered, continue with converting tab file to csv file
echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"

# move space separated file into results
echo "Move space separated file into results folder"
mv $1.csv ../results/

#exit
