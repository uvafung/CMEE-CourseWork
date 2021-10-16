#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: csvtospace.sh
# Description: convert csv to space separated files
# Date: Oct 16 2021
#

# error message when no file is entered
if [[ -z "$1" ]]; then
   printf '%s\n' "No file entered"
   exit 1
fi 

# error message when the input format is wrong
if [ $# -eq 2 ] 
    then
    printf '%s\n' "Correct input format" 
    else
    printf '%s\n' "Wrong input format. Enter using the following format: csvfile newfilename.txt"
    exit 1
fi 

# if format is correct and a csv file is entered, create space separated file
base=`basename "$1" .csv`
if test "$base" != "$1"
then
    echo "Correct file type."

    echo "Creating a space separated version of $1 ..."
    cat $1 | tr -s "," " " >> $2
    echo "Done!"

    # move space separated file into results
    echo "Move space separated file into results folder"
    mv $2 ../results/

# if no csv file input, print error message
else
    echo "Wrong file type. CSV file needed to convert."
fi



#exit