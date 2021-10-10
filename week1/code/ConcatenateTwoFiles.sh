#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: ConcatenateTwoFiles.sh
# Description: concatenate two files
# Date: Oct 8 2021
#
# check if input format is correct, if not send error message
if [ $# -eq 3 ] 
    then
    printf '%s\n' "Merging files into "$3" "
    else
    printf '%s\n' "Wrong input. Enter using the following format: File1 File2 NewConcatenatedFileName"
    exit 1
fi 


cat $1 > $3
cat $2 >> $3
echo "Merged files is"
cat $3

# move concatednated file into results
echo "Move concatenated file into results folder"
mv $3 ../results/

#exit
