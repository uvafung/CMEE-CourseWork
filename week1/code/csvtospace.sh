#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: csvtospace.sh
# Description: convert csv to space separated files
# Date: Oct 2021
#
# error message when no file is entered
if [[ -z "$1" ]]; then
   printf '%s\n' "No file entered"
   exit 1
fi 

# if csv file is entered, correct. If not, produce error message
base=`basename "$1" .csv`
if test "$base" != "$1"
then
    echo "Correct file type."
else
    echo "Wrong file type."
fi


echo "Creating a space separated version of $1 ..."
cat $1 | tr -s "," " " >> $2
echo "Done!"


# move space separated file into results
echo "Move space separated file into results folder"
mv $2 ../results/

#exit