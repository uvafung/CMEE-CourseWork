#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: tiff2png.sh
# Description: convert tiff to png
# Date: Oct 2021
#
# error message when no file is entered
if [[ -z "$1" ]]; then
   printf '%s\n' "No file entered"
   exit 1
fi 

# if tiff file is entered, correct. If not, produce error message
base=`basename "$1" .tif`
if test "$base" != "$1"
then
    echo "Correct file type."
else
    echo "Wrong file type."
fi

# convert tif to png
for base in *.tif;
    do 
        echo "Converting $base";
        convert "$base"  "$(basename "$base" .tif).png"
        NewPNG=$(basename "$base" .tif).png ;
    done 

# move png file into results
echo "Move png file into results folder"
mv $NewPNG ../results/

#exit
