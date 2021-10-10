#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: CountLines.sh
# Description: count lines
# Date: Oct 8 2021
#
# if no file is entered show error message
if [[ -z "$1" ]]; then
   printf '%s\n' "No file entered"
   exit 1
else
   # If filename is entered show what the user typed in and run ls -l
   printf "Counting lines in %s " "$1"
   ls -l
fi
NumLines=`wc -l < $1`
echo "The file $1 has $NumLines lines"
echo
#exit
