#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: CountLines.sh
# Description: count lines
# Date: Oct 2021
#
NumLines=`wc -l < $1`
echo "The file $1 has $NumLines lines"
echo
#exit
