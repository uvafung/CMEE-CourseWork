#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: ConcatenateTwoFiles.sh
# Description: concatenate two files
# Date: Oct 2021
#
cat $1 > $3
cat $2 >> $3
echo "Merged files is"
cat $3
#exit
