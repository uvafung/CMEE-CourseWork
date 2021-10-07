#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: csvtospace.sh
# Description: convert csv to tsv files
# Date: Oct 2021
#
echo "Creating a space separated version of $1 ..."
cat $1 | tr -s "," " " >> $2.txt
echo "Done!"
#exit