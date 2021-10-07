#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: tiff2png.sh
# Description: convert tiff to png
# Date: Oct 2021
#
for f in *.tif;
    do 
        echo "Converting $f";
        convert "$f"  "$(basename "$f" .tif).png";
    done 
#exit
