#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: CompileLaTeX.sh
# Description: compile LaTeX
# Date: Oct 2021
#
Var=$(basename "$1" | cut -d. -f1)          # trim off everything behind the . and save it as Var
echo "$Var"

pdflatex $Var.tex 
bibtex $Var
pdflatex $Var.tex 
pdflatex $Var.tex 
evince $Var.pdf & 
mv $Var.pdf ../results

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg
#exit
