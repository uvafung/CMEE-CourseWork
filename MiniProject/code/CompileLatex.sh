#!/bin/bash
# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Dec 3 2021
# Name: CompileLatex.sh
# Description: This bash script runs all necessary scripts for compiling the report


# Compiles latex report for MiniProject
pdflatex Report.tex
bibtex Report
pdflatex Report.tex
pdflatex Report.tex

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg

