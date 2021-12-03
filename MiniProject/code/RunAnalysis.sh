#!/bin/bash
# Compiles latex report for MiniProject

Rscript DataPrep.R
Rscript ModFitting.R
Rscript ModAssumppCheck.R 
Rscript SearchBestMod.R 
Rscript ModPlotting_para.R 

Report $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex
evince $1.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg