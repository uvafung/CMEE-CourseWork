# CMEE CourseWork Week 3

This repository contains codes and data for Computational Methods in Ecology and Evolution (CMEE) week 3 coursework.
Instructions to run the codes can be found [here](https://mhasoba.github.io/TheMulQuaBio/intro.html).

Data required to run the coding scripts can be downloaded from the following [repository](https://github.com/mhasoba/TheMulQuaBio).

***

#### Languages:
R version 3.6.3

RStudio 2021.09.0

***********
#### Dependencies:
A UNIX based Operating System is needed to run the projects. Linux and Mac OS are both possible options. Linux Ubuntu can be downloaded [here](https://ubuntu.com/)


******************
#### Installation:

###### R version 3.6.3 for MacOS can be installed [here](https://cran.r-project.org/bin/macosx/)
###### R version 3.6.3 for Linux can be installed [here](https://cran.r-project.org/)
###### RStudio 2021.09.0 can be installed [here](https://www.rstudio.com/products/rstudio/download/)

***********

#### Project structure and usage:
This project contains five folders: code, data, results, sandbox, writeup; and a README file.

Code stores all the codes written for week 3 practicals. Data stores all the data needed to run the scripts stored in code. Results store the output of coding scripts. Sandbox is used for experimental purposes and is left empty. Writeup stores the report for TAutoCorr in pdf format.

###### Files in code:

    Code scripts --
        basic_io.R: illustrate R input-output
        control_flow.R: testing if statements, for loop, while loop
        break.R: break out of a loop
        next.R: skip to next iteration of a loop
        boilerplate.R: R boilerplate
        R_conditionals.R: R functions with conditionals
        TreeHeight.R: calculate tree height
        Vectorize1.R: testing vectorization
        preallocate.R: testing preallocation
        apply1.R: applying the same function to rows/columns of a matrix
        apply2.R: applying function SomeOperation to a matrix
        sample.R: sampling random numbers, generating histogram and testing vectorization and preallocation
        Ricker.R: Ricker model
        Vectorize2.R: stochastic Ricker model with quicker processing times
        browse.R: debugging with browser()
        try.R: testing try()
        Florida_warming.R: calculate the correlation coefficients for Florida temperature data
        DataWrang.R: exploring data wrangling functions
        DataWrangTidy.R: data wrangling using tidyverse
        PP_Dists.R: produce subplots for body mass distributions
        Girko.R: plotting the Girko’s law simulation
        MyBars.R: annotating bar chart
        plotLin.R: annotating a linear regression plot
        PP_Regress.R: produce regression results for Predator mass vs Prey mass
        GPDD_Data.R: mapping the Global Population Dynamics Database (GPDD) on a world map
        get_TreeHeight.R: R script that calculates tree heights for all trees and saves output as new csv (GROUPWORK)
        get_TreeHeight.py: Python script that calculates tree heights for all trees and saves output as new csv (GROUPWORK)
        run_get_TreeHeight.sh: Bash script that runs both get_TreeHeight.R and run_get_TreeHeight.sh (GROUPWORK)
        TAutoCorr.R: calculates the correlation between pairs of years to analysis temperature trends in Florida (GROUPWORK)
        PP_Regress_loc.R: calculates linear regression on subsets of the data corresponding to i) Feeding Type, ii) Predator life Stage and iii) Location (GROUPWORK)

    Latex script --
        Florida_warming.tex: generating Latex document for Florida warming data
        TAutoCorr.tex: latex file for compiling a report on Florida temperatures over years (GROUPWORK)

    Bib file --
        Floridabiblio.bib: stores the bibliography for the report (GROUPWORK)


###### Files in data:
    trees.csv
    KeyWestAnnualMeanTemperature.RData
    PoundHillData.csv
    PoundHillMetaData.csv
    EcolArchives-E089-51-D1.csv
    GPDDFiltered.RData
    Results.txt

###### Files in writeup:
    Florida_warming.pdf: report on Florida temperature change over years using correlation 
    TAutoCorr.pdf: report on Florida temperature change over years in pdf format (GROUPWORK)

*****************
#### Author name and contact:
Uva Fung uf21@imperial.ac.uk
