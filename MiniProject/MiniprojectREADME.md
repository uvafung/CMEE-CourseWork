# CMEE MiniProject

This repository contains codes and data for Computational Methods in Ecology and Evolution (CMEE) MiniProject.


Data required to run the coding scripts can be downloaded from the following [repository](https://github.com/mhasoba/TheMulQuaBio).

***

#### Languages:
R version 3.6.3
bash version 3.2.57(1)

#### Interactive interface:
RStudio 2021.09.0

***********
#### Dependencies and special packages:
A UNIX based Operating System is needed to run the projects. Linux and Mac OS are both possible options. Linux Ubuntu can be downloaded [here](https://ubuntu.com/)

Latex version pdfTex 3.141592653-2.6-1.40.22



******************
#### Installation:

###### R version 3.6.3 for MacOS can be installed [here](https://cran.r-project.org/bin/macosx/)
###### R version 3.6.3 for Linux can be installed [here](https://cran.r-project.org/)
###### RStudio 2021.09.0 can be installed [here](https://www.rstudio.com/products/rstudio/download/)

###### Install  LaTeX with the following code:

*for Linux*
```Bash
sudo apt-get install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick    
```


*for Mac*
```bash
brew install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick
```           

***********

#### Project structure and usage:
This project contains four folders: code, data, results, sandbox; and a README file.

Code stores all the codes written for the MiniProject. Data stores all the data needed to run the scripts stored in code. Results store the output of coding scripts. Sandbox stores scripts used for experimental purposes.

###### Files in code:

    R scripts (listed in the order of running) --
        DataPrep.R: Data wrangling to filter good quality datasets for analysis.
        ModFitting: Model fitting and returns analysis output csv files to be stored in data.
        ModAssumpCheck.R: Check that the assumptions of model fitting are met.
        SearchBestMod.R: Search for the best model based on AIC scores.
        ModPlotting_para: Plot model fitting results of selected datasets.

    latex file -- 
        Report.tex: contains the report written in latex
        
    bash script --
        CompileLatex.sh: compile the report in pdf format from latex
        RunAnalysis.sh: run all the R scripts for analysis and the CompileLatex.sh to produce the final report
    
    bibTex file:
    Report.bib: stores the bibliography used in the report



###### Files in data:
    LogisticGrowthData.csv: contains all the datasets
    LogisticGrowthMetaData.csv: description for each column of the datasets


###### Troubleshoot!!!
    If you encounter a prompt in the terminal while compiling the latex report, type in quit() then enter. The sysytem will require you to repeat this step for three times. Afterwards the report in pdf format should be generated in the code directory.

*****************
#### Author name and contact:
Uva Fung uf21@imperial.ac.uk
