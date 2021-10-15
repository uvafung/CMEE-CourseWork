# CMEE CourseWork Week 1

This repository contains codes and data for Computational Methods in Ecology and Evolution (CMEE) week 1 coursework.
Instructions to run the codes can be found [here](https://mhasoba.github.io/TheMulQuaBio/intro.html).

Data required to run the coding scripts can be downloaded from the following [repository](https://github.com/mhasoba/TheMulQuaBio).

***

#### Languages:
bash version 3.2.57(1)

***********
#### Dependencies:
A UNIX based Operating System is needed to run the projects. Linux and Mac OS are both possible options. Linux Ubuntu can be downloaded [here](https://ubuntu.com/)

Other dependencies include LaTeX and Git:

LaTeX version pdfTeX 3.141592653-2.6-1.40.22

Git version 2.33.0

******************
#### Installation:

###### Install  LaTeX with the following code:

*for Linux*
```Bash
sudo apt-get install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick    
```


*for Mac*
```bash
brew install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick
```           


###### Install Git with the following code:

*for Linux*
```bash
sudo apt-get install git
```
```bash
git config --global user.name "Your Name"
```
```bash
git config --global user.email "your.login@imperial.ac.uk"
```
```bash
git config --list    
```


*for Mac*
```bash
brew install git
```
```bash
git config --global user.name "Your Name"
```
```bash
git config --global user.email "your.login@imperial.ac.uk"
```
```bash
git config --list    
```

***********

#### Project structure and usage:
This project contains four folder: code, data, results, sandbox; and a README file.

Code stores all the codes written for week 1 practicals. Data stores all the data needed to run the scripts stored in code. Results store the output of coding scripts. Sandbox is used for experimental purposes and is left empty.

###### Files in code:

    Code scripts --
        UnixPrac1.txt: count the sequence length of a genome and calculate the AT to GC ratio.
        FirstExample.tex: LaTeX document example, to be run with FirstBiblio.bib
        FirstBiblio.bib: Bibliography for LaTeX document, to be run with FirstExample.tex

    Bash scripts --
        tabtocsv.sh: convert tab delimited file to comma delimited file
        variables.sh: shows the use of variables and calculate two variables
        MyExampleScript.sh: shows the use of variables
        CountLines.sh: count lines in a file
        ConcatenateTwoFiles.sh: concatenate the contents of two files and save as a new file
        tiff2png.sh: convert tif file to png file and save as a new file
        csvtospace.sh: convert tab delimited file to comma delimited file
        CompileLaTeX.sh: compile LaTeX file and save it as a new pdf file


###### Folders and files in data:
    fasta --
        Run UnixPrac1.txt to analysis files in fasta folder

    Temperatures --
        Run csvtospace.sh to convert csv files in Temperature folder into space separated text files
*****************
#### Author name and contact:
Uva Fung uf21@imperial.ac.uk
