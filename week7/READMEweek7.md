# CMEE CourseWork Week 7

This repository contains codes and data for Computational Methods in Ecology and Evolution (CMEE) week 7 coursework.
Instructions to run the codes can be found [here](https://mhasoba.github.io/TheMulQuaBio/intro.html).

Data required to run the coding scripts can be downloaded from the following [repository](https://github.com/mhasoba/TheMulQuaBio).

***

#### Languages:
Python 3.9.7

#### Interactive interface:
Jupyter Notebook, Python kernel and R kernel

***********
#### Dependencies:
A UNIX based Operating System is needed to run the projects. Linux and Mac OS are both possible options. Linux Ubuntu can be downloaded [here](https://ubuntu.com/)


******************
#### Installation:

###### Python 3.9.7 can be installed [here](https://www.python.org/downloads/release/python-397/)

###### Jupyter Notebook interface, Python and R kernels can be installed following the instructions [here](https://jupyter.readthedocs.io/en/latest/install.html)

***********

#### Project structure and usage:
This project contains four folders: code, data, results, sandbox; and a README file.

Code stores all the codes written for week 7 practicals. Data stores all the data needed to run the scripts stored in code. Results store the output of coding scripts. Sandbox is used for experimental purposes and is left empty.

###### Files in code:

    Python scripts --
        grp_oaks_debugme.py: search for oaks in a given dataset while handling for slight typos (GROUPWORK)
        LV1.py: produce consumer resource dynamics figures
        oaks_debugme.py: runs a function that finds and returns oak species, with unit testing
        profileme.py: profiling code to speed up computational process
        profileme2.py: faster code profiling
        TestR.py: test the process of running R scripts from Python
        timeitme.py: compares the speed of loops, list comprehensions and join methods for strings

    R script --
        TestR.R: run by TestR.py to test running R scripts from Python

    Jupyter notebook --
        MyFirstJupyterNb.ipynb: running Python and R commands with Jupyter Notebook



###### Files in data:
    TestOaksData.csv
    
    *Note that new data files will be generated after running grp_oaks_debugme.py and oaks_debugme.py as instructed


*****************
#### Author name and contact:
Uva Fung uf21@imperial.ac.uk
