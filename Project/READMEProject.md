# CMEE MRes Project
#### Predicting habitat suitability for urban mammals in Greater London using citizen science data


This repository contains codes and data for the Computational Methods in Ecology and Evolution (CMEE) MRes Project - Predicting habitat suitability for urban mammals in Greater London using citizen science data.
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
This project contains six folders: Proposal, Thesis, code, data, results, sandbox; and a README file.

Proposal stores all pdfs, documents and tex files necessary for compiling the proposal in a latex format. Thesis stores the final version of the thesis in pdf format. Code stores all the codes written for MRes project. Data stores all the data needed to run the scripts stored in code. Results store the output of coding scripts. Sandbox is used for experimental purposes and is left empty.

###### Files in code:
This folder contains two subfolders: **CitiSci_modelling_codes** and **sort_data_codes**
* **CitiSci_modelling_codes** - 2 subfolders
 * **AllYear** contains 3 sets of scripts for selecting buffer scales, model fitting and graph plotting. Each set of script contains 3 scripts and is specific for each target mammal (hedgehogs `HOG`, foxes `FOX` and badgers `BDG`). The `evaluation_3` and `plotting_4` scripts in this folder are specific for modelling with the **inclusion of mammal presence predictors**.
   * `SDM_MAMMAL_citisci_scale_optimize_2_Uva.R` selecting the best buffer scale for each environmental predictor
   * `SDM_MAMMAL_citisci_model_evaluation_3_Uva.R` evaluating the best fitting model and the predictors to be included
   * `SDM_MAMMAL_citisci_map_graph_plotting_4_Uva.R` generating maps and graphs using the best fitting model

 * **AllYear_XMammalPred** also contains three sets of R scripts for model fitting and graph plotting. Each set of script contains 2 scripts and is specific for each target mammal (hedgehogs `HOG`, foxes `FOX` and badgers `BDG`). The `evaluation_3` and `plotting_4` scripts in this folder are specific for modelling **without mammal presence predictors**. This folder also contains two additional scripts for plotting the relationship between distance from city centre and habitat suitability score.
   * `SDM_MAMMAL_citisci_model_evaluation_3_Uva.R` evaluating the best fitting model and the predictors to be included
   * `SDM_MAMMAL_citisci_map_graph_plotting_4_Uva.R` generating maps and graphs using the best fitting model
   * `SDM_XMammalPred_prob_distance_plot_5.R` plotting the relationship between distance from city centre and habitat suitability score
   * `habitatscore_camfreq.R` plotting the relationship between habitat suitability score and camera trapping rate



* **sort_data_codes** contains 3 R scripts for sorting GiGL and NBN citizen science mammal data
   * `sortdata_citisci_GiGL_1a.R` filter GiGL mammal data
   * `sortdata_citisci_NBN_1a.R` filter NBN mammal data
   * `sortdata_citisci_combine_GiGLNBN_1b.R` combine the filtered GiGL and NBN data

###### Files in data:

Data for the project is not uploaded due to confidential agreement with GiGL


*****************
#### Author name and contact:
Uva Fung uf21@imperial.ac.uk
