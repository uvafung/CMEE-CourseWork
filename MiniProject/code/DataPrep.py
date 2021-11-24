# Author: Uva Fung u.fung21@imperial.ac.uk
# Date: Nov 19, 2021
# File name: DataPrep.py
# Description: This script imports data for the miniproject and prepares 
# it for model fitting.

import pandas as pd
import scipy as sc
import matplotlib.pylab as pl
import seaborn as sns # You might need to install this (e.g., pip install seaborn)
import os as os
os.chdir("~/Documents/CMEECourseWork/MiniProject/code")


data = pd.read_csv("../data/LogisticGrowthData.csv")
print("Loaded {} columns.".format(len(data.columns.values)))

