#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: TestR.py.py
# Desc: Test the process of running R scripts from Python
# Date: Nov 17 2021

"""Test the process of running R scripts from Python"""

import subprocess
subprocess.Popen("Rscript --verbose TestR.R > ../results/TestR.Rout 2> ../results/TestR_errFile.Rout", shell=True).wait() 
# error output being redirected to a new file

