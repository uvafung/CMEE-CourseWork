#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: boilerplate.py
# Desc: simple boilerplate for python programs
# Date: Oct 13 2021

"""boilerplate for python"""

__appname__ = '[application name here]'
__author__ = 'Your Name (your email address)'
__version__ = '0.0.1'
__license__ = "License for this code/program"

## imports ##
import sys #module to interface our program with the operating system

## constants ##

## functions ##
def main(argv):          
    """ Main entry point of the program """
    print('This is a boilerplate') # Note: indented using two tabs / four spaces
    return 0
    
if __name__ == "__main__":
    """Make sure the "main" function is called from command line"""
    status = main(sys.argv)
    sys.exit(status)

