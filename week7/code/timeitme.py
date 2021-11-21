#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: profileme2.py
# Desc: figure out the best way to do something specific as part of a larger program (eg. particular command or a loop)
# Date: Nov 16 2021

"""This script compares the speed of loops/list comprehensions/join method for strings"""

##############################################################################
# loops vs. list comprehensions: which is faster?
##############################################################################

iters = 1000000

import timeit # import timeit module

from profileme import my_squares as my_squares_loops # import functions from another python file

from profileme2 import my_squares as my_squares_lc

##############################################################################
# loops vs. the join method for strings: which is faster?
##############################################################################

mystring = "my string"

from profileme import my_join as my_join_join

from profileme2 import my_join as my_join



### A possible approach to time the function
import time
start = time.time()
my_squares_loops(iters)
print("my_squares_loops takes %f s to run." % (time.time() - start))

start = time.time()
my_squares_lc(iters)
print("my_squares_lc takes %f s to run." % (time.time() - start))