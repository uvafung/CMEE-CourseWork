#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: profileme.py
# Desc: profiling code to speed up computational process
# Date: Nov 16 2021

"""This script profile codes to speed up computational process"""

def my_squares(iters):
    """give a list of squared values"""
    out = []
    for i in range(iters):
        out.append(i ** 2)
    return out

def my_join(iters, string):
    """add a string separated by commas and spaces and repeat for a given number of times"""
    out = ''
    for i in range(iters):
        out += string.join(", ")
    return out

def run_my_funcs(x,y):
    """print the input values and outputs for functions my_square and my_join"""
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")