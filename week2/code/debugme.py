#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: debugme.py
# Desc: debugging
# Date: Oct 15 2021

"""testing debugging in python"""

def buggyfunc(x):
    """testing debugging via mathematical calculations"""
    y = x
    for i in range(x):
        try:
            y = y-1
            z = x/y
        except ZeroDivisionError:
            print(f"The result of fividing a number by zero is undefined")
        except:
            print(f"This didn't work; x = {x}; y = {y}")
        else:
            print(f"OK; x = {x}; y = {y}, z = {z};")
    return z

buggyfunc(20)

