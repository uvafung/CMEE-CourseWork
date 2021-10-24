#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: test_control_flow.py
# Desc: unit testing with control flow
# Date: Oct 14 2021

"""unit testing with flow control"""


__author__ = 'Uva Fung (uf21@imperial.ac.uk)'
__version__ = '0.0.1'


## imports ##
import sys #module to interface our program with the operating system
import doctest #import the doctest module

def even_or_odd(x=0): #if not specified, x should take value 0

    """Find whether a number x is even or odd.
    
    >>> even_or_odd(10)
    '10 is Even!'

    >>> even_or_odd(5)
    '5 is Odd!'

    whenever a float is provided, then the closest integer is used:
    >>> even_or_odd(3.2)
    '3 is Odd!'

    in case of negative numbers, the positive is taken:
    >>> even_or_odd(-2)
    '-2 is Even!'

    """

    #Define function to be tested
    if x % 2 == 0:
        return "%d is Even!" % x
    return "%d is Odd!" % x


## functions ##
def main(argv):     
    """test argument"""     
    print(even_or_odd(22))
    print(even_or_odd(33))
    return 0

if __name__ == "__main__":
    status = main(sys.argv)

doctest.testmod() # to run with embedded tests



