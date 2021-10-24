#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: cfexercise1.py
# Desc: conditional modules for python
# Date: Oct 13 2021

"""conditonal modules for python"""

__author__ = 'Uva Fung (@imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys #module to interface our program with the operating system


def foo_1(x=0):         # calculate the square root of x 

    """Calculate the squre root of x"""
    return "The square root of %d is %d" % (x, (x ** 0.5))


def foo_2(x, y):        # compare if x is greater than y

    """Compare if the value of x is greater than y"""
    if x > y:
        return "The value of %d is greater than %d" % (x, y)
    return "The value of %d is greater than %d" % (y, x)


def foo_3(x, y, z):     # tmp refers a temporary variable, swap order of x y and order of y z
    
    """If x is greater than y, swap their order; if y is greater than z, swap their order"""
    
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return "The new order is [%d, %d, %d]" % (x, y, z)


def foo_4(x):         # print + update results in a loop (range 1, x+1)
    
    """Calculate the factorial of x"""
    result = 1
    for i in range(1, x+1):
        result = result * i
    return "The factorial of %d is %d" % (x, result)


def foo_5(x):         # recursive function that calculates the factorial of x
    """Calculate the factorial of x. The factorial of x is ..."""
    if x == 1:
        return 1
    return x * foo_5(x-1)


        

def foo_6(x):         # calculate factorial of x in a different way
    """Calculate the factorial of x"""
    initial_x = x   # save the original value of x as a temporary variable
    facto = 1
    if x == 1:
        return "The factorial of 1 is 1"
    while x >= 1:
        facto = facto * x
        x = x - 1
    return "The factorial of %d is %d" % (initial_x, facto)


def main(argv):
    """test agurments"""
    #x=argv[1]
    #print(foo_5(x))
    print(foo_1(16))
    print(foo_2(1, 2))
    print(foo_2(3, 2))
    print(foo_3(3, 2, 1))
    print(foo_3(3, 1, 2))
    print(foo_4(3))
    print(foo_5(3))
    print(foo_6(3))
    return(0)

if(__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)