#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: vectorization.py
# Desc: vectorization Python II week -- testing the speed of vectorization
# Date: Nov 16 2021

"""This script tests the speed of vectorization vs loops"""

import numpy as np
import scipy as sp

def loop_product(a, b):
    """Calculate the entrywise product of a and b using loop"""
    N = len(a)
    c = np.zeros(N)
    for i in range(N):
        c[i] = a[i] * b[i]   
    return c


def vect_product(a, b):
    """Calculate the entrywise product of a and b using vectorization"""
    return np.multiply(a, b) # the multiply function from numpy is a vectorized implementation of the loop


### compare runtimes of loop_product and vect_product functions on large randomly-generated 1D arrays
import timeit

array_lengths = [1, 100, 10000, 1000000, 10000000]
t_loop = []
t_vect = []

for N in array_lengths:
    print("\nSet N=%d" %N) 
    a = np.random.rand(N) # randomly generate our 1D arrays (a) of length N
    b = np.random.rand(N) # randomly generate our 1D arrays (b) of length N
    
    # time loop_product 3 times and save the mean execution time
    timer = timeit.repeat('loop_product(a, b)', globals=globals().copy(), number=3)
    t_loop.append(1000 * np.mean(timer))
    print("Loop method took %d ms on average." %t_loop[-1])
    
    # time vect_product 3 times and save the mean execution time
    timer = timeit.repeat('vect_product(a, b)', globals=globals().copy(), number=3)
    t_vect.append(1000 * np.mean(timer))
    print("vectorized method took %d ms on average." %t_vect[-1])



### compare the timings on a plot
p.figure()
p.plot(array_lengths, t_loop, label="loop method")
p.plot(array_lengths, t_vect, label="vect method")
p.xlabel("Array length")
p.ylabel("Execution time (ms)")
p.legend()
p.show()


### rerun the sample but make it even bigger to simulate memory error 
N = 1000000000

a = np.random.rand(N)
b = np.random.rand(N)
c = vect_product(a, b)


for N in array_lengths:
    print("\nSet N=%d" %N) 
    a = np.random.rand(N) # randomly generate our 1D arrays (a) of length N
    b = np.random.rand(N) # randomly generate our 1D arrays (b) of length N
    
    # time loop_product 3 times and save the mean execution time
    timer = timeit.repeat('loop_product(a, b)', globals=globals().copy(), number=3)
    t_loop.append(1000 * np.mean(timer))
    print("Loop method took %d ms on average." %t_loop[-1])
    
    # time vect_product 3 times and save the mean execution time
    timer = timeit.repeat('vect_product(a, b)', globals=globals().copy(), number=3)
    t_vect.append(1000 * np.mean(timer))
    print("vectorized method took %d ms on average." %t_vect[-1])



# if no error, remove a, b, c from memory.
del a
del b
del c