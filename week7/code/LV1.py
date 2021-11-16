#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: LV1.py
# Desc: Consumer Resource Dynamics Figures
# Date: Nov 16 2021

"""This script produces consumer resource dynamics figures"""

### import necessary packages
import numpy as np
import scipy as sc
from scipy import stats # necessary for stats to run
import scipy.integrate as integrate # to calculate integration using scipy.integrate package
import matplotlib.pylab as p # to visualize the curve using matplotlib package

### Lotka-Volterra model equation
def dCR_dt(pops, t=0):
    """returns the growth rate of consumer and resource population at any given time step"""

    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return np.array([dRdt, dCdt])

type(dCR_dt) # check that the equation is written as a function

### assign parameter values
r = 1. 
a = 0.1 
z = 1.5
e = 0.75
t = np.linspace(0, 15, 1000) # define time vector (time point from 0 to 15, using 1000 sub-divisions of time)

R0 = 10 # set initial for resource population (R at time 0 = 10)
C0 = 5  # set initial for consumer population (C at time 0 = 5)
RC0 = np.array([R0, C0])

### Calculate the integration of the Lotka Volterra equation, starting from the initial conditions
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True) # integrate this system forward from the starting conditions
pops

type(infodict) # infodict is a dictionary with additional information
infodict.keys()
infodict['message'] # check whether the integration was successful

### Create figure for consumer-resource dynamics over time
f1 = p.figure() # open an empty figure

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')

f1.savefig('../results/LV_model.pdf') # save output as pdf


### Create figure for consumer density vs resource density
f2 = p.figure() # open an empty figure

p.plot(pops[:,0], pops[:,1], "r-") # plot(x-axis, y-axis, "r-" for red line)
p.grid()
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')

f2.savefig('../results/LV1_model.pdf') # save output as pdf