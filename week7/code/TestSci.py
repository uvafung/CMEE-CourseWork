#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: TestSci.py
# Desc: testing scipy
# Date: Nov 15 2021

"""This script tests the various functions in scipy"""

### import necessary packages
import scipy as sc
from scipy import stats # necessary for stats to run

### random number generation
sc.stats.norm.rvs(size = 10) # generate 10 samples from normal distribution
np.random.seed(1234) # set seed to generate the same random number seqeunce everytime
sc.stats.norm.rvs(size = 10)
sc.stats.norm.rvs(size=5, random_state=1234)
sc.stats.randint.rvs(0, 10, size = 7) # generate random integers between 0 and 10
sc.stats.randint.rvs(0, 10, size = 7, random_state=1234) # with random seed
sc.stats.randint.rvs(0, 10, size = 7, random_state=3445) # a different seed

### numerical integration using scipy
import scipy.integrate as integrate

# calulcate area under a curve (differentiation)
y = np.array([5, 20, 18, 19, 18, 7, 4])
import matplotlib.pylab as p # to visualize the curve using matplotlib package
p.plot(y)
area = integrate.trapz(y, dx = 2) # compute the area using the composite trapezoidal rule
print("area =", area)
area = integrate.trapz(y, dx = 1) # changing the value of dx will change the area output
print("area =", area)
area = integrate.trapz(y, dx = 3)
print("area =", area)

area = integrate.simps(y, dx = 2) # calculate area using Simpson's rule
print("area =", area)
area = integrate.simps(y, dx = 1)
print("area =", area)
area = integrate.simps(y, dx = 3)
print("area =", area)


### Lotka-Volterra model
def dCR_dt(pops, t=0):
    """returns the growth rate of consumer and resource population at any given time step"""

    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return np.array([dRdt, dCdt])

type(dCR_dt)

r = 1. # assign parameter values
a = 0.1 
z = 1.5
e = 0.75
t = np.linspace(0, 15, 1000) # define time vector (time point from 0 to 15, using 1000 sub-divisions of time)

R0 = 10 # set initial for resource population (R at time 0 = 10)
C0 = 5  # set initial for consumer population (C at time 0 = 5)
RC0 = np.array([R0, C0])

pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True) # integrate this system forward from the starting conditions
pops

type(infodict) # infodict is a dictionary with additional information
infodict.keys()
infodict['message'] # check whether the integration was successful

f1 = p.figure() # open an empty figure

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')

p.show()# To display the figure
f1.savefig('../results/LV_model.pdf') # save output as pdf
