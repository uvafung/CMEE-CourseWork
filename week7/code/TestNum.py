#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: TestNum.py
# Desc: testing numpy
# Date: Nov 15 2021

"""This script tests the various functions in numpy"""

import numpy as np   # import numpy as np so that you can run numpy using np (type faster!)

a = np.array(range(5)) # one-dimension array
a
print(type(a)) # tells you the data structure type
print(type(a[0])) # 

a = np.array(range(5), float) # specify the data type as float)
a
a.dtype # check type 

x = np.arange(5) # alternative method to get 1-D array
x

x = np.arange(5.) # directly specify float using decimal
x

x.shape

b = np.array([i for i in range(10) if i % 2 == 1]) #odd numbers between 1 and 10
b

c = b.tolist() # convert array back to list
c

mat = np.array([[0,1], [2,3]]) # create 2-D matrix
mat

mat.shape

### Indexing and accessing arrays
mat[1] # accessing whole of second row
mat[:,1] # accessing whole of second column
mat[0,0] # accessing particular elements
mat[1,0] # 2nd row, first column
mat[:,0] # accessing whole of first column
mat[0,1]
mat[0,-1] # count the column from the back
mat[-1,0]
mat[0,-2]

### Manipulating arrays
mat[0,0] = -1 # replace a single element
mat

mat[:,0] = [12,12] # replace whole of first column with 12
mat

np.append(mat, [[12,12]], axis = 0) # add another row (axis = 0 for row)
np.append(mat, [[12],[12]], axis = 1) # add another column (axis = 1 for column)
newRow = [[12,12]] # create a new row and store
mat = np.append(mat, newRow, axis = 0) # append existing row
np.delete(mat, 2, 0) # delete third row

mat = np.array([[0, 1], [2, 3]]) # concatenation
mat0 = np.array([[0, 10], [-1, 3]])
np.concatenate((mat, mat0), axis = 0)

### Flattening or reshaping arrays
mat.ravel() # flatten array
mat.reshape((4,1)) # change array into four rows and one column
mat.reshape((1, 4)) # change array into one row and four columns


### Preallocating arrays
np.ones((4,2)) #initialzie array with ones, (4,2) are the (row,col) array dimensions
np.zeros((4,2)) # initialize array with zeros
m = np.identity(4) # create identity matrix
m.fill(16) # fill the matrix with 16
m

### Matrix-vector operations
mm = np.arange(16)
mm = mm.reshape(4,4) # convert to matrix
mm 
mm.transpose() # transpose a matrix
mm + mm.transpose() # perform addition
mm - mm.transpose() # perform subtraction
mm * mm.transpose() # element-wise multiplication
mm // mm.transpose() # integer division
mm // (mm + 1).transpose()
mm * np.pi # mm multiply by pi
mm.dot(mm) # produce dot product
mm = np.matrix(mm) # convert to numpy matrix class
print(type(mm)) # print structure type
mm * mm # instead of mm.dot(mm)