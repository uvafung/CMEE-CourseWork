# FOR loops in Python

"""testing for loops in python"""

for i in range(5):
    print(i)

my_list = [0, 2, "geronimo!", 3.0, True, False]
for k in my_list:
    print(k)

# print each value by summing up the one before it [0, 0+1, 0+1+11, 0+1+11+111, 0+1+11+111+1111]
total = 0
summands = [0, 1, 11, 111, 1111]
for s in summands:
    total = total + s
    print(total)

# WHILE loops in Python, print out all the possible values (bc it's a loop)
z = 0
while z < 100:
    z = z + 1
    print(z)

# non-stop printing until it stops
b = True
while b:
    print("GERONIMO! infinite loop! ctrl+c to stop!")
    

