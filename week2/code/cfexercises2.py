# Loops and conditionals combined

"""testing for and while loop in python"""

# for j = 0-11, if j is completely divided by 3, print hello (repeat until j = 11)
for j in range(12):
    if j % 3 == 0:
        print(j)    # 0 divided by 3 is also 0!!!
        print('hello')


for j in range(15):
    if j % 5 == 3:
        print('hello')
    elif j % 4 == 3:
        print('hello')

# if z is not equal to 15, print hello until z equal to 15 (via loop)
z = 0
while z != 15:
    print('hello')
    z = z + 3

# while loop repeats itself until z < 100
z = 12
while z < 100:
    if z == 31:
        for k in range(7):
            print('hello')
    elif z == 18:
        print('hello')
    z = z + 1         # repeat this loop up until z < 100

