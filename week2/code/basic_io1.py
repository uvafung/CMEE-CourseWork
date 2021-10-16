###################
# FILE INPUT
###################
# Open a file for reading
with open('../sandbox/test.txt', 'r') as f:
# use "implicit" for loop:
# if the object is a file, python will cycle over lines
    for line in f:
        print(line)


# Same example, skip blank lines
with open('../sandbox/test.txt', 'r') as f:
    for line in f:
        if len(line.strip()) > 0:
            print(line)

