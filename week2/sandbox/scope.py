# Experimenting with variable scopes

# 1
_a_global = 10 # a global variable
if _a_global >= 5:
    _b_global = _a_global + 5 # also a global variable

def a_function():
    _a_global = 5 # a local variable

    if _a_global >= 5:
        _b_global = _a_global + 5 # also a local variable

    _a_local = 4

    print("Inside the function, the value of _a_global is ", _a_global)
    print("Inside the function, the value of _b_global is ", _b_global)
    print("Inside the function, the value of _a_local is ", _a_local)

    return None

a_function()

print("Outside the function, the value of _a_global is ", _a_global)
print("Outside the function, the value of _b_global is ", _b_global)
###############################

# 2
_a_global = 10

def a_function():
    _a_local = 4

    print("Inside the function, the value _a_local is ", _a_local)
    print("Inside the function, the value _a_local is ", _a_global)

    return None

a_function()

print("Outside the function, the value of _a_global is", _a_global)
##############################


# 3
_a_global = 10

print("Outside the function, the value of _a_global is", _a_global)


def a_function():
    global _a_global
    _a_global = 5
    _a_local = 4

    print("Inside the function, the value _a_local is ", _a_local)
    print("Inside the function, the value of _a_global is ", _a_global)

    return None

a_function()


print("Outside the function, the value of _a_global is", _a_global)
########################


# 4
def a_function():
    _a_global = 10

    def _a_function2():
        global _a_global
        _a_global = 20
    
    print("Before calling a_function, value of _a_global is ", _a_global)

    _a_function2()
    
    print("After calling _a_function2, value of _a_global is ", _a_global)
    
    return None

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)
#####################

# 5
_a_global = 10

def a_function():

    def a_function2():
        global _a_global
        _a_global = 20

    print("Before calling a_function, value of _a_global is ", _a_global)

    a_function2()

    print("After calling _a_function2, value of _a_global is ", _a_global)

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)
#######################


# 6
# define a function modify_list_1
def modify_list_1(some_list):
    print('got', some_list)
    some_list = [1, 2, 3, 4]
    print('set to', some_list)

my_list = [1, 2, 3]

print('before, my_list =', my_list)

modify_list_1(my_list)

print('after, my_list =', my_list)
#######################

# 7
def modify_list_2(some_list):
    print('got', some_list)
    some_list = [1,2 ,3, 4]
    print('set to', some_list)
    return some_list

my_list = modify_list_2(my_list)

print('after, my_list =', my_list)
###########################

# 8
def modify_list_3(some_list):
    print('got', some_list)
    some_list.append(4) # an actual modification of the list
    print('changed to', some_list)


my_list = [1, 2, 3]

print('before, my_list =', my_list)

modify_list_3(my_list)

print('after, my_list =', my_list)




