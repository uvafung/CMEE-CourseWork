"""Experimenting with variable scopes"""

# 1
_a_global = 10 # a global variable
if _a_global >= 5:
    _b_global = _a_global + 5 # also a global variable

print("Before calling a_function, outside the function, the value of _a_global is", _a_global)
print("Before calling a_function, outside the function, the value of _b_global is", _b_global)

def a_function():
    """tesing global and local variables"""
    _a_global = 4 # a local variable

    if _a_global >= 4:
        _b_global = _a_global + 5 # also a local variable

    _a_local = 3

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
    """tesing global and local variables"""
    _a_local = 4

    print("Inside the function, the value _a_local is ", _a_local)
    print("Inside the function, the value _a_local is ", _a_global)

    return None

a_function()

print("Outside the function, the value of _a_global is", _a_global)
##############################


# 3
_a_global = 10

print("Before calling a_function, the value of _a_global is", _a_global)


def a_function():
    """tesing global and local variables"""
    global _a_global
    _a_global = 5
    _a_local = 4

    print("Inside the function, the value _a_global is ", _a_global)
    print("Inside the function, the value of _a_local is ", _a_local)

    return None

a_function()


print("After calling a_function, the value of _a_global now is", _a_global)
########################


# 4
def a_function():
    """tesing global and local variables"""
    _a_global = 10

    def _a_function2():
        """tesing global and local variables"""
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
    """tesing global and local variables"""

    def a_function2():
        """tesing global and local variables"""
        global _a_global
        _a_global = 20

    print("Before calling a_function, value of _a_global is ", _a_global)

    a_function2()

    print("After calling _a_function2, value of _a_global is ", _a_global)

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)
#######################



