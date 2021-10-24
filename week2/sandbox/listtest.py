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