birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

"""list comprehensions and loops to create list of bird names and masses"""
#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

# create a list of scientific names with comprehension
sci_list = [item[0] for item in birds]
print(sci_list)

# create a list of common names with comprehension
common_list = [item[1] for item in birds]
print(common_list)

# create a list of body masses with comprehension
mass_list = [item[2] for item in birds]
print(mass_list)



# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 

# A nice example out out is:
# Step #1:
# Latin names:
# ['Passerculus sandwichensis', 'Delichon urbica', 'Junco phaeonotus', 'Junco hyemalis', 'Tachycineata bicolor']
# ... etc.
 
 # create a list of scientific names with conventional loops
sci_list = []
for item in birds:
    sci_list.append(item[0]) #split using a comma
print(sci_list)

# create a list of common names with conventional loops
common_list = []
for item in birds:
    common_list.append(item[1]) #split using a comma
print(common_list)

# create a list of body masses with conventional loops
mass_list = []
for item in birds:
    mass_list.append(item[2]) #split using a comma
print(mass_list)