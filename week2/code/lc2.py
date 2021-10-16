# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# create a list of months with rain <100 with conventional loops

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.

# create a list of rainfall >100 with comprehension
heavy_rain = [r for r in rainfall if r[1] > 100]
print(heavy_rain)


# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 

# create a list of months with rain <50 with comprehension
little_rain = [(r[0]) for r in rainfall if r[1] < 50]
print(little_rain)


# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 

# create a list of rainfall >100 with conventional loops
heavy_rain = []
for r in rainfall:
    if r[1] > 100:
        heavy_rain.append(r)
print(heavy_rain)

# create a list of months with rain <50 with conventional loops
little_rain = []

for r in rainfall:
    if r[1] < 50:
        little_rain.append((r[0]))
print(little_rain)


# A good example output is:
#
# Step #1:
# Months and rainfall values when the amount of rain was greater than 100mm:
# [('JAN', 111.4), ('FEB', 126.1), ('AUG', 140.2), ('NOV', 128.4), ('DEC', 142.2)]
# ... etc.

