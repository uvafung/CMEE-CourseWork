#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: using_name.py
# Desc: using name for python programs
# Date: Oct 13 2021
# Filename: using_name.py

if __name__ == '__main__':
    print('This program is being run by itself')
else:
    print('I am being imported from another module')

print("This module's name is : " + __name__)


