#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: variable.sh
# Description: illustrate different types of shell variables
# Date: Oct 16 2021
## Illustrate the use of variables

# Special variables

echo "This script was called with $# parameters"
echo "The script's name is $0"
echo "The arguments are $@"
echo "The first argument is $1"
echo "The second argument is $2"

# Assigned variables; explicit declaration:
MY_VAR='some string'
echo 'the current value of the variable is:' $MY_VAR
echo
echo 'Please enter a new string'
read MY_VAR
echo
echo 'the current value of the variable is:' $MY_VAR
echo

## Assigned variables; Reading (multiple values) from user input:
echo 'Enter two numbers separated by spaces(s)'
read a b
echo 'you entered' $a 'and' $b '; Their sum is:'

## Assigned variables: Command substitution
MY_SUM=$(expr $a + $b)
echo $MY_SUM
#exit