#!/bin/bash
# Author: Uva Fung uf21imperial.ac.uk
# Script: variable.sh
# Description: show and substitute variables, sum of two numbers
# Date: Oct 2021
#
#Show the use of variables
MyVar='some string'
echo 'the current value of the variable is' $MyVar
echo 'Please enter a new string'
read MyVar
echo 'the current value of the variable is' $MyVar
#
## Reading multiple values
echo 'Enter two numbers separated by spaces(s)'
read a b
echo 'you entered' $a 'and' $b '. Their sum is:'
mysum=`expr $a + $b`
echo $mysum
#exit