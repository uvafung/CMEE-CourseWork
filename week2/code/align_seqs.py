#!/user/bin/env python3
# Author: Uva Fung uf21@imperial.ac.uk
# Script: align_seqs.py
# Desc: align seqeunces
# Date: Oct 18 2021

"""Align two seqeunces and gives the best alignment as output"""

__appname__ = 'AlignSequence'
__author__ = 'Uva Fung uf21@imperial.ac.uk'
__version__ = '0.0.1'
__license__ = "License for this code/program"

## imports ##
import sys #module to interface our program with the operating system

def main(argv):
    """ Main entry point of the program """
    print('This program aligns seqeunces') # NOTE: indented using two tabs or 4 spaces
    return 0

# import seqeunces from seq.txt
with open('../data/seq.txt','r') as f:   # open seq.txt which stores the two sequences
    line = f.readlines()         
    seq_line = []     #create a new list for storing the seqeunces
    
    for l in line:
        seq_line.append(l.strip())

seq1 = line[1] # save line 1 as seq1
seq2 = line[0] # save line 0 as seq2


# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest

l1 = len(seq1) # find length of seq1 and save it as l1
l2 = len(seq2) # find length of seq2 and save it as l2
if l1 >= l2:   # if seq1 is longer than seq2
    s1 = seq1  
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1 # swap the two lengths so that s1 is always longer than s2

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    """Calculate the matching score for seqeunces 1 and 2"""
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1: # this means that some bases still needs to be compared
            if s1[i + startpoint] == s2[i]: # if the bases match. i + startpoint because python index starts from 0
                matched = matched + "*" 
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched) # "." * startpoint = the number of . to be printed           
    print("." * startpoint + s2) # print . plus the shorter seqeuence
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)


# now try to find the best match (highest score) for the two sequences
my_best_align = None
my_best_score = -1 # use -1 because the best alignment could be 0 (no match). If we put 0 here this could cause confusion.

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 # this will align s2 with the longer s1 seqeunce
        my_best_score = z  #loop until there is a score greater than all of the other scores (top score found)

# Below shows the best alignment between the two seqeunces:
print(my_best_align) #print best alignment
print(s1) #print s1 such that it aligns best with s2
print("The best score is %d" % my_best_score)


print("%s" % my_best_align, file=open("output.txt", "a"))
print("%s" % s1, file=open("output.txt", "a"))
print("The best score is %d" % my_best_score, file=open("output.txt", "a"))

import shutil

shutil.move('output.txt', '../results/')

"""output.txt is stored in results folder"""




if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)