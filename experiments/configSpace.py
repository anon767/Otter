# Given a covering array spec, calculate the size of the config space

import sys,os,re

if len(sys.argv) == 1:
	print '''Usage: python configSpace.py spec.txt
'''
	sys.exit(0)

file = open (sys.argv[1])
line = file.readline()
while not line.startswith("aggr"):
	line = file.readline()

result = 1

line = file.readline()
while not line.startswith("}"):
	#print line.split()
	n = (len(line.split())-1)
	#print n
	result *= n
	line = file.readline()

print result
