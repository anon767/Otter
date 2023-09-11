# count the number of uncovered lines for each file
import sys
from collections import defaultdict

file = open(sys.argv[1],'r')
count = defaultdict(int)
for line in file.readlines():
	count[line.split(":")[0]] += 1

for (x,y) in sorted(count.items(),key=lambda a:a[1]):
	print x,":",y
