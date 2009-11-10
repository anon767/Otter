import sys
from collections import defaultdict

t = dict()
types = ["Globals","Locals","Formals","Memory"]
for type in types:
	t[type] = defaultdict(set)

curtype = None
isPrintState = False

for file in sys.argv[1:]:
	for line in open(file,"r"):
		if "#BEGIN PRINTSTATE" in line:
			isPrintState = True
			continue
		elif "#END PRINTSTATE" in line:
			isPrintState = False
			continue
		elif isPrintState:
			for type in types:
				if "#"+type in line:
					curtype = type
					break
			else:
				line = line[:-1].split(":",2)[2]
				(key,val) = line.split("="if curtype!="Memory" else"->",1) 
				(key,val) = (key.strip(),val.strip())
				t[curtype][key].add(val)

for type in types:
	print type, ":"
	for (key,val) in t[type].items():
		print "%s : %d" % (key,len(val)) # or print the set

