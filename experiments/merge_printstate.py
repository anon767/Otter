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

print "COUNT:"
for type in types:
	print type, ":"
	for (key,val) in sorted(t[type].items(),key=lambda (x,y):(len(y),x)):
		print "\t%s : %d" % (key,len(val))

print ""
print "CONTENT:"
for type in types:
	print type, ":"
	for (key,val) in sorted(t[type].items(),key=lambda (x,y):(len(y),x)):
		print "\t%s" % key
		for v in val:
			print "\t\t%s" % v

