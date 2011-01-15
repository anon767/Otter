import sys,os,cgi
from collections import defaultdict

print '''Warning: Usage: python uncovered.py allLines_file diff_file output_dir path1 [path2 [...]]
'''
if len(sys.argv)<3:
	print '''Usage: python uncovered.py allLines_file diff_file output_dir path1 [path2 [...]]
'''
	sys.exit(0)
	

path = []
def open_under_path(filename,mode):
	filename = "/"+filename
	for p in path:
		if os.path.exists(p+filename):
			print (p+filename)
			return open(p+filename,mode)
	raise NameError,filename+' not found'

	

map = dict()
count = defaultdict(int)

def txt2html(filename):
	file = open_under_path(filename,"r")
	linenum = 0
	output = [""]
	
	for line in file:
		linenum += 1
		output.append(("%5d"%linenum)+ "\t"+ cgi.escape(line))
	
	file.close()
	map[filename] = output
	
output_dir = sys.argv[3]
path = sys.argv[4:]
file = open(sys.argv[2],"r")

print "Path:"
for p in path:
	print p
print ""

# Get count of *total* lines for each file
totalCounts = defaultdict(int)
for line in open(sys.argv[1]):
	(filename,str_linenum) = line.split(":")
	linenum = int(str_linenum)
	if filename not in map:
		txt2html(filename)
	map[filename][linenum] = "<font color=\"#000000\">"+map[filename][linenum][:-1]+"</font>\n"
	totalCounts[filename] += 1


for line in file:
	(filename,str_linenum) = line.split(":")
	linenum = int(str_linenum)
	if filename not in map:
		raise abc
	map[filename][linenum] = "<span style=\"background-color: #ffff00\">"+map[filename][linenum][:-1]+"</span>\n"
	count[filename] += 1

file.close()

try:
	os.mkdir(output_dir)
except OSError:
	pass

index = open(output_dir+"/index.html","w")
print >>index, "<html>"
print >>index, "<body>"
print >>index, "<table>"
print >>index, "<tr><td>File<td>Uncovered<td>Total&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<td>Ratio"

total_uncovered = 0
total_total = 0

def percentage(a,b):
	return "%.0f%%" % (float(a)*100/b)
def fix(f):
	g = f.replace("/","__SLASH__")
	return g

for (filename,lines) in sorted(map.items(),key=lambda (f,l): -count[f]):
	file = open(output_dir+"/"+fix(filename)+".html","w")
	print >>file, "<html>"
	print >>file, "<body>"
	print >>file, "<pre>"
	print >>file, "<font color=\"#BBBBBB\">"
	for line in lines:
		if line=="": continue
		print >> file, line,
	print >>file, "</font>"
	print >>file, "</pre>"
	print >>file, "</body>"
	print >>file, "</html>"
	file.close()
	print >>index, ("<tr><td><a href=\"./%s.html\">%s</a>" % (fix(filename),filename)),
	print >>index, ("<td>%d<td>%d<td>%s" % (count[filename],totalCounts[filename],percentage(count[filename],totalCounts[filename]))) 
	total_uncovered += count[filename]
	total_total += totalCounts[filename]

print >>index, ("<tr><td>Total: <td>%d<td>%d<td>%s" % (total_uncovered,total_total,percentage(total_uncovered,total_total))) 
print >>index, "</table>"
print >>index, "</body>"
print >>index, "</html>"
index.close()
