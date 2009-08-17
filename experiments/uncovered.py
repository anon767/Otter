import sys,os,cgi
from collections import defaultdict

if len(sys.argv)<3:
	print '''Usage: python uncovered.py diff_file path1 [path2 [...]]
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
	
path = sys.argv[2:]
file = open(sys.argv[1],"r")

for p in path:
	print p

for line in file:
	(filename,str_linenum) = line.split(":")
	linenum = int(str_linenum)
	if filename not in map:
		txt2html(filename)
	map[filename][linenum] = "<span style=\"background-color: #ffff00\">"+map[filename][linenum][:-1]+"</span>\n"
	count[filename] += 1

file.close()

try:
	os.mkdir("html")
except OSError:
	pass

index = open("html/index.html","w")
print >>index, "<html>"
print >>index, "<body>"
print >>index, "<table>"
print >>index, "<tr><td>File<td>Uncovered<td>Total&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<td>Ratio"

for (filename,lines) in sorted(map.items(),key=lambda (f,l): -count[f]):
	file = open("html/"+filename+".html","w")
	print >>file, "<html>"
	print >>file, "<body>"
	print >>file, "<pre>"
	for line in lines:
		if line=="": continue
		print >> file, line,
	print >>file, "</pre>"
	print >>file, "</body>"
	print >>file, "</html>"
	file.close()
	print >>index, ("<tr><td><a href=\"./%s.html\">%s</a>" % (filename,filename)),
	print >>index, ("<td>%d<td>%d<td>%.2f" % (count[filename],len(lines),float(count[filename])/len(lines))) 

print >>index, "</table>"
print >>index, "</body>"
print >>index, "</html>"
index.close()
