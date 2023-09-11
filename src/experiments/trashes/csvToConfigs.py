import sys
lines = open(sys.argv[1]).readline()[1:-1].split('\r')
vars = lines[0].split(',')
for line in lines[1:]:
 for var,val in zip(vars,line.split(',')[1:]):
  print '-D%s=%d' % (var,int(val)),
 print
