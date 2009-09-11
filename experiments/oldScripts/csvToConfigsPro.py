import sys

sys.argv = sys.argv[1:]
while len(sys.argv)>0:
 lines = open(sys.argv[1]).readline()[1:].split('\r')
 vars = lines[0].split(',')
 count = 0
 for line in lines[1:]:
  print '-DWAY_%s_%d\t' % (sys.argv[0],count),
  count += 1
  for var,val in zip(vars,line.split(',')[1:]):
   if val == "T": nval = "1"
   elif val == "F": nval = "0"
   else: nval = val
   val = nval
   print '-D%s=%d' % (var,int(val)),
  print
 sys.argv = sys.argv[2:]
