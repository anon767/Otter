from __future__ import with_statement
import sys, re, getopt

# Useful regex objects
branch_re = re.compile(r".*\[(.*),.*].*Job (.*) is the true branch and job (.*) is the false branch.")
curjob_re = re.compile(r".*\[(.*),.*].* :")
loc_re = re.compile(r".*\[.*,.*] (.*) :")


# Only prints the (file, line number) pair,
# and does not print if it's the same as the previous.
class CompactPrint:
    def __init__(self):
        self.lastline = None
    def write(self, line=""):
        result = loc_re.match(line)
        if result != None:
            thisline = result.group(1)
            if self.lastline != thisline:
                print(thisline)
                self.lastline = thisline


# Parse command line input
def usage(msg=""):
    if msg != "": print msg
    print "Usage: python path.py filename [--jid=<jid> | --match=<matchstr>] [--compact]"
    sys.exit(2)

def get_parents(filename):
    with open(filename) as f:
        parents = dict()
        while True:
            line = f.readline()
            if line == '': break
            line = line.rstrip()

            result = branch_re.match(line)
            if result != None:
                job_parent = int(result.group(1))
                for i in [2, 3]:
                    job_child = int(result.group(i))
                    if job_child != job_parent:
                        parents[job_child] = job_parent
        return parents

def get_matching_jid(filename, matchstr):
    with open(filename) as f:
        jid = None
        while True:
            line = f.readline()
            if line == '': break
            if line.find(matchstr) >= 0:
                result = curjob_re.match(line)
                if result != None:
                    jid = int(result.group(1))
                    break
        return jid

def get_jids(parents, jid):
    jids = set([jid])
    while True:
        if jid not in parents: break
        jid = parents[jid]
        jids.add(jid)
    return jids

def output(filename, jids, write):
    with open(filename) as f:
        while True:
            line = f.readline()
            if line == '': break
            line = line.rstrip()

            result = curjob_re.match(line)
            if result == None:
                write(line)  # TODO: distinguish lines not related to jobs
            else:
                jid = int(result.group(1))
                if jid in jids:
                    write(line)
                # remove expired jid from jids
                # e.g., if job 0 spawns jobs 0 and 1, and 1 is in jobs, it means (child) job 0 is not longer interesting to us.
                # Notice: this assumes job x always forks to job x and job x+i.
                result = branch_re.match(line)
                if result != None:
                    jid_child = max(int(result.group(2)), int(result.group(3)))
                    if jid_child in jids:
                        jids.remove(jid)

def parse_argv():
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "", ["jid=", "match=", "compact"])
    except getopt.GetoptError, err:
        usage(str(err))

    filename = args[0]
    jid = None
    matchstr = None
    compact = False
    for o, a in opts:
        if o == "--compact":
            compact = True
        elif o == "--jid":
            jid = int(a)
        elif o == "--match":
            matchstr = a
        else:
            usage("unhandled option: %s=%s" % (o, a))
    if len(args) != 1: usage()
    if (jid==None) == (matchstr==None): usage()
    return filename, jid, matchstr, compact

# Main routine
filename, jid, matchstr, compact = parse_argv()
parents = get_parents(filename)
if matchstr!=None:
    jid = get_matching_jid(filename, matchstr)
jids = get_jids(parents, jid)
output(filename, jids,
       CompactPrint().write if compact else lambda line: sys.stdout.write(line+"\n"))
