#!/usr/bin/env python
import sys, re, getopt

# Return the current path id indicated by the line, or None
curjob_re = re.compile(r".*\[(.*),.*].* :")
def curjob(line):
    result = curjob_re.match(line)
    if result != None:
        pid = int(result.group(1))
        return pid
    else:
        return None

# Return the children path ids indicated by the line, or None
branch_re = re.compile(r".*\[(.*),.*].*Job (.*) is the .* branch and job (.*) is the .* branch.")
fp_re = re.compile(r".*\[(.*),.*].*Function pointer can take multiple values; fork job \d* to ((?:\(job \d*,function \S*\))*)")
f_re = re.compile(r"(\(job (\d*),function (\S*)\))")
def branches(line):
    results = branch_re.match(line)
    if results != None:
        children = [int(results.group(2)), int(results.group(3))]
        return children
    results = fp_re.match(line)
    if results != None:
        part = results.group(2)
        children = []
        i = 0
        while True:
            r = f_re.match(part, i)
            if r == None:
                break
            else:
                children.append(int(r.group(2)))
                i = r.end(1)
        return children
    return None


def maketree(f):
    tree = dict()
    pid2id = dict()   # map path_id to unique id given by this script
    global_data = []
    id_counter = 0

    pid2id[0] = 0
    tree[0] = { "parent":None, "children":[], "pid":0, "data":[] }

    while True:
        line = f.readline()
        if line == '': break
        line = line.rstrip()

        pid = curjob(line)
        if pid == None:
            global_data.append(line)
        else:
            this_id = pid2id[pid]
            children = branches(line)
            if children == None:
                tree[this_id]["data"].append(line)
            else:
                for child_pid in children:
                    id_counter += 1
                    child_id = id_counter
                    pid2id[child_pid] = child_id
                    tree[this_id]["children"].append(child_id)
                    tree[child_id] = { "parent":this_id, "children":[], "pid":child_pid, "data":[] }


    return (tree, global_data)


def printtree(tree, i):
    print "Parent: ", tree[i]["parent"]
    print "Children: ", tree[i]["children"]
    print "Path id: ", tree[i]["pid"]
    print "Data: "
    for line in tree[i]["data"]:
        print "\t", line
    print

    for child_id in tree[i]["children"]:
        printtree(tree, child_id)


filename_predix = "output"
next_file_id = 0
def extract_paths(tree, ids):
    global next_file_id
    global filename_predix
    if tree[ids[-1]]["children"] == []:
        print ids
        filename = filename_predix + "-" + str(next_file_id) + ".log"
        next_file_id += 1
        f = open(filename,"w")
        print >>f, ids
        for i in ids:
            for line in tree[i]["data"]:
                print >>f, line
        f.close()
    else:
        for child_id in tree[ids[-1]]["children"]:
            extract_paths(tree, ids + [child_id])


def main():
    global filename_predix
    f = open(sys.argv[1])
    (tree, global_data) = maketree(f)
    f.close()

    filename_predix = sys.argv[2]
    extract_paths(tree, [0])


main()

