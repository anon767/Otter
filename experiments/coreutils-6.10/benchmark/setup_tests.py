#!/usr/bin/env python
import sys, os, re

if len(sys.argv) < 5:
    print "Usage: ./setup_tests.py <output_dir> <experiment_name> <trunk_dir> <programs_list> <commands_list> [<commands_list2>...]"
    sys.exit(1)

base = sys.argv[1]
exp_name = sys.argv[2].replace(" ", "_")
exp_base = os.path.join(base, exp_name)

trunk_dir = sys.argv[3]
programs_in = sys.argv[4]
options_in = sys.argv[5]

def add_trunk(line, trunk_dir):
    p = re.compile(r"@TRUNK@")
    return p.sub(trunk_dir, line)

def not_comment(line):
    p = re.compile(r"^\s*(#|$)")
    return p.match(line) == None

def get_progname(line):
    p = re.compile(r"(.*)\.c.*")
    results = p.match(line)
    if results:
        return os.path.basename(results.group(1))
    else:
        return None

def mkdir_p(path, mode=0755):
    if not os.path.exists(path):
        os.makedirs(path, mode)

mkdir_p(exp_base)
config = os.path.join(exp_base, "config.log")
f_config = open(config, "w")

for prog_opt in open(programs_in):
    prog_opt = prog_opt.rstrip()
    if not_comment(prog_opt):
        print "Process %s" % prog_opt
    else:
        continue
    prog_opt = add_trunk(prog_opt, trunk_dir)
    prog_name = get_progname(prog_opt)
    options_id = 1
    for options in open(options_in):
        options = options.rstrip()
        if not_comment(options):
            print "Process %s" % options
        else:
            continue
        options = add_trunk(options, trunk_dir)
        log_file = os.path.join(exp_base, "results", "%s_%d.log" % (prog_name, options_id))
        test_sh = os.path.join(exp_base, "tests", "%s_%d.sh" % (prog_name, options_id))
        mkdir_p(os.path.dirname(test_sh))

        f_test_sh = open(test_sh, "w")
        print >> f_test_sh,'mkdir -p "%s"' % os.path.dirname(log_file)
        print >> f_test_sh,'echo Command: %s %s >> "%s"' % (options, prog_opt, log_file)
        print >> f_test_sh,'%s %s 2>&1 | timelines >> "%s"' % (options, prog_opt, log_file)
        print >> f_config,'%s_%d.sh: %s %s' % (prog_name, options_id, options, prog_opt)
        f_test_sh.close()

        options_id += 1

f_config.close()

at_sh = os.path.join(exp_base, "at.sh")
f_at_sh = open(at_sh, "w")
print >> f_at_sh,'find "%s"-type f | xargs -P 15 -n 1 sh" >> "%s"' % (os.path.join(exp_base, "tests"), at_sh)

