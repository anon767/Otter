#!/usr/bin/env python
import sys, os, re, itertools, string, random

if len(sys.argv) < 5:
    print "Usage: ./setup_tests.py <output_dir> <experiment_name> <trunk_dir> <programs_list> <commands_list> [<commands_list2>...]"
    sys.exit(1)

base = sys.argv[1]
exp_name = sys.argv[2].replace(" ", "_")
exp_base = os.path.join(base, exp_name)

trunk_dir = sys.argv[3]
min_seed = int(sys.argv[4])
max_seed = int(sys.argv[5])
programs_in = sys.argv[6]
options_ins = sys.argv[7:]

def add_trunk(line, trunk_dir):
    p = re.compile(r"@TRUNK@")
    return p.sub(trunk_dir, line)

def not_comment(line):
    p = re.compile(r"^\s*(#|$)")
    return p.match(line) == None

def get_prog_name_opt(line):
    p = re.compile(r"\s*(\S*)\s*(.*)")
    results = p.match(line)
    if results:
        return results.group(1), results.group(2)
    else:
        return None

def mkdir_p(path, mode=0755):
    if not os.path.exists(path):
        os.makedirs(path, mode)

def get_seed_option(seed):
    return "--random-seed=%d" % seed

mkdir_p(exp_base)
config = os.path.join(exp_base, "config.log")
f_config = open(config, "w")
at_sh = os.path.join(exp_base, "at.sh")
f_at_sh = open(at_sh, "w")

for seed in range(min_seed, max_seed+1):

    seed_option = get_seed_option(seed)

    options_list = [ "" ]
    for options_in in options_ins:
        options = filter(lambda line: not_comment(line), [line.rstrip() for line in open(options_in).readlines()])
        options_list = [string.join(x) for x in itertools.product(options_list, options)]
    options_list = [ x + " " + seed_option for x in options_list ]

    test_sh_list = []
    for prog_opt in open(programs_in):
        prog_opt = prog_opt.rstrip()
        if not_comment(prog_opt):
            print "Process %s" % prog_opt
        else:
            continue
        prog_opt = add_trunk(prog_opt, trunk_dir)
        prog_name, prog_opt = get_prog_name_opt(prog_opt)
        options_id = 1

        for options in options_list:
            print "Process %s" % options
            options = add_trunk(options, trunk_dir)
            log_file = os.path.join(exp_base, "results", str(seed), "%s_%d.log" % (prog_name, options_id))
            test_sh = os.path.join(exp_base, "tests", str(seed), "%s_%d.sh" % (prog_name, options_id))
            test_sh_list.append(test_sh)
            mkdir_p(os.path.dirname(test_sh))

            f_test_sh = open(test_sh, "w")
            print >> f_test_sh,'mkdir -p "%s"' % os.path.dirname(log_file)
            print >> f_test_sh,'echo Command: %s %s >> "%s"' % (options, prog_opt, log_file)
            print >> f_test_sh,'%s %s 2>&1 | timelines >> "%s"' % (options, prog_opt, log_file)
            print >> f_config,'%s_%d.sh: %s %s' % (prog_name, options_id, options, prog_opt)
            f_test_sh.close()

            options_id += 1

    test_order = os.path.join(exp_base, "tests", str(seed), "ordering.txt")
    f_test_order = open(test_order, "w")
    # Shuffle the tests 7 times
    for i in range(0,7):
        random.shuffle(test_sh_list)
    for test_sh in test_sh_list:
        print >> f_test_order, test_sh
    f_test_order.close()
    print >> f_at_sh,'cat "%s" | xargs -P 8 -n 1 sh' % test_order

f_config.close()
f_at_sh.close()
