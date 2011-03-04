import sys,re

if len(sys.argv) != 3:
    print '''Usage: python diff_line_coverage.py file1 file2
  Print the lines covered by file1 but not by file2, by file2 but not by file1, and by both'''
    sys.exit(1)

covered_line_re = re.compile(r"\[.*] (.*):(.*)")

def find_covered_lines(file):
    covered_lines = set()
    with open(file) as f:
        while 1:
            line = f.next()
            if line == 'The lines covered were:\n':
                line = f.next()
                while line[0] == '[':
                    match = covered_line_re.match(line)
                    covered_lines.add((match.group(1), int(match.group(2))))
                    line = f.next()
                return covered_lines

def print_set(s):
    for x in sorted(s): print x

a_lines = find_covered_lines(sys.argv[1])
b_lines = find_covered_lines(sys.argv[2])

a_minus_b = a_lines - b_lines
print '%d lines covered by %s but not by %s:' % (len(a_minus_b), sys.argv[1], sys.argv[2])
print_set(a_minus_b)
print

b_minus_a = b_lines - a_lines
print '%d lines covered by %s but not by %s:' % (len(b_minus_a), sys.argv[2], sys.argv[1])
print_set(b_minus_a)
print

a_inter_b = a_lines & b_lines
print '%d lines covered by both:' % len(a_inter_b)
print_set(a_inter_b)
