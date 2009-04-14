#!/usr/bin/ruby

require "open3"

$cilly = "../../cil/bin/cilly"
$libcdir = "../../libc"
$mockeddir = "../../mockedFns"

config = {
  "LIST_FILES" => "0",
  "WITH_FILENAMES" => "0",
  "OUT_LINE" => "1",
  "MATCH_WORDS" => "0", #"1",
  "MATCH_LINES" => "0", #"1",
  "BINARY_FILES" => "0",
  "COUNT_MATCHES" => "0",
  "NO_FILENAMES" => "0",
  "SUPPRESS_ERRORS" => "0",
}

def compile(config, exe)
  args = ""
  config.each_pair { |k,v|
    args += " -D#{k}=#{v}"
  }
  cmd = "gcc -I. #{args} -DCONCRETE grep.c -o #{exe} > /dev/null 2>&1"
  system cmd
end

def run(exe, search, contents)
  input = "/tmp/#{Process.pid}.txt"
  File.open(input, "w") { |f|
    f.puts contents
  }
  result = nil
  IO.popen("#{exe} \"#{search}\" #{input}") { |f|
    result = f.read
  }
  File.unlink input
  return result
end

def make_mock(contents)
  s = <<END
#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

	sym_file_t* input = IOSIM_addfile("/input.txt", 0);
	input->contents = "#{contents}";
	input->stat.st_size = #{contents.length};
}
END
end

def readline_echo(fin, fout)
  result = fin.readline
  fout.puts result
  result
end

def symbolic_exec(i, config, search, contents)
  tmpfile = "/tmp/#{Process.pid}-test.c"
  combfile = "/tmp/#{Process.pid}_comb.c"
  File.open(tmpfile, "w") { |f|
    f.puts(make_mock(contents))
  }
  args = ""
  config.each_pair { |k,v|
    args += " -D#{k}=#{v}"
  }
  cmd = "CILLY_DONT_COMPILE_AFTER_MERGE=1 " +
        "#{$cilly} --merge --useLogicalOperators --out=#{combfile} " +
        "#{args} -I. -I #{$libcdir} -I #{$mockeddir} " +
        "-include #{$mockeddir}/symexe.h " +
            "#{$libcdir}/*.c #{$mockeddir}/*.c #{tmpfile} grep.c --warnall > /dev/null 2>&1"
  system cmd
  File.unlink tmpfile

  cmd = "#{$cilly} #{combfile} --useLogicalOperators --arg=\"#{search}\" " +
        "--arg=input.txt --doexecute"
  #  CILLY_DONT_COMPILE_AFTER_MERGE=1
  result = ""
  out = File.open("out/#{i}.out", "w")
  Open3.popen3(cmd) { |stdin,stdout,stderr|
    while not stdout.eof?
      if ((readline_echo(stdout, out) =~ /__COMMENT\(\(char \*\)"Writing on fildes"\);/) &&
          (readline_echo(stdout, out)) && # COMMENT:(char *)"Writing on fildes"
          (readline_echo(stdout, out)) && # #line 101
          (readline_echo(stdout, out)) && # __EVAL(fildes)
          (readline_echo(stdout, out) =~ /Evaluates to Bytearray\(\/01\/00\/00\/00\)/) &&
          (readline_echo(stdout, out)) && # #line 103
          (readline_echo(stdout, out)) && # __EVALSTR((char *_buf, (int )nbyte)
          (readline_echo(stdout, out) =~ /Evaluates to string: "(.*)"/))
        result = result + $1
      end
    end
  }

  File.unlink combfile
  out.close
  result
end

tests = [
  ["abc", "abc"],
  ["abc", "xbc"],
  ["abc", "axc"],
  ["abc", "abx"],
  ["abc", "xabcy"],
  ["abc", "ababc"],
  ["ab*c", "abc"],
  ["ab*bc", "abc"],
  ["ab*bc", "abbc"],
  ["ab*bc", "abbbbc"],
  ["ab+bc", "abbc"],
  ["ab+bc", "abc"],
  ["ab+bc", "abq"],
  ["ab+bc", "abbbbc"],
  ["ab?bc", "abbc"],
  ["ab?bc", "abc"],
  ["ab?bc", "abbbbc"],
  ["ab?c", "abc"],
  ["^abc$", "abc"],
  ["^abc$", "abcc"],
  ["^abc", "abcc"],
  ["^abc$", "aabc"],
  ["abc$", "aabc"],
  ["^", "abc"],
  ["$", "abc"],
  ["a.c", "abc"],
  ["a.c", "axc"],
  ["a.*c", "axyzc"],
  ["a.*c", "axyzd"],
  ["a[bc]d", "abc"],
  ["a[bc]d", "abd"],
  ["a[b-d]e", "abd"],
  ["a[b-d]e", "ace"],
  ["a[b-d]", "aac"],
  ["a[-b]", "a-"],
  ["a[b-]", "a-"],
  ["a[b-a]", "-"],
  ["a[]b", "-"],
  ["a[", "-"],
  ["a]", "a]"],
  ["a[]]b", "a]b"],
  ["a[^bc]d", "aed"],
  ["a[^bc]d", "abd"],
  ["a[^-b]c", "adc"],
  ["a[^-b]c", "a-c"],
  ["a[^]b]c", "a]c"],
  ["a[^]b]c", "adc"],
  ["ab|cd", "abc"],
  ["ab|cd", "abcd"],
  ["()ef", "def"],
  ["()*", "-"],
  ["*a", "-"],
  ["^*", "-"],
  ["$*", "-"],
  ["(*)b", "-"],
  ["$b", "b"],
  ["a\\", "-"],
  ["a\\(b", "a(b"],
  ["a\\(*b", "ab"],
  ["a\\(*b", "a((b"],
  ["a\\x", "a\\x"],
  ["abc)", "-"],
  ["(abc", "-"],
  ["((a))", "abc"],
  ["(a)b(c)", "abc"],
  ["a+b+c", "aabbabc"],
  ["a**", "-"],
  ["a*?", "-"],
  ["(a*)*", "-"],
  ["(a*)+", "-"],
  ["(a|)*", "-"],
  ["(a*|b)*", "-"],
  ["(a+|b)*", "ab"],
  ["(a+|b)+", "ab"],
  ["(a+|b)?", "ab"],
  ["[^ab]*", "cde"],
  ["(^)*", "-"],
  ["(ab|)*", "-"],
  [")(", "-"],
  ["abc", ""],
  ["abc", ""],
  ["a*", ""],
  ["([abc])*d", "abbbcd"],
  ["([abc])*bcd", "abcd"],
  ["a|b|c|d|e", "e"],
  ["(a|b|c|d|e)f", "ef"],
  ["((a*|b))*", "-"],
  ["abcd*efg", "abcdefg"],
  ["ab*", "xabyabbbz"],
  ["ab*", "xayabbbz"],
  ["(ab|cd)e", "abcde"],
  ["[abhgefdc]ij", "hij"],
  ["^(ab|cd)e", "abcde"],
  ["(abc|)ef", "abcdef"],
  ["(a|b)c*d", "abcd"],
  ["(ab|ab*)bc", "abc"],
  ["a([bc]*)c*", "abc"],
  ["a([bc]*)(c*d)", "abcd"],
  ["a([bc]+)(c*d)", "abcd"],
  ["a([bc]*)(c+d)", "abcd"],
  ["a[bcd]*dcdcde", "adcdcde"],
  ["a[bcd]+dcdcde", "adcdcde"],
  ["(ab|a)b*c", "abc"],
  ["((a)(b)c)(d)", "abcd"],
  ["[A-Za-z_][A-Za-z0-9_]*", "alpha"],
  ["^a(bc+|b[eh])g|.h$", "abh"],
  ["(bc+d$|ef*g.|h?i(j|k))", "effgz"],
  ["(bc+d$|ef*g.|h?i(j|k))", "ij"],
  ["(bc+d$|ef*g.|h?i(j|k))", "effg"],
  ["(bc+d$|ef*g.|h?i(j|k))", "bcdd"],
  ["(bc+d$|ef*g.|h?i(j|k))", "reffgz"],
  ["((((((((((a))))))))))", "-"],
  ["(((((((((a)))))))))", "a"],
  ["multiple words of text", "uh-uh"],
  ["multiple words", "multiple words, yeah"],
  ["(.*)c(.*)", "abcde"],
  ["\((.*),", "(.*)\)"],
  ["[k]", "ab"],
  ["abcd", "abcd"],
  ["a(bc)d", "abcd"],
  ["a[^A-^C]?c", "ac"],
  ["(....).*\1", "beriberi"],
]

puts "pid: #{Process.pid}"
grep_exe = "/tmp/#{Process.pid}.exe"
if not (File.exists?("out"))
  Dir.mkdir("out")
end
compile(config, grep_exe)
i = 0
tests.each { |search, contents|
  msg = "#{i}: Testing search=\"#{search}\", contents=\"#{contents}\"..."
  print msg
  real = run(grep_exe, search, contents)
  symbolic = symbolic_exec(i, config, search, contents)
  symbolic = eval("\"" + symbolic + "\"") # unescape string
  if (real == symbolic)
    puts "PASS"
  else
    puts "FAIL:  real=\"#{real}\" symbolic=\"#{symbolic}\""
  end
  i = i + 1
}
File.unlink grep_exe
