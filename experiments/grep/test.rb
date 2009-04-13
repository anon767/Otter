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
#  puts cmd
  system cmd
end

def run(exe, search, contents)
  input = "/tmp/#{Process.pid}.txt"
  File.open(input, "w") { |f|
    f.puts contents
  }
  result = nil
  IO.popen("#{exe} #{search} #{input}") { |f|
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

def symbolic_exec(config, search, contents)
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
#  puts cmd
  system cmd
  File.unlink tmpfile

  cmd = "#{$cilly} #{combfile} --useLogicalOperators --arg=#{search} " +
        "--arg=input.txt --doexecute"
#  CILLY_DONT_COMPILE_AFTER_MERGE=1
#  puts cmd
  result = ""
  state = 0
  Open3.popen3(cmd) { |stdin,stdout,stderr|
    while not stdout.eof?
      if ((stdout.readline =~ /__COMMENT\(\(char \*\)"Writing on fildes"\);/) &&
          (stdout.readline) && # COMMENT:(char *)"Writing on fildes"
          (stdout.readline) && # #line 101
          (stdout.readline) && # __EVAL(fildes)
          (stdout.readline =~ /Evaluates to Bytearray\(\/01\/00\/00\/00\)/) &&
          (stdout.readline) && # #line 103
          (stdout.readline) && # __EVALSTR((char *_buf, (int )nbyte)
          (stdout.readline =~ /Evaluates to string: "(.*)"/))
        result = result + $1
      end
    end
  }

  File.unlink combfile
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
]

grep_exe = "/tmp/#{Process.pid}.exe"
compile(config, grep_exe)
tests.each { |search, contents|
  msg = "Testing search=\"#{search}\", contents=\"#{contents}\"..."
  print msg
  real = run(grep_exe, search, contents)
#  puts "Got string: #{real}"
  symbolic = symbolic_exec(config, search, contents)
  symbolic = eval("\"" + symbolic + "\"") # unescape string
#  puts "Got symbolic string: #{symbolic}"
  if (real == symbolic)
    puts "PASS"
  else
    puts "FAIL:  real=\"#{real}\" symbolic=\"#{symbolic}\""
  end
}
File.unlink grep_exe
