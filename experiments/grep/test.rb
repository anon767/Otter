#!/usr/bin/ruby

$cilly = "../../cil/bin/cilly"
$libcdir = "../../libc"
$mockeddir = "../../mockedFns"

config = {
  "LIST_FILES" => "0",
  "WITH_FILENAMES" => "0",
  "OUT_LINE" => "1",
  "MATCH_WORDS" => "1",
  "MATCH_LINES" => "1",
  "BINARY_FILES" => "0",
  "COUNT_MATCHES" => "0",
  "NO_FILENAMES" => "0",
  "SUPPRESS_ERRORS" => "0",
}

def compile(defs, exe)
  args = ""
  defs.each_pair { |k,v|
    args += " -D#{k}=#{v}"
  }
  cmd = "gcc -I. #{args} -DCONCRETE grep.c -o #{exe}"
  puts cmd
  system cmd
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

	sym_file_t* input = IOSIM_addfile("input.txt", 0);
	input->contents = "#{contents}";
	input->stat.st_size = #{contents.length};
}
END

end

def merge(defs, comb, contents)
  tmpfile = "/tmp/#{Process.pid}-test.c"
  File.open(tmpfile, "w") { |f|
    f.puts(make_mock contents)
  }
  args = ""
  defs.each_pair { |k,v|
    args += " -D#{k}=#{v}"
  }
  cmd = "CILLY_DONT_COMPILE_AFTER_MERGE=1 " +
        "#{$cilly} --merge --useLogicalOperators --out=#{comb} " +
        "#{args} -I. -I #{$libcdir} -I #{$mockeddir} " +
        "-include #{$mockeddir}/symexe.h " +
            "#{$libcdir}/*.c #{$mockeddir}/*.c #{tmpfile} grep.c --warnall"
  puts cmd
  system cmd
  File.unlink tmpfile
end

def symbolic_exec(comb)
  cmd = "#{$cilly} #{comb} --doexecute --printLittle"
#  cmd = "CILLY_DONT_COMPILE_AFTER_MERGE=1 #{$cilly} #{comb} --doexecute"
  puts cmd
  system cmd
end

tests = [ ["abc", "abc"],
]

pid = Process.pid
grep_exe = "/tmp/#{pid}.exe"
compile(config, grep_exe)
tests.each { |search, contents|

#  input = "/tmp/#{pid}.txt"
#  File.open(input, "w") { |f|
#    f.puts contents
#  }
#  system("#{grep} #{search} #{input}")
#  File.unlink input
 
  grep_comb = "/tmp/#{pid}_comb.c"
  merge(config, grep_comb, contents)
  symbolic_exec grep_comb
  File.unlink grep_comb
}
File.unlink grep_exe
