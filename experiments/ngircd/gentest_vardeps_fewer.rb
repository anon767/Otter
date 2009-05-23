#!/usr/bin/ruby

def make_testcontents(name)
  s = <<END
TOPDIR=../../..
MARSHALDIR=/fs/skoll/symexe/data/ngircd/12.6fewer/marshalledData
POSSIBLE=/fs/skoll/symexe/data/ngircd/ngircdPossibleValues

cd $1

echo
echo Running Test vardeps_fewer_#{name} 
echo


CILLY_DONT_COMPILE_AFTER_MERGE= \\
$TOPDIR/cil/bin/cilly \\
	--docalculateVarDeps \\
	--fileWithCovInfo=$MARSHALDIR/#{name}.marshal \\
	--fileWithPossibleValues=$POSSIBLE --numVars=1 /fs/skoll/symexe/trunk/cil/setest/test0.c 

echo "============================================"

CILLY_DONT_COMPILE_AFTER_MERGE= \\
$TOPDIR/cil/bin/cilly \\
	--docalculateVarDeps \\
	--fileWithCovInfo=$MARSHALDIR/#{name}.marshal \\
	--fileWithPossibleValues=$POSSIBLE --numVars=2 /fs/skoll/symexe/trunk/cil/setest/test0.c 

END
end

def make_testfile(name)
  file = "tests/#{name}.test"
  File.open(file, "w") { |f|
    f.puts(make_testcontents(name))
  }
  File.chmod(0755,file)
end

tests = [
	"rfc1459_4.1.1",
	"rfc1459_4.1.2-a",
	"rfc1459_4.1.2-b",
	"rfc1459_4.1.2",
	"rfc1459_4.1.3-a",
	"rfc1459_4.1.3",
	"rfc1459_4.1.4",
	"rfc1459_4.1.5",
	"rfc1459_4.1.6",
	"rfc1459_4.1.7",
	"rfc1459_4.2.1-a",
	"rfc1459_4.2.1",
	"rfc1459_4.2.2-a",
	"rfc1459_4.2.2",
	"rfc1459_4.2.3.1",
	"rfc1459_4.2.3.2",
	"rfc1459_4.2.4",
	"rfc1459_4.2.5",
	"rfc1459_4.2.6",
	"rfc1459_4.2.7",
	"rfc1459_4.2.8",
	"rfc1459_4.3.1",
	"rfc1459_4.3.2",
	"rfc1459_4.3.3",
	"rfc1459_4.3.4",
	"rfc1459_4.3.5",
	"rfc1459_4.3.6",
	"rfc1459_4.3.7",
	"rfc1459_4.3.8",
	"rfc1459_4.4.1-a",
	"rfc1459_4.4.1",
	"rfc1459_4.4.2-a",
	"rfc1459_4.4.2",
	"rfc1459_4.5.1",
	"rfc1459_4.5.2",
	"rfc1459_4.5.3",
	"rfc1459_4.6.1",
	"rfc1459_4.6.2",
	"rfc1459_4.6.3",
	"rfc1459_5.1",
	"rfc1459_5.2",
	"rfc1459_5.3",
	"rfc1459_5.5",
	"rfc1459_5.7",
	"rfc1459_5.8",
	"rfc2812_3.4.2",
	"rfc2813_4.1.2",
	"rfc2813_4.2.2",

	"rfc2812_3.2.1",
	"rfc2813_4.1.2-a",
	"plus_chaninfo",

	"ngircd_00",
	"ngircd_01",
	"ngircd_02",
	"ngircd_03",
	"ngircd_04",
	"ngircd_05",
	"ngircd_06",
	"ngircd_07",
	"ngircd_08",
	"ngircd_09",

	"ngircd_10",
	"ngircd_11",
	"ngircd_12",
	"ngircd_13",
	"ngircd_14",
	"ngircd_15",
	"ngircd_16",
	"ngircd_17",
	"ngircd_18",
	"ngircd_19",
	
]

tests.each { |name|
  puts "Generate #{name}.test"
  make_testfile(name)
}
