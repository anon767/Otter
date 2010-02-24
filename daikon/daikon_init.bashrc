# Daikon's environment variables
export DAIKONDIR=$PWD
export JDKDIR=/usr/lib/jvm/java-6-sun-1.6.0.15/
export DAIKONCLASS_SOURCE=1
source $DAIKONDIR/bin/daikon.bashrc
export CLASSPATH=.:$CLASSPATH:$DAIKONDIR/jyaml-1.3.jar
