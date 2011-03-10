#!/bin/sh

if [ $# -ne 6 ]
then
    echo "Usage: ./setup_tests.sh <otter-flavor> <output-directory> <experiment-name> <programs_source_dir> <programs.in> <options.in>"
    exit 1
fi

otter_flavor=$1
base="$2"
exp_name="$(echo $3|sed 's/ /_/g')"
exp_base="$base/${otter_flavor}_$exp_name"
ottercmd="$(pwd)/run$otter_flavor"

programs_dir="$4"
programs_in="$5"
options_in="$6"

mkdir -p "$exp_base"
chmod 755 "$exp_base"

config="$exp_base/config.log"

echo "Options used to generate this test suite:" >> $config
echo "$*" >> $config
echo "" >> $config
echo "Program settings in $programs_in:" >> $config
cat "$programs_in" >> $config
echo "" >> $config
echo "Option settings in $options_in:" >> $config
cat "$options_in" >> $config

cat "$programs_in" | while read prog_opt
do 
    prog=$(echo $prog_opt|sed 's/\(.*\.c\).*|.*/\1/')
    prog_opt=$(echo $prog_opt|sed 's/.*\.c.*|\(.*\)/\1/')
    if [ "$(echo $prog | sed '/^[ ]*#/d')" ]; then echo "Process $prog with options $prog_opt"; else continue; fi
    prog="$programs_dir/$prog"
    options_id=1
    cat "$options_in" | while read options
    do 
        if [ "$(echo $options | sed '/^[ ]*#/d')" ]; then echo "Process $options"; else continue; fi
        options="$prog_opt $options"
        prog_name=$(basename $prog .c)
        log_file="$exp_base/results/${prog_name}_$options_id.log"
        test_sh="$exp_base/tests/${prog_name}_$options_id.sh"
        options_id=$(expr $options_id + 1)
        mkdir -p "$(dirname "$test_sh")"
        echo "# options: $options" >> $test_sh
        echo "mkdir -p \"$(dirname "$log_file")\"" >> $test_sh
        echo "\"$ottercmd\" \"$prog\" $options 2>&1 | timelines > \"$log_file\"" >> $test_sh
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh

