#!/bin/sh

if [ $# -ne 6 ]
then
    echo "Usage: ./setup_backotter_tests.sh <base-directory> <line_targets_dir> <experiment-name> <programs_dir> <programs.in> <options.in>"
    exit 1
fi

base="$1"
line_targets_dir="$2"
exp_name="$(echo $3|sed 's/ /_/g')"
exp_base="$base/backotter_$(date "+%Y_%m_%d_%H")_$exp_name"
runbackotter="$(pwd)/runbackotter"

programs_dir="$4"
programs_in="$5"
options_in="$6"

mkdir -p "$exp_base"
chmod 755 "$exp_base"

cat $programs_in | while read prog_opt
do 
    prog=$(echo $prog_opt|sed 's/\(.*\.c\).*|.*/\1/')
    prog_opt=$(echo $prog_opt|sed 's/.*\.c.*|\(.*\)/\1/')
    prog="$programs_dir/$prog"
    if [ "$(echo $prog | sed '/^[ ]*#/d')" ]; then echo "Process $prog with options $prog_opt"; else continue; fi
    options_id=1
    cat $options_in | while read options
    do 
        if [ "$(echo $options | sed '/^[ ]*#/d')" ]; then echo "Process $options"; else continue; fi
        options="$prog_opt $options"
        prog_name=$(basename $prog .c)
        log_file="$exp_base/results/${prog_name}_$options_id.log"
        test_sh="$exp_base/tests/${prog_name}_$options_id.sh"
        options_id=$(expr $options_id + 1)
        line_targets_file="$line_targets_dir/$prog_name.line_targets"
        mkdir -p "$(dirname "$test_sh")"
        echo "# options: $options" >> $test_sh
        echo "mkdir -p \"$(dirname "$log_file")\"" >> $test_sh
        echo "\"$runbackotter\" \"$prog\" --line-targets-file=$line_targets_file $options 2>&1 | timelines > \"$log_file\"" >> $test_sh
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh

