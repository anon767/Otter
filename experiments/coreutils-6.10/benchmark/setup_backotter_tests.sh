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

cat $programs_in | while read prog
do 
    prog="$programs_dir/$prog"
    if [ "$(echo $prog | sed '/^[ ]*#/d')" ]; then echo "Process $prog"; else continue; fi
    cat $options_in | while read options
    do 
        if [ "$(echo $options | sed '/^[ ]*#/d')" ]; then echo "Process $options"; else continue; fi
        prog_name=$(basename $prog .c)
        opts_name=$(echo $options|sed 's/ /_/g')
        log_file="$exp_base/results/$prog_name/run_with_$opts_name.log"
        test_sh="$exp_base/tests/${prog_name}_run_with_$opts_name.sh"
        line_targets_file="$line_targets_dir/$prog_name.line_targets"
        mkdir -p "$(dirname "$test_sh")"
        echo "mkdir -p \"$(dirname "$log_file")\" && \"$runbackotter\" \"$prog\" --line-targets-file=$line_targets_file $options 2>&1 | timelines > \"$log_file\"" >> $test_sh
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh

