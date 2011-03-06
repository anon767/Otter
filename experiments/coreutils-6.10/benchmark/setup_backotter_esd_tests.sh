#!/bin/sh

if [ $# -ne 5 ]
then
    echo "Usage: ./setup_backotter_tests.sh <base-directory> <experiment-name> <programs_dir> <programs.in> <options.in>"
    exit 1
fi

base="$1"
exp_name="$(echo $2|sed 's/ /_/g')"
exp_base="$base/backotter_$(date "+%Y_%m_%d_%H")_$exp_name"
runbackotter="$(pwd)/runbackotter"

programs_dir="$3"
programs_in="$4"
options_in="$5"

mkdir -p "$exp_base"
chmod 755 "$exp_base"

cat $programs_in | while read prog_opt
do 
    prog=$(echo $prog_opt|sed 's/\(.*\.c\).*|.*/\1/')
    prog_opt=$(echo $prog_opt|sed 's/.*\.c.*|\(.*\)/\1/')
    prog="$programs_dir/$prog"
    if [ "$(echo $prog | sed '/^[ ]*#/d')" ]; then echo "Process $prog with options $prog_opt"; else continue; fi
    cat $options_in | while read options
    do 
        if [ "$(echo $options | sed '/^[ ]*#/d')" ]; then echo "Process $options"; else continue; fi
        options="$prog_opt $options"
        prog_name=$(basename $prog .c)
        opts_name=$(echo $options|sed 's/ /_/g')
        log_file="$exp_base/results/$prog_name/run_with_$opts_name.log"
        test_sh="$exp_base/tests/${prog_name}_run_with_$opts_name.sh"
        mkdir -p "$(dirname "$test_sh")"
        echo "mkdir -p \"$(dirname "$log_file")\" && \"$runbackotter\" \"$prog\" $options 2>&1 | timelines > \"$log_file\"" >> $test_sh
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh
