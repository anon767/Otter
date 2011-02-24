#!/bin/sh

if [ $# -ne 2 ]
then
    echo "Usage: ./setup_tests.sh <base-directory> <experiment-name>"
    exit 1
fi

base="$1"
exp_name="$(echo $2|sed 's/ /_/g')"
exp_base="$base/otter_$(date "+%Y_%m_%d_%H")_$exp_name"
runotter="$(pwd)/runotter"

mkdir -p "$exp_base"
chmod 700 "$exp_base"

cat programs.in | while read prog
do 
    cat options.in | while read options
    do 
        prog_name=$(basename $prog .c)
        opts_name=$(echo $options|sed 's/ /_/g')
        log_file="$exp_base/results/$prog_name/run_with_$opts_name.log"
        test_sh="$exp_base/tests/${prog_name}_run_with_$opts_name.sh"
        mkdir -p $(dirname "$test_sh")
        mkdir -p $(dirname "$log_file")
        echo "\"$runotter\" \"$prog\" $options 2>&1 | timelines > \"$log_file\"" >> $test_sh
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh

