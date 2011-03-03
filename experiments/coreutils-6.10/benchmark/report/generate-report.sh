#!/bin/sh

test_results_dir="$1"
option_filter="$2"

find "$test_results_dir" -name "$2" | while read log
do 
    prog=$(echo $log|sed 's|.*results/\(.*\)_comb.*|\1|')  # Get the program name
    echo -n $prog "	"
    ./parse-backotter-output.py $log
done
