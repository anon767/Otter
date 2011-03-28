#!/bin/sh
trunk="$PWD/../../../"
min_seed=$2 
max_seed=$3 
output_dir=$4
trunk_version=$(svnversion "$trunk" | sed 's/:/_/')
otter_version=$(svnversion "$trunk/otter" | sed 's/:/_/')
./setup_tests.py "$output_dir" "otter $otter_version trunk $trunk_version $min_seed $max_seed $1" "$trunk" $min_seed $max_seed programs.in options.in options-common.in
