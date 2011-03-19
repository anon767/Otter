#!/bin/sh
trunk="$PWD/../../../"
min_seed=$2 
max_seed=$3 
output_dir=$4
svn_version=$(svnversion "$trunk" | sed 's/:/_/')
./setup_tests.py "$output_dir" "$svn_version $min_seed $max_seed $1" "$trunk" $min_seed $max_seed programs.in options.in options-common.in
