#!/bin/sh

if [ $# -ne 5 ]
then
    echo "Usage: ./setup_tests.sh <output_dir> <experiment_name> <trunk_dir> <programs_list> <commands_list>"
    exit 1
fi

base="$1"
exp_name="$(echo $2|sed 's/ /_/g')"
exp_base="$base/$exp_name"

trunk_dir="$3"
programs_in="$4"
options_in="$5"

function add_trunk { 
    echo $1 | sed "s:@TRUNK@:$trunk_dir:g" 
}
function not_comment { 
    echo $1 | sed '/^[ ]*#/d' 
}

mkdir -p "$exp_base"
chmod 755 "$exp_base"

config="$exp_base/config.log"

echo >> $config "Options used to generate this test suite:"
echo >> $config "$*"                                       
echo >> $config                                            
echo >> $config "Program settings in $programs_in:"        
cat  >> $config "$programs_in"                             
echo >> $config                                            
echo >> $config "Option settings in $options_in:"          
cat  >> $config "$options_in"                              

cat "$programs_in" | while read prog_opt
do 
    if [ "$(not_comment "$prog_opt")" ]; then echo "Process $prog_opt"; else continue; fi
    prog_opt=$(add_trunk "$prog_opt")
    prog_name=$(basename $(echo $prog_opt|sed 's/"\(.*\.c\)".*/\1/') .c)
    options_id=1
    cat "$options_in" | while read options
    do 
        if [ "$(not_comment "$options")" ]; then echo "Process $options"; else continue; fi
        options="$(add_trunk "$options")"
        log_file="$exp_base/results/${prog_name}_$options_id.log"
        test_sh="$exp_base/tests/${prog_name}_$options_id.sh"
        options_id=$(expr $options_id + 1)
        mkdir -p "$(dirname "$test_sh")"
        echo >> $test_sh "mkdir -p \"$(dirname "$log_file")\""                 
        echo >> $test_sh "echo Command: $options $prog_opt >> \"$log_file\""   
        echo >> $test_sh "$options $prog_opt 2>&1 | timelines >> \"$log_file\""
    done
done

at_sh="$exp_base/at.sh"
echo "find \"$exp_base/tests\" -type f | xargs -P 15 -n 1 sh" >> $at_sh

