#/bin/sh

function gettimeofday {
    echo $(python -c "import time; print time.time()") 
}

start_time=$(gettimeofday)

dir="$1"

testno=1
while true
do 
    sleep 0.1
    testfile=$(printf "test%06d.ktest" $testno)
    if [ -f "$dir/$testfile" ]; then
        time_elapsed=$(echo $(gettimeofday) - $start_time|bc)
        echo "$testfile $time_elapsed"
        testno=$(expr $testno + 1)
    fi
done


