#!/bin/bash

> Timings.txt

ncores=8
nprocesses=8
# First compile the pwhg_main executable in the ../ directory
#

# the following function limits the number of subprocesses
# to be not larger than the number of cores specified by the
# user

function limit_procs {
    while [ `jobs -p | wc -w` -gt $ncores ]
    do
	sleep 1
    done
}

    
outfile=pwgLHEFanalysis-output
    
for file in pwgevents-0*.lhe
do
    which=lhef-`echo $file | sed 's/pwgevents-//; s/lhe/log/ '`
    echo $file | ../lhef_analysis > $which &
done

wait

labels=( 'dummy1' '11' '12' '21' '22' '1H' 'H1' 'HH' )
# adjust number of weights accordingly
for i in {2..8}
do
    mergedata 1 pwgLHEF_analysis-00*W$i.top -o $outfile-${labels[$i-1]}.top
done
