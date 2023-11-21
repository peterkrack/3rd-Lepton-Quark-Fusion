#!/bin/bash

# this is an example of a parallel run setup, suitable for running on a single
# multi-core node. It is meant to illustrate the sequence of the typical parallel run.
# Appropriate scripts should be setup for use with batch systems, depending upon
# the specific implementation.


> Timings.txt

ncores=8

# the following function limits the number of subprocesses
# to be not larger than the number of cores specified by the
# user

function limit_procs {
    while [ `jobs -p | wc -w` -gt $ncores ]
    do
	sleep 1
    done
}

# number of parallel runs; better be a multiple of ncores.
nprocesses=8

# The pwhg_main executable should be in the ../ directory
#
PRG=../pwhg_main

#-----------------------------------------------------------------
# PARALLEL STAGE 1
#-----------------------------------------------------------------

# two stages of importance sampling grid calculation
for igrid in 1 2 3
do
    
    (echo -n st1 xg$igrid ' ' ; date ) >> Timings.txt

    cat powheg.input-save | sed "s/xgriditeration.*/xgriditeration $igrid/ ; s/parallelstage.*/parallelstage 1/" > powheg.input

    for i in `seq $nprocesses`
    do
	echo $i | $PRG > run-st1-xg$igrid-$i.log 2>&1 &
	limit_procs
    done
    wait
    
done



#-----------------------------------------------------------------
# PARALLEL STAGE 2
#-----------------------------------------------------------------

# compute NLO and upper bounding envelope for underlying born comfigurations
cat powheg.input-save | sed 's/parallelstage.*/parallelstage 2/ ' > powheg.input
(echo -n st2 ' ' ; date ) >> Timings.txt
for i in `seq $nprocesses`
do
    echo $i | $PRG > run-st2-$i.log 2>&1 &
    limit_procs
done
# this stage should be fully completed before the next stage is started.
# on batch systems, failed jobs can be abandoned, if their number is not too large.
wait

#-----------------------------------------------------------------
# PARALLEL STAGE 3
#-----------------------------------------------------------------

# compute upper bounding coefficients for radiation
cat powheg.input-save | sed 's/parallelstage.*/parallelstage 3/' > powheg.input
(echo -n st3 ' ' ; date ) >> Timings.txt
for i in `seq $nprocesses`
do
    echo $i | $PRG > run-st3-$i.log 2>&1 &
    limit_procs
done
# this stage should be fully completed before the next stage is started.
# on batch systems, failed jobs can be abandoned, if their number is not too large.
wait

#-----------------------------------------------------------------
# PARALLEL STAGE 4
#-----------------------------------------------------------------

# generate events 
cat powheg.input-save | sed 's/parallelstage.*/parallelstage 4/' > powheg.input
(echo -n st4 ' ' ; date ) >> Timings.txt
for i in `seq $nprocesses`
do
    echo $i | $PRG > run-st4-$i.log 2>&1 &
    limit_procs
done
# this 'wait' instruction is only to get the final timing in the timing file.
wait

(echo -n end ' ' ; date ) >> Timings.txt




