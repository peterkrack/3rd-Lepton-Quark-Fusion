#!/bin/bash

# this is an example of a parallel run setup, suitable for running on a single
# multi-core node. It is meant to illustrate the sequence of the typical parallel run.
# Appropriate scripts should be setup for use with batch systems, depending upon
# the specific implementation.

#source ~/Pheno/Herwig7.2.1-inst/bin/activate
#source activate          # script to set the environmental variables automatically generated by the hw7 bootstrap


#personal rivet analysis

if [ -d HerwigRun ] ; then
    rm -r HerwigRun 
fi

mkdir HerwigRun

cd HerwigRun

ln -sf ../../HerwigInterface/powhegHerwig.so .
cp ../powheg.input .

(echo -n start hw7_analysis ' ' ; date ) >> Timings.txt

# number of cores to be used for a given parallel run
ncores=8

# The pwhg_main executable should be in the ../ directory

# the following function limits the number of subprocesses
# to be not larger than the number of cores specified by the
# user
function limit_procs {
    while [ `jobs -p | wc -w` -ge $ncores ]
    do
	sleep 1
    done
}


for file in ../pwgevents-????.lhe
do
    evtsfile=`echo $file | sed 's/\.\.\///'` 
    sed 's/<initrwgt>// ; s/<\/initrwgt>// ' $file > $evtsfile
    seed=`echo $evtsfile | sed 's/pwgevents-// ; s/.lhe//'`
    cat ../../run-master/Herwig.in | sed "s/set myReader:FileName.*/set myReader:FileName $evtsfile/ ; s/set powhegAnalysis:RunNumber.*/set powhegAnalysis:RunNumber $seed/; s/saverun.*/saverun Herwig-$seed EventGenerator/" > Herwig-$seed.in
    Herwig read Herwig-$seed.in > run-hw7-$seed.log 
    Herwig run Herwig-$seed.run >> run-hw7-$seed.log &    
    limit_procs
done
# this 'wait' instruction is only to get the final timing in the timing file.
wait

(echo -n end hw7_analysis ; date ) >> Timings.txt

outfile=pwg-POWHEG+HERWIG7-output

labels=( 'dummy1' '11' '12' '21' '22' '1H' 'H1' 'HH' )
# adjust number of weights accordingly
for i in {2..8}
do
    mergedata 1 pwg-0*-POWHEG+HERWIG7-output-W$i.top -o $outfile-${labels[$i-1]}.top
done



