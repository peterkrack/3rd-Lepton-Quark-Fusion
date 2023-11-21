#!/bin/bash

quark="b" # choose u,d,s,c,b
lepton="t" # choose e,m,t
coupling=1
charge=2  # in units of 1/3
massLQ=2000

# Adding cuts for jet, lepton and LQ amss
ptcutjet=300   # default 500
ptcutlep=200   # default 500
etacutjet=2.4  # default 2.5
etacutlep=2.1  # default 2.5
mLQcuthi=7500  # default HUGE()
mLQcutlo=200  # default 0
tageff=0.6 # tau-tagging efficiency
drljetcut=0.5 # dR between b- and tau-jet
dphilptmiss=0.3 #dphi between missing pt and taujet
ptmisscut=100 # missing pt cut


# Run parallel set number of cores and processes to run. Each process will
# shower 250000 events
ncores=8
nprocesses=8

# creating three directories. Directory with LO/NLO will be used to compute 
# the process in, while the directory without any suffix will contain the 
# plots.

rundir=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')
newrundir=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')
i=1
while [ -d ${newrundir} ]; do 
   newrundir=${rundir}_${i}
   let "i+=1"
done
rundir=${newrundir}
mkdir ${rundir}

rundirLO=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')-LO
newrundirLO=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')-LO
i=1
while [ -d ${newrundirLO} ]; do 
   newrundirLO=${rundirLO}_${i}
   let "i+=1"
done
rundirLO=${newrundirLO}
mkdir ${rundirLO}

rundirNLO=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')-NLO
newrundirNLO=run-${quark}${lepton}-${massLQ}GeV-${charge}-$(date '+%Y%m%d')-NLO
i=1
while [ -d ${newrundirNLO} ]; do 
   newrundirNLO=${rundirNLO}_${i}
   let "i+=1"
done
rundirNLO=${newrundirNLO}
mkdir ${rundirNLO}

cp ./run-master/plots.py ./${rundir}
cp ./run-master/plotter_style.py ./${rundir}
cp ./run-master/* ./${rundirLO}/
cp ./run-master/* ./${rundirNLO}/

cd ${rundirLO}
echo "$(pwd)"
sed "s/nprocesses=8/nprocesses=${nprocesses}/g" run-parallel.sh > tmprun-parallel.sh
mv tmprun-parallel.sh run-parallel.sh
sed "s/ncores=8/ncores=${ncores}/g" run-parallel.sh > tmprun-parallel.sh
mv tmprun-parallel.sh run-parallel.sh
chmod +x run-parallel.sh
sed "s/charge 1/charge ${charge}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/mU 1/mU ${massLQ}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/ptcutjet 0/ptcutjet ${ptcutjet}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/ptcutlep 0/ptcutlep ${ptcutlep}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/etacutjet 0/etacutjet ${etacutjet}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/etacutlep 0/etacutlep ${etacutlep}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/mLQcuthi 0/mLQcuthi ${mLQcuthi}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/mLQcutlo 0/mLQcutlo ${mLQcutlo}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/tageff 0/tageff ${tageff}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/drljetcut 0/drljetcut ${drljetcut}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/ptmisscut 0/ptmisscut ${ptmisscut}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO
sed "s/dphilptmiss 0/dphilptmiss ${dphilptmiss}/g" powheg.input-save-LO > powheg.input-save
mv powheg.input-save powheg.input-save-LO

mv powheg.input-save-LO powheg.input-save

echo "Running POWHEG"
./run-parallel.sh
echo "Running LHE analysis"
./runlhe.sh
echo "Running Herwig"
./hw7.sh
cp ./HerwigRun/*.top .
./refine.sh
echo "LO process completed, moving on to NLO"

# NLO folder now
cd ../${rundirNLO}
echo "$(pwd)"
sed "s/nprocesses=8/nprocesses=${nprocesses}/g" run-parallel.sh > tmprun-parallel.sh
mv tmprun-parallel.sh run-parallel.sh
sed "s/ncores=8/ncores=${ncores}/g" run-parallel.sh > tmprun-parallel.sh
mv tmprun-parallel.sh run-parallel.sh
chmod +x run-parallel.sh
sed "s/charge 1/charge ${charge}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/mU 1/mU ${massLQ}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/ptcutjet 0/ptcutjet ${ptcutjet}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/ptcutlep 0/ptcutlep ${ptcutlep}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/etacutjet 0/etacutjet ${etacutjet}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/etacutlep 0/etacutlep ${etacutlep}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/mLQcuthi 0/mLQcuthi ${mLQcuthi}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/mLQcutlo 0/mLQcutlo ${mLQcutlo}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/tageff 0/tageff ${tageff}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/drljetcut 0/drljetcut ${drljetcut}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/ptmisscut 0/ptmisscut ${ptmisscut}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO
sed "s/dphilptmiss 0/dphilptmiss ${dphilptmiss}/g" powheg.input-save-NLO > powheg.input-save
mv powheg.input-save powheg.input-save-NLO

mv powheg.input-save-NLO powheg.input-save

echo "Running POWHEG"
./run-parallel.sh
echo "Running LHE analysis"
./runlhe.sh
echo "Running Herwig"
./hw7.sh
cp ./HerwigRun/*.top .
./refine.sh
echo "NLO process completed."

# Plot the histograms now.
cd ../${rundir}
sed "s/rundirnameLO/${rundirLO}/g" plots.py > plot.py
mv plot.py plots.py
sed "s/rundirnameNLO/${rundirNLO}/g" plots.py > plot.py
python3.9 plot.py
