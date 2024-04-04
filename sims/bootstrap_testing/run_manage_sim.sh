#!/bin/bash
ml fhR
# num_combos is number of parameter combinations (see manage_sim.R)
#    this does not need to be changed
num_combos=32
njobs=`expr 1000 \* $num_combos`

sbatch --array=1-$njobs -p short -t 0:05:00 -e ./iotrash/s-%A_%a.out -o ./iotrash/s-%A_%a.out /home/cwolock/chu_lab/susan/code/bootstrap_testing/call_manage_sim.sh
