#!/bin/bash
ml R/4.4.0-gfbf-2023b
# num_combos is number of parameter combinations (see manage_sim.R)
#    this does not need to be changed
num_combos=48
njobs=`expr 1000 / 5 \* $num_combos`

sbatch --array=1-$njobs -p short -t 0:15:00 -e ./iotrash/s-%A_%a.out -o ./iotrash/s-%A_%a.out /home/cwolock/currstat_CIR_supplementary/sims/bootstrap_testing/call_manage_sim.sh
