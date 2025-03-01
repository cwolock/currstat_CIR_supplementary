#!/bin/bash
ml R/4.4.0-gfbf-2023b
# num_combos is number of parameter combinations (see manage_sim.R)
#    this does not need to be changed
num_combos=4
njobs=`expr 500 \* $num_combos`

sbatch --array=1-$njobs -p short -t 05:00:00 -e ./iotrash/s-%A_%a.out -o ./iotrash/s-%A_%a.out /home/cwolock/currstat_CIR_supplementary/sims/CIR_stability/call_manage_sim.sh
