#!/bin/bash
ml R/4.4.0-gfbf-2023b
# num_combos is number of parameter combinations (see manage_sim.R)
#    this does not need to be changed
num_combos=8
njobs=`expr 500 \* $num_combos`

sbatch --array=1-$njobs -p short -t 02:00:00 -e ./iotrash/s-%A_%a.out -o ./iotrash/s-%A_%a.out /home/cwolock/currstat_CIR_supplementary/sims/CIR_testing/sample_splitting/call_manage_sim.sh
