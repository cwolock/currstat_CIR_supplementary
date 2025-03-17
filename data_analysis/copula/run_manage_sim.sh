#!/bin/bash
ml R/4.4.0-gfbf-2023b
# num_combos is number of parameter combinations (see manage_sim.R)
#    this does not need to be changed
num_combos=70
njobs=`expr 1 \* $num_combos`

sbatch --array=1-$njobs -e ./iotrash/s-%A_%a.out -o ./iotrash/s-%A_%a.out /home/cwolock/currstat_CIR_supplementary/data_analysis/copula/call_manage_sim.sh
