library(SuperLearner)
library(dplyr)
library(fdrtool)
library(haldensify)
#library(ChernoffDist)
library(survML)
# library(copula)
library(ranger)
library(earth)
library(xgboost)

sim_name <- "CIR_copula_030325_known_nuis"
nreps_total <- 1
nreps_per_job <- 1

source("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/do_one.R")

taus <- seq(-0.35, -0.02, by = 0.01)

njobs_per_combo <- nreps_total/nreps_per_job

param_grid <- expand.grid(mc_id = 1:njobs_per_combo,
                          tau = taus)

job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

current_dynamic_args <- param_grid[job_id, ]

current_seed <- job_id
set.seed(current_seed)
output <- replicate(nreps_per_job,
                    do_one(tau = current_dynamic_args$tau),
                    simplify = FALSE)
sim_output <- lapply(as.list(1:length(output)),
                     function(x) tibble::add_column(output[[x]]))
sim_output_tib <- do.call(rbind.data.frame, sim_output)
file_name <- paste0("output/", sim_name, "_", job_id, ".rds")
saveRDS(sim_output_tib, file = file_name)
