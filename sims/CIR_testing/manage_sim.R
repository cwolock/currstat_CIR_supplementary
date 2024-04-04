#!/usr/local/bin/Rscript
.libPaths(c(
  "/home/cwolock/R_lib",
  .libPaths()
))

# Setup
library(SuperLearner, lib = "/home/cwolock/R_lib")
library(dplyr, lib = "/home/cwolock/R_lib")
library(fdrtool, lib = "/home/cwolock/R_lib")
library(haldensify, lib = "/home/cwolock/R_lib")
library(ChernoffDist, lib = "/home/cwolock/R_lib")
#library(SuperLearner)
#library(dplyr)
#library(fdrtool)
#library(haldensify)
#library(ChernoffDist)

sim_name <- "CIR_testing"
nreps_total <- 500
nreps_per_job <- 1

source("/home/cwolock/chu_lab/susan/code/CIR_testing/do_one.R")
source("/home/cwolock/chu_lab/susan/code/CIR_testing/utils.R")

ns <- c(250, 500, 750, 1000, 1250, 1500)
nonresponses <- c(FALSE)

njobs_per_combo <- nreps_total/nreps_per_job

param_grid <- expand.grid(mc_id = 1:njobs_per_combo,
                          n = ns,
                          nonresponse = nonresponses)

job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

current_dynamic_args <- param_grid[job_id, ]

current_seed <- job_id
set.seed(current_seed)
output <- replicate(nreps_per_job,
                    do_one(n = current_dynamic_args$n,
                           nonresponse = current_dynamic_args$nonresponse),
                    simplify = FALSE)
sim_output <- lapply(as.list(1:length(output)),
                     function(x) tibble::add_column(output[[x]]))
sim_output_tib <- do.call(rbind.data.frame, sim_output)
file_name <- paste0("output/", sim_name, "_", job_id, ".rds")
saveRDS(sim_output_tib, file = file_name)
