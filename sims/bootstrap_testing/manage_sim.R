#!/usr/local/bin/Rscript
.libPaths(c(
  "/home/cwolock/R_lib",
  .libPaths()
))
suppressMessages(library(dplyr))

sim_name <- "icenReg_bootstrap"
nreps_total <- 1000
nreps_per_job <- 1

source("/home/cwolock/chu_lab/susan/code/bootstrap_testing/do_one.R")

ns <- c(500, 1000, 1500, 2000)
Bs <- c(100, 250, 500, 1000)
nonresponses <- c(FALSE, TRUE)

njobs_per_combo <- nreps_total/nreps_per_job

param_grid <- expand.grid(mc_id = 1:njobs_per_combo,
                          n = ns,
                          B = Bs,
                          nonresponse = nonresponses)

job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

current_dynamic_args <- param_grid[job_id, ]

current_seed <- job_id
set.seed(current_seed)
output <- replicate(nreps_per_job,
                    do_one(n = current_dynamic_args$n,
                           B = current_dynamic_args$B,
                           nonresponse = current_dynamic_args$nonresponse),
                    simplify = FALSE)
sim_output <- lapply(as.list(1:length(output)),
                     function(x) tibble::add_column(output[[x]]))
sim_output_tib <- do.call(rbind.data.frame, sim_output)
file_name <- paste0("output/", sim_name, "_", job_id, ".rds")
saveRDS(sim_output_tib, file = file_name)
