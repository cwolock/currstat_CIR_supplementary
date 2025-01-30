#!/usr/local/bin/Rscript

sim_name <- "icenReg_bootstrap"
nreps_total <- 1000
nreps_per_job <- 5

## set up directories for output, plots
output_dir <- "output/"

## set up parameter grid
ns <- c(500, 1000, 1500, 2000)
Bs <- c(100, 250, 500, 1000)
missing_bounds <- c(1.65,1.8, 2.1)
## number of monte-carlo iterations per job
nreps_per_combo <- nreps_total/nreps_per_job
## set up grid of parameters
param_grid <- expand.grid(mc_id = 1:nreps_per_combo,
                          n = ns,
                          B = Bs,
                          missing_bound = missing_bounds)

## names of files to read in
output_nms <- paste0(sim_name, "_", 1:dim(param_grid)[1], ".rds")
avail_nms <- list.files(output_dir, pattern = paste0(sim_name, "_*"))
names_to_try <- output_nms[which(output_nms %in% avail_nms)]
missing <- output_nms[which(!(output_nms %in% avail_nms))]
print(missing)
## list of output
output_lst <- lapply(paste0(output_dir, names_to_try), readRDS)
## make it a matrix
output_df <- do.call(rbind.data.frame, output_lst)
saveRDS(output_df, paste0(sim_name, ".rds"))
