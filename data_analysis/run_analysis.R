set.seed(72724)
library(ranger)
library(earth)
library(SuperLearner)
library(dplyr)
library(haldensify)
library(ChernoffDist)
library(fdrtool)
library(xgboost)
library(survML)

dat <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120.rds")

names(dat)[names(dat) == "time_event"] <- "y"
names(dat)[names(dat) == "resolution"] <- "delta"

# xgboost tuning parameters
tune <- list(ntrees = c(250, 500, 1000), max_depth = c(1,2), minobspernode = 10, shrinkage = 0.01)
xgb_grid <- create.SL.xgboost(tune = tune)

res <- survML::currstatCIR(time = dat$y,
                           event = dat$delta,
                           W = dat[,!(names(dat) %in% c("y", "delta"))],
                           SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger", xgb_grid$names),
                                             V = 5,
                                             method = "method.NNLS"),
                           HAL_control = list(n_bins = c(5,10),
                                              grid_type = c("equal_mass", "equal_range"),
                                              V = 5),
                           missing_method = "extended",
                           n_eval_pts = 1001,
                           eval_region = c(0, 115))

saveRDS(res_correct, "/home/cwolock/currstat_CIR_supplementary/data_analysis/CIR_results.rds")
