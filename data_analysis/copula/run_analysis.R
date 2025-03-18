set.seed(72724)
library(SuperLearner)
library(dplyr)
library(survML)

dat <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/long_covid_data.rds")

dat <- dat %>% select(-record_id)
names(dat)[names(dat) == "time_event"] <- "y"
names(dat)[names(dat) == "resolution"] <- "delta"

tune <- list(ntrees = c(250, 500, 1000), max_depth = c(1,2), minobspernode = 10, shrinkage = 0.01)
xgb_grid <- create.SL.xgboost(tune = tune)

res <- survML::currstatCIR(time = dat$y,
                           event = dat$delta,
                           X = dat[,!(names(dat) %in% c("y", "delta"))],
                           SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger", xgb_grid$names),
                                             V = 5,
                                             method = "method.NNLS"),
                           HAL_control = list(n_bins = c(5,10),
                                              grid_type = c("equal_mass", "equal_range"),
                                              V = 5),
                           n_eval_pts = 1001,
                           eval_region = c(0, 115),
                           sensitivity_analysis = TRUE,
                           copula_control = list(taus = c(seq(-0.25, -0.01, by = 0.01), seq(0.01, 0.25, by = 0.01))))
primary_results <- res$primary_results %>%
  select(t, S_hat_est) %>%
  mutate(tau = 0)
sensitivity_results <- dplyr::bind_rows(res$sensitivity_results)
all_res <- dplyr::bind_rows(primary_results, sensitivity_results)
