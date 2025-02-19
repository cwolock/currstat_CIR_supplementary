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

dat <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120.rds")

names(dat)[names(dat) == "time_event"] <- "y"
names(dat)[names(dat) == "resolution"] <- "delta"

# xgboost tuning parameters
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
                           eval_region = c(0, 115))


# res <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/chu_lab/susan/data/CIR_results_correct_063024_trunc120_window115rds")

# responders <- dat %>% filter(!is.na(delta))

# # BY-HAND
# fit <- isoreg(responders$y, responders$delta)
# xvals <- sort(fit$x)
# yvals <- fit$yf
# fn <- stepfun(xvals, c(yvals[1], yvals))
# res$npmle <- apply(matrix(res$x), MARGIN = 1, FUN = fn)
#
# # USING ICENREG PACKAGE --- yields identical results as above
# icen_form <- icenReg::cs2ic(time = responders$y,
#                eventOccurred = as.logical(responders$delta))
# fit_np <- icenReg::ic_np(icen_form)
# # fit_sp <- icenReg::ic_sp(icen_form ~ age_bin + core_sex +
# #                            symp_resp + symp_gi + symp_systemic + symp_tired +
# #                            comorbid_cardio + comorbid_resp + comorbid_immun +
# #                            comorbid_allergy +
# #                            days_since_last_vax_cat + viral_load,
# #                          model = "ph",
# #                          bs_samples = 50,
# #                          data = responders)
#
#
# # USING SURVIVAL PACKAGE --- yields similar but not identical results. uses a different algorithm, I guess?
# icen_form[icen_form == 0] <- -Inf
# surv_obj <- Surv(time = icen_form[,1], time2 = icen_form[,2], type = "interval2")
# fit_surv <- survfit(surv_obj ~ 1)
# res$surv_npmle <- NA
# for (i in 1:nrow(res)){
#   closest <- which.min(abs(fit_surv$time - res$x[i]))
#   res$surv_npmle[i] <- 1 - fit_surv$surv[closest]
# }


saveRDS(res$results, "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/CIR_results_021825_trunc120_window115_noinconclusives.rds")
