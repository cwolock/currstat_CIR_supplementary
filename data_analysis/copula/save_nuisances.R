set.seed(72724)
library(ranger)
library(earth)
library(SuperLearner)
library(dplyr)
library(haldensify)
#library(ChernoffDist)
library(fdrtool)
library(xgboost)
library(survML)

dat <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_021825_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")

dat <- dat %>% select(-record_id)

names(dat)[names(dat) == "time_event"] <- "y"
names(dat)[names(dat) == "resolution"] <- "delta"

# xgboost tuning parameters
tune <- list(ntrees = c(250, 500, 1000), max_depth = c(1,2), minobspernode = 10, shrinkage = 0.01)
xgb_grid <- create.SL.xgboost(tune = tune)
SL_control <- list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                  V = 5,
                  method = "method.NNLS")
HAL_control <- list(n_bins = c(5,10),
                   grid_type = c("equal_mass", "equal_range"),
                   V = 5)

event <- dat$delta
time <- dat$y
X <- dat %>% select(-c(delta, y))

s <- as.numeric(!is.na(event))

time[s == 0] <- max(time, na.rm = TRUE)

dat <- list(delta = event,
            y = time,
            s = s,
            w = X)

dat$w <- data.frame(stats::model.matrix(stats::as.formula(paste("~",
                                                                paste(names(dat$w),
                                                                      collapse =  "+"))),
                                        dat$w)[,-1])
names(dat$w) <- paste("w", 1:ncol(dat$w), sep="")

# estimate conditional density (only among observed)
cond_density_fit <- survML:::construct_f_sIx_n(dat = dat,
                                      HAL_control = HAL_control)
f_sIx_n <- cond_density_fit$fnc
Riemann_grid <- c(0, cond_density_fit$breaks)
# estimate marginal density (marginalizing the conditional density over whole sample)
f_s_n <- survML:::construct_f_s_n(dat = dat, f_sIx_n = f_sIx_n)
# estimate density ratio
g_n <- survML:::construct_g_n(f_sIx_n = f_sIx_n, f_s_n = f_s_n)
# estimate conditional CDF of response times
F_sIx_n <- survML:::construct_F_sIx_n(dat = dat, f_sIx_n = f_sIx_n, Riemann_grid = Riemann_grid)

# estimate outcome regression (only among observed)
mu_n <- survML:::construct_mu_n(dat = dat, SL_control = SL_control, Riemann_grid = Riemann_grid)

y_vals <- sort(unique(dat$y))

# Use the empirical cdf for F_n
F_n <- stats::ecdf(dat$y)
F_n_inverse <- function(t){
  stats::quantile(dat$y, probs = t, type = 1)
}


saveRDS(list(f_sIx_n = f_sIx_n, f_s_n = f_s_n, Riemann_grid = Riemann_grid, g_n = g_n, F_sIx_n = F_sIx_n, mu_n = mu_n,
             F_n = F_n, F_n_inverse = F_n_inverse),
        file = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/saved_nuisances.rds")


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
