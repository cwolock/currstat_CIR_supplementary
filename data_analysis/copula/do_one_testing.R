do_one <- function(tau){
  tau_to_theta <- function(tau){
    return(-2*tau/(tau - 1))
  }
  theta <- tau_to_theta(tau)

  dat <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/long_covid_truncated_120_021825_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")
  nuisances <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/saved_nuisances.rds")
  # dat <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/long_covid_truncated_120_021825_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")
  # nuisances <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/saved_nuisances.rds")

  dat <- dat %>% select(-record_id)

  names(dat)[names(dat) == "time_event"] <- "y"
  names(dat)[names(dat) == "resolution"] <- "delta"
  #
  # event <- dat$delta
  # time <- dat$y
  # X = dat %>% select(-c(delta, y))
  # s <- as.numeric(!is.na(event))
  #
  # time[s == 0] <- max(time, na.rm = TRUE)
  #
  # dat <- list(delta = event,
  #             y = time,
  #             s = s,
  #             w = X)
  #
  # dat$w <- data.frame(stats::model.matrix(stats::as.formula(paste("~",
  #                                                                 paste(names(dat$w),
  #                                                                       collapse =  "+"))),
  #                                         dat$w)[,-1])
  # names(dat$w) <- paste("w", 1:ncol(dat$w), sep="")
  #
  # y_vals <- sort(unique(dat$y))
  # dat$y[is.na(dat$delta)] <- max(dat$y, na.rm = TRUE)

  # F_n <- stats::ecdf(dat$y)
  # F_n_inverse <- function(t){
  #   stats::quantile(dat$y, probs = t, type = 1)
  # }
  # tau_to_theta <- function(tau){
  #   return(-2*tau/(tau - 1))
  # }
  # theta <- tau_to_theta(tau)
  # F_n_inverse(2^(1/theta))


  res <- survML::currstatCIR_copula_saved_nuisances(time = dat$y,
                                                    event = dat$delta,
                                                    X = dat[,!(names(dat) %in% c("y", "delta"))],
                                                    mu_n = nuisances$mu_n,
                                                    f_sIx_n = nuisances$f_sIx_n,
                                                    f_s_n = nuisances$f_s_n,
                                                    # g_n = nuisances$g_n,
                                                    Riemann_grid = nuisances$Riemann_grid,
                                                    n_eval_pts = 1001,
                                                    eval_region = c(0, 115),
                                                    theta = theta)
  res$tau <- tau

  return(res)

}
