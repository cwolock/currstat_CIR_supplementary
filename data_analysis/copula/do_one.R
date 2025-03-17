do_one <- function(tau, copula = "frank"){
  start <- Sys.time()
  if (copula == "clayton"){
    tau_to_theta <- function(tau){
      return(-2*tau/(tau - 1))
    }
    theta <- tau_to_theta(tau)
  } else if (copula == "frank"){
    tau_to_theta <- function(tau){
      thetas <- c(seq(-10, -0.01, by = 0.01), seq(0.01, 10, by = 0.01))
      taus <- rep(NA, length(thetas))
      for (i in 1:length(thetas)){
        mycop <- copula::frankCopula(param = thetas[i])
        taus[i] <- copula::tau(mycop)
      }
      return(thetas[which.min(abs(taus - tau))])
    }
    theta <- tau_to_theta(tau)
  }

  # dat <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/long_covid_truncated_120_021825_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")
  # nuisances <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/copula/saved_nuisances.rds")
  dat <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/long_covid_truncated_120_021825_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")
  nuisances <- readRDS("/home/cwolock/currstat_CIR_supplementary/data_analysis/copula/saved_nuisances.rds")

  dat <- dat %>% select(-record_id)
  names(dat)[names(dat) == "time_event"] <- "y"
  names(dat)[names(dat) == "resolution"] <- "delta"
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

  # quant <- quantile(dat$y[dat$y < 120], probs = 2^(1/theta), na.rm = TRUE)

  res <- survML::currstatCIR_copula_saved_nuisances(time = dat$y,
                                                    event = dat$delta,
                                                    X = dat[,!(names(dat) %in% c("y", "delta"))],
                                                    mu_n = nuisances$mu_n,
                                                    f_sIx_n = nuisances$f_sIx_n,
                                                    f_s_n = nuisances$f_s_n,
                                                    F_sIx_n = nuisances$F_sIx_n,
                                                    Riemann_grid = nuisances$Riemann_grid,
                                                    n_eval_pts = 1001,
                                                    eval_region = c(0, 115),
                                                    theta = theta,
                                                    copula = copula)
  res$tau <- tau
  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime
  res$copula <- copula
  return(res)

}
