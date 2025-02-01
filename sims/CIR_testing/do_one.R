do_one <- function(n, missing_bound, method, eval_upper_bound){
  start <- Sys.time()
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2]))
  y <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2]))

  # round c to nearest quantile of c, just so there aren't so many unique values
  quants <- quantile(y, probs = seq(0, 1, by = 0.02), type = 1)
  for (i in 1:length(y)){
    y[i] <- quants[which.min(abs(y[i] - quants))]
  }
  delta <- as.numeric(t <= y)

  dat <- data.frame(y = y, delta = delta, w1 = w[,1], w2 = w[,2], w3 = w[,3])

  if (missing_bound != -100){
    dat$delta[dat$y > missing_bound] <- NA
    dat$y[dat$y > missing_bound] <- missing_bound
  }
  eval_region <- c(0, eval_upper_bound+0.125)


  if (method == "npmle"){
    dat_cc <- dat %>% filter(!is.na(delta))
    y_vals <- sort(unique(dat_cc$y))
    F_n <- stats::ecdf(dat_cc$y)
    F_n_inverse <- function(t){
      stats::quantile(dat_cc$y, probs = t, type = 1)
    }
    eval_cdf_upper <- mean(dat_cc$y <= eval_region[2])
    eval_cdf_lower <- mean(dat_cc$y <= eval_region[1])
    x <- sapply(seq(eval_cdf_lower, eval_cdf_upper, length.out = 101), F_n_inverse)
    fit <- isoreg(dat_cc$y, dat_cc$delta)
    xvals <- sort(fit$x)
    yvals <- fit$yf
    fn <- stepfun(xvals, c(yvals[1], yvals))
    res <- data.frame(t = x)
    res$cdf_estimate = apply(matrix(x), MARGIN = 1, FUN = fn)
    res$cil <- NA
    res$ciu <- NA
  } else if (method == "npmle_survival"){
    dat_cc <- dat %>% filter(!is.na(delta))

    icen_form <- icenReg::cs2ic(time = dat_cc$y,
                                eventOccurred = as.logical(dat_cc$delta))
    icen_form[icen_form == 0] <- -Inf
    surv_obj <- survival::Surv(time = icen_form[,1], time2 = icen_form[,2], type = "interval2")
    fit_surv <- survfit(surv_obj ~ 1)

    F_n <- stats::ecdf(dat_cc$y)
    F_n_inverse <- function(t){
      stats::quantile(dat_cc$y, probs = t, type = 1)
    }
    eval_cdf_upper <- mean(dat_cc$y <= eval_region[2])
    eval_cdf_lower <- mean(dat_cc$y <= eval_region[1])
    x <- sapply(seq(eval_cdf_lower, eval_cdf_upper, length.out = 101), F_n_inverse)
    res <- data.frame(t = x)

    res$cdf_estimate <- NA
    res$cil <- NA
    res$ciu <- NA
    for (i in 1:nrow(res)){
      closest <- which.min(abs(fit_surv$time - res$t[i]))
      res$cdf_estimate[i] <- 1 - fit_surv$surv[closest]
      res$cil[i] <- 1 - fit_surv$upper[closest]
      res$ciu[i] <- 1 - fit_surv$lower[closest]
    }
  } else if (method == "cc"){
    dat_cc <- dat %>% filter(!is.na(delta))
    res <- survML::currstatCIR(time = dat_cc$y,
                               event = dat_cc$delta,
                               X = dat_cc[,3:5],
                               SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                 V = 5,
                                                 method = "method.NNLS"),
                               HAL_control = list(n_bins = c(5,10),
                                                  grid_type = c("equal_mass", "equal_range"),
                                                  V = 5),
                               eval_region = eval_region)
  } else if (method == "extended"){
    res <- survML::currstatCIR(time = dat$y,
                               event = dat$delta,
                               X = dat[,3:5],
                               SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                 V = 5,
                                                 method = "method.NNLS"),
                               HAL_control = list(n_bins = c(5,10),
                                                  grid_type = c("equal_mass", "equal_range"),
                                                  V = 5),
                               eval_region = eval_region)
  } else if (method == "extended_smalllib"){
    res <- survML::currstatCIR(time = dat$y,
                               event = dat$delta,
                               X = dat[,3:5],
                               SL_control = list(SL.library = c("SL.mean", "SL.glm"),
                                                 V = 5,
                                                 method = "method.NNLS"),
                               HAL_control = list(n_bins = c(5,10),
                                                  grid_type = c("equal_mass", "equal_range"),
                                                  V = 5),
                               eval_region = eval_region)
  }


  names(res) <- c("y", "cdf_estimate", "cil", "ciu")
  res$n = n
  res$missing_bound <- missing_bound
  res$method <- method
  res$eval_upper_bound <- eval_upper_bound
  res$n_actual <- sum(!is.na(dat$delta))

  n <- 5e5
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5) - 1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2]))
  pop_truths <- seq(0, 1, length.out = 501)
  pop_taus <- quantile(t, probs = pop_truths)

  sample_truths <- rep(NA, nrow(res))
  sample_taus <- rep(NA, nrow(res))
  for (i in 1:nrow(res)){
    index <- which.min(abs(res$y[i] - pop_taus))
    sample_truths[i] <- pop_truths[index]
    sample_taus[i] <- pop_taus[index]
  }

  res$truth <- sample_truths
  res$tau <- sample_taus

  res <- res %>% filter(y <= eval_upper_bound)

  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime

  rownames(res) <- NULL
  return(res)
}
