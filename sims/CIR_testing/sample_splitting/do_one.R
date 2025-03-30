do_one <- function(n, missing_bound = 1.65, method, eval_upper_bound = 1.5){
  start <- Sys.time()
  beta_int <- 0.4
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3] - beta_int*w[,2]*w[,3]))
  y <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int/5*w[,1]*w[,2] + beta_int/5*w[,1]*w[,3] - beta_int/5*w[,2]*w[,3]))

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


  if (method == "sample_split"){
    res <- survML::currstatCIR(time = dat$y,
                               event = dat$delta,
                               X = dat[,3:5],
                               SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                 V = 5,
                                                 method = "method.NNLS"),
                               HAL_control = list(n_bins = c(5,10),
                                                  grid_type = c("equal_mass", "equal_range"),
                                                  V = 5),
                               sample_split = TRUE,
                               eval_region = eval_region)$primary_results
    res$S_hat_est <- 1 - res$S_hat_est
    temp_cil <- res$S_hat_cil
    temp_ciu <- res$S_hat_ciu
    res$S_hat_cil <- 1 - temp_ciu
    res$S_hat_ciu <- 1 - temp_cil
  } else if (method == "no_split"){
    res <- survML::currstatCIR(time = dat$y,
                               event = dat$delta,
                               X = dat[,3:5],
                               SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                 V = 5,
                                                 method = "method.NNLS"),
                               HAL_control = list(n_bins = c(5,10),
                                                  grid_type = c("equal_mass", "equal_range"),
                                                  V = 5),
                               eval_region = eval_region)$primary_results
    res$S_hat_est <- 1 - res$S_hat_est
    temp_cil <- res$S_hat_cil
    temp_ciu <- res$S_hat_ciu
    res$S_hat_cil <- 1 - temp_ciu
    res$S_hat_ciu <- 1 - temp_cil
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
                scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3] - beta_int*w[,2]*w[,3]))
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
