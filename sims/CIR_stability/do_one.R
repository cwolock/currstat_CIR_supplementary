do_one <- function(n, method){
  n_train <- n
  missing_bound <- 1.65
  eval_upper_bound <- 1.5
  beta_int <- 0.5
  start <- Sys.time()
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3]))
  # scale = exp(0.4*w[,1] - 0.2*w[,2] + beta_int*w[,1]*w[,2]))
  y <- rweibull(n,
                shape = 0.75,
                scale = exp(0.4*w[,1] - 0.2*w[,2]))# + beta_int*w[,1]*w[,2]))
  # y <- rlnorm(n, meanlog = 0.4*w[,1] - 0.2*w[,2] + beta_int*w[,1]*w[,2], sdlog = 1)

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

  if (method == "multi"){
    methods <- c("glm_unif", "gam_unif", "ranger_unif", "xgboost_unif")
  } else{
    methods <- method
  }

  for (j in 1:length(methods)){
    method <- methods[j]
    if (method == "SL1_HAL"){
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
    } else if (method == "SL2_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.gam"),
                                                   V = 5,
                                                   method = "method.NNLS"),
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 eval_region = eval_region)
    } else if (method == "SL3_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.gam", "SL.earth"),
                                                   V = 5,
                                                   method = "method.NNLS"),
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 eval_region = eval_region)
    } else if (method == "SL4_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.gam", "SL.earth", "SL.ranger"),
                                                   V = 5,
                                                   method = "method.NNLS"),
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 eval_region = eval_region)
    } else if (method == "SL4_parametric"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.gam", "SL.earth", "SL.ranger"),
                                                   V = 5,
                                                   method = "method.NNLS"),
                                 eval_region = eval_region,
                                 g_nuisance = "parametric")
    } else if (method == "glm_parametric"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 g_nuisance = "parametric",
                                 mu_nuisance = "glm")
    } else if (method == "glm_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 mu_nuisance = "glm")
    } else if (method == "gam_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 mu_nuisance = "gam")
    } else if (method == "ranger_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 mu_nuisance = "ranger")
    } else if (method == "xgboost_HAL"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 HAL_control = list(n_bins = c(5,10),
                                                    grid_type = c("equal_mass", "equal_range"),
                                                    V = 5),
                                 mu_nuisance = "xgboost")
    }  else if (method == "glm_unif"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 g_nuisance = "uniform",
                                 mu_nuisance = "glm")
    } else if (method == "gam_unif"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 g_nuisance = "uniform",
                                 mu_nuisance = "gam")
    } else if (method == "ranger_unif"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 g_nuisance = "uniform",
                                 mu_nuisance = "ranger")
    } else if (method == "xgboost_unif"){
      res <- survML::currstatCIR(time = dat$y,
                                 event = dat$delta,
                                 X = dat[,3:5],
                                 eval_region = eval_region,
                                 g_nuisance = "uniform",
                                 mu_nuisance = "xgboost")
    }

    mu_n <- res$mu_n
    f_sIx_n <- res$f_sIx_n

    res <- res$results
    res$S_hat_est <- 1 - res$S_hat_est
    temp_cil <- res$S_hat_cil
    temp_ciu <- res$S_hat_ciu
    res$S_hat_cil <- 1 - temp_ciu
    res$S_hat_ciu <- 1 - temp_cil

    names(res) <- c("y", "cdf_estimate", "cil", "ciu")
    res$n <- n_train
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
                  scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3]))
    # scale = exp(0.4*w[,1] - 0.2*w[,2] + beta_int*w[,1]*w[,2]))
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

    n <- 1e3
    w <- cbind(2*rbinom(n, size = 1, prob = 0.5) - 1,
               2*rbinom(n, size = 1, prob = 0.5)-1,
               2*rbinom(n, size = 1, prob = 0.5)-1)
    sample_MSEs_mu <- rep(NA, length(sample_taus))
    sample_MSEs_g <- rep(NA, length(sample_taus))
    for (i in 1:length(sample_taus)){
      this_tau <- sample_taus[i]
      this_truth_mu <- pweibull(q = this_tau, shape = 0.75,
                                scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3]))
      # this_truth_mu <- pweibull(q = this_tau, shape = 0.75, scale = exp(0.4*w[,1] - 0.2*w[,2] + beta_int*w[,1]*w[,2] ))
      this_mu_n <- apply(X = w, MARGIN = 1, FUN = function(x) mu_n(y = this_tau, w = x))
      sample_MSEs_mu[i] <- mean((this_truth_mu - this_mu_n)^2)
      this_truth_g <- dweibull(x = this_tau, shape = 0.75, scale = exp(0.4*w[,1] - 0.2*w[,2]))# + beta_int*w[,1]*w[,2]))
      # this_truth_g <- dlnorm(x = this_tau, meanlog = 0.4*w[,1] - 0.2*w[,2] + beta_int*w[,1]*w[,2], sdlog = 1)
      this_g_n <- apply(X = w, MARGIN = 1, FUN = function(x) f_sIx_n(y = this_tau, w = x))
      sample_MSEs_g[i] <- mean((this_truth_g - this_g_n)^2)
    }
    res$MSE_mu <- sample_MSEs_mu
    res$MSE_g <- sample_MSEs_g

    if (j == 1){
      all_res <- res
    } else{
      all_res <- rbind(all_res, res)
    }
  }

  res <- all_res
  res <- res %>% filter(y <= eval_upper_bound)

  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime

  rownames(res) <- NULL
  return(res)
}
