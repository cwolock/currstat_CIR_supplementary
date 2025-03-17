do_one <- function(n, tau){
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

  theo_kendall <- tau
  missing_bound <- 1.65
  eval_upper_bound <- 1.5
  start <- Sys.time()
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)

  weib_scale <- exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3])

  y <- rweibull(n = n,
                shape = 0.75,
                scale = weib_scale)

  F_Y_of_y <- pweibull(y,
                       shape = 0.75,
                       scale = weib_scale)
  u <- runif(n = n, min = 0, max = 1)
  h_inverse_of_F_Y_of_y <- -(1/theta)*log(1 - (u*(1 - exp(-theta)))/(exp(-theta * F_Y_of_y) + u*(1 - exp(-theta*F_Y_of_y))))

  F_inverse_of_h_inverse_of_F_Y_of_y <- qweibull(p = h_inverse_of_F_Y_of_y,
                                                 shape = 0.75, scale = weib_scale)
  t <- F_inverse_of_h_inverse_of_F_Y_of_y

  emp_tau<- cor(t, y, method = "kendall")

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

  res <- survML::currstatCIR(time = dat$y,
                             event = dat$delta,
                             X = dat[,3:5],
                             SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                               V = 5,
                                               method = "method.NNLS"),
                             HAL_control = list(n_bins = c(5,10),
                                                grid_type = c("equal_mass", "equal_range"),
                                                V = 5),
                             eval_region = eval_region,
                             sensitivity_analysis = TRUE,
                             copula_control = list(taus = tau))
  res_nocop <- res$primary_results %>%
    select(t, S_hat_est) %>%
    mutate(tau = tau, method = "nocopula")
  res_cop <- res$sensitivity_results[[1]] %>% mutate(method = "copula")
  res <- bind_rows(res_nocop, res_cop)

  res$S_hat_est <- 1 - res$S_hat_est

  names(res) <- c("y", "cdf_estimate", "tau", "method")
  res$n <- n
  res$n_actual <- sum(!is.na(dat$delta))

  n <- 5e5
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5) - 1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  weib_scale <- exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3])
  t <- rweibull(n,
                shape = 0.75,
                scale = weib_scale)
  pop_truths <- seq(0, 1, length.out = 501)
  pop_taus <- quantile(t, probs = pop_truths)

  sample_truths <- rep(NA, nrow(res))
  for (i in 1:nrow(res)){
    index <- which.min(abs(res$y[i] - pop_taus))
    sample_truths[i] <- pop_truths[index]
  }

  res$truth <- sample_truths
  res$emp_tau <- emp_tau

  res <- res %>% filter(y <= eval_upper_bound)

  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime

  rownames(res) <- NULL
  return(res)
}
