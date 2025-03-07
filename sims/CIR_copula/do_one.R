do_one <- function(n, tau){
  tau_to_theta <- function(tau){
    return(-2*tau/(tau - 1))
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

  if (theta == 0){
    t <- rweibull(n = n,
                  shape = 0.75,
                  scale = weib_scale)
  } else{
    F_Y_of_y <- pweibull(y,
                         shape = 0.75,
                         scale = weib_scale)
    u <- runif(n = n, min = 0, max = 1)
    h_inverse_of_F_Y_of_y <- (1 + F_Y_of_y^(-theta)*(u^(-theta/(theta + 1))-1))^(-1/theta)
    F_inverse_of_h_inverse_of_F_Y_of_y <- qweibull(p = h_inverse_of_F_Y_of_y,
                                                   shape = 0.75, scale = weib_scale)
    t <- F_inverse_of_h_inverse_of_F_Y_of_y
    # mycop <- claytonCopula(param = theta, dim = 2)
    #mycop <- claytonCopula(param = theta, dim = 2)
    # u <- rCopula(n, mycop)
    # y <- qweibull(p = u[,2],
    #               shape = 0.75,
    #               scale = weib_scale)#exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3]))
    # t <- qweibull(p = u[,1],
    #               shape = 0.75,
    #               scale = weib_scale)#exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3]))
  }

  problem <- ifelse(theta < 0,
                    max(qweibull(2^(1/theta), shape = 0.75, scale = weib_scale)),
                    0)

  kendalls <- cor(t, y, method = "kendall")
  # kendalls1 <- cor(t[w[,1] == 1], y[w[,1] == 1], method = "kendall")
  # print(kendalls1)
  # kendalls0 <- cor(t[w[,1] == -1], y[w[,1] == -1], method = "kendall")
  # print(kendalls0)
  # print(tau(copula = mycop))

  # mycop <- claytonCopula(param = theta, dim = 2)
  # u <- rCopula(n, mycop)

  # tau = theta/(theta + 2)

  # inverse cdf transform to get exponential marginals
  # t <- qweibull(p = u[,1],
  #               shape = 0.75,
  #               scale = exp(0.4*w[,1] - 0.2*w[2] + 0.1*w[,3]))


  # t <- rweibull(n,
  # shape = 0.75,
  # scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int*w[,1]*w[,2] + beta_int*w[,1]*w[,3] - beta_int*w[,2]*w[,3]))
  # y <- rweibull(n,
  # shape = 0.75,
  # scale = exp(0.4*w[,1] - 0.2*w[,2] + 0.1*w[,3] + beta_int/5*w[,1]*w[,2] + beta_int/5*w[,1]*w[,3] - beta_int/5*w[,2]*w[,3]))

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
  eval_region <- c(problem, eval_upper_bound+0.125)

  res_nocop <- survML::currstatCIR(time = dat$y,
                                   event = dat$delta,
                                   X = dat[,3:5],
                                   SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                     V = 5,
                                                     method = "method.NNLS"),
                                   HAL_control = list(n_bins = c(5,10),
                                                      grid_type = c("equal_mass", "equal_range"),
                                                      V = 5),
                                   eval_region = eval_region)
  res_nocop$method <- "nocopula"
  res <- res_nocop
  if (theta != 0){
    res_cop <- survML::currstatCIR_copula(time = dat$y,
                                          event = dat$delta,
                                          X = dat[,3:5],
                                          SL_control = list(SL.library = c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger"),
                                                            V = 5,
                                                            method = "method.NNLS"),
                                          HAL_control = list(n_bins = c(5,10),
                                                             grid_type = c("equal_mass", "equal_range"),
                                                             V = 5),
                                          eval_region = eval_region,
                                          theta = theta)
    res_cop$method <- "copula"
    # res <- res_cop
    res <- bind_rows(res, res_cop)
  }

  res$S_hat_est <- 1 - res$S_hat_est

  names(res) <- c("y", "cdf_estimate", "cil", "ciu", "method")
  res$n <- n
  res$n_actual <- sum(!is.na(dat$delta))
  res$theta <- theta

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
  sample_taus <- rep(NA, nrow(res))
  for (i in 1:nrow(res)){
    index <- which.min(abs(res$y[i] - pop_taus))
    sample_truths[i] <- pop_truths[index]
    sample_taus[i] <- pop_taus[index]
  }

  res$truth <- sample_truths
  res$tau <- sample_taus
  res$emp_kendall <- kendalls
  res$theo_kendall <- theo_kendall
  res$lower_bound <- problem

  res <- res %>% filter(y <= eval_upper_bound)

  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime

  rownames(res) <- NULL
  return(res)
}
