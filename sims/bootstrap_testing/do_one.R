do_one <- function(n, B, missing_bound){
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
  # round c to nearest quantile of c
  quants <- quantile(y, probs = seq(0, 1, by = 0.02), type = 1)
  for (i in 1:length(y)){
    y[i] <- quants[which.min(abs(y[i] - quants))]
  }
  delta <- t <= y

  dat <- data.frame(y = y, delta = delta, w1 = w[,1], w2 = w[,2], w3 = w[,3])

  if (missing_bound != -100){
    dat$delta[dat$y > missing_bound] <- NA
    dat$y[dat$y > missing_bound] <- missing_bound
  }

  dat <- dat %>% mutate(s = !is.na(delta))

  dat <- dat %>% filter(s == 1)

  cs_fit <- icenReg::ic_sp(icenReg::cs2ic(time = y, eventOccurred = delta)~ w1 + w2 + w3,
                           model = "ph",
                           bs_samples = 0,
                           data = dat)
  boot_ests <- matrix(NA, nrow = B, ncol = 3)
  for (i in 1:B){
    inds <- sample(1:nrow(dat), size= nrow(dat), replace = TRUE)
    boot_dat <- dat[inds,]
    boot_fit <- icenReg::ic_sp(icenReg::cs2ic(time = y, eventOccurred = delta)~ w1 + w2 + w3,
                               model = "ph",
                               bs_samples = 0,
                               data = boot_dat)
    boot_ests[i,] <- c(boot_fit$coefficients[1],
                       boot_fit$coefficients[2],
                       boot_fit$coefficients[3])
  }

  end <- Sys.time()
  runtime <- difftime(end, start, units = "mins")
  return(data.frame(runtime = runtime,
                    n = n,
                    n_actual = nrow(dat),
                    B = B,
                    missing_bound = missing_bound,
                    x1_est = cs_fit$coefficients[1],
                    x1_se = sqrt(var(boot_ests[,1])),
                    x1_025 = quantile(boot_ests[,1], probs = 0.025),
                    x1_975 = quantile(boot_ests[,1], probs = 0.975),
                    x2_est = cs_fit$coefficients[2],
                    x2_se = sqrt(var(boot_ests[,2])),
                    x2_025 = quantile(boot_ests[,2], probs = 0.025),
                    x2_975 = quantile(boot_ests[,2], probs = 0.975),
                    x3_est = cs_fit$coefficients[3],
                    x3_se = sqrt(var(boot_ests[,3])),
                    x3_025 = quantile(boot_ests[,3], probs = 0.025),
                    x3_975 = quantile(boot_ests[,3], probs = 0.975)))
}
