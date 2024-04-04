# source("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/utils.R")
do_one <- function(n, nonresponse, method){
  start <- Sys.time()
  # landmark times at which to evaluate survival curve
  # taus <- c(0.0485, 0.1868, 0.6081, 1.5529, 3.1122)
  # truths <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5) - 1,#sample(c(-2, -1.5, -1, 0, 1, 1.5, 2), size = n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.3, 0.2, 0.1, 0.05)),
             2*rbinom(n, size = 1, prob = 0.5)-1,
             #2*rbinom(n, size = 1, prob = 0.5)-1,
             #2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
             #sample(c(-1, -0.5, 0, 0.5, 1), size = n, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
             #sample(c(-1, -0.5, 0, 0.5, 1), size = n, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)))
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.2*w[,1] - 0.1*w[,2]))
  y <- rweibull(n,
                shape = 0.75,
                scale = exp(0.2*w[,1] - 0.1*w[,2]))
  # round c to nearest quantile of c, just so there aren't so many unique values
  quants <- quantile(y, probs = seq(0, 1, by = 0.02), type = 1)
  for (i in 1:length(y)){
    y[i] <- quants[which.min(abs(y[i] - quants))]
  }
  delta <- as.numeric(t <= y)

  dat <- data.frame(y = y, delta = delta, w1 = w[,1], w2 = w[,2], w3 = w[,3])#, w4 = w[,4], w5 = w[,5])

  if (nonresponse == "mcar"){
    miss_prob <- 0.5
    dat$s <- rbinom(n, size = 1, prob = miss_prob)
  } else if (nonresponse == "mar"){
    miss_prob <- exp(0.5*w[,1] - 0.3*w[,2] + 0.1*w[,2]*w[,3])/(1 + exp(0.5*w[,1] - 0.3*w[,2] + 0.1*w[,2]*w[,3]))
    dat$s <- rbinom(n, size = 1, prob = miss_prob)
  }
  if (nonresponse == "none"){
    dat$s <- 1
  }
#
#   dat$delta[dat$s == 0] <- 0
#   dat$y[dat$s == 0] <- 0

  # if (nonresponse){
  #   dat <- dat %>% filter(s == 1)
  # }
  res <- survML::currstatCIR(time = dat$y,
                     event = dat$delta,
                     W = dat[,3:5],
                     SL_control = list(SL.library = c("SL.mean", "SL.glm"),
                                       V = 5,
                                       method = "method.NNLS"),
                     HAL_control = list(n_bins = c(5),
                                        grid_type = c("equal_mass", "equal_range"),
                                        V = 5),
                     missing_method = method)
  # res <- run_CIR(dat, method = ifelse(nonresponse == "none", "ignore", method))

  # inds_to_keep <- apply(as.matrix(c(0.1, 0.25, 0.5, 0.75, 0.9)), MARGIN = 1, FUN = function(x) which.min(abs(x - res$x_quants)))
  # inds_to_keep <- apply(as.matrix(taus), MARGIN = 1, FUN = function(x) which.min(abs(x - res$x)))
  # inds_to_keep <- apply(as.matrix(seq()))
  # res <- res[inds_to_keep,]
  names(res) <- c("y", "y_quant", "cdf_estimate", "cil", "ciu")
  res$n = n
  res$nonresponse <- nonresponse
  res$method <- method
  res$n_actual <- sum(dat$s)

  n <- 1e6
  w <- cbind(2*rbinom(n, size = 1, prob = 0.5) - 1,
             2*rbinom(n, size = 1, prob = 0.5)-1,
             2*rbinom(n, size = 1, prob = 0.5)-1)
  t <- rweibull(n,
                shape = 0.75,
                scale = exp(0.2*w[,1] - 0.1*w[,2]))
  pop_truths <- seq(0, 1, length.out = 101)
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

  # truths <- apply(matrix(res$y), MARGIN = 1, FUN = function(x){
    # return(mean(t <= t[which.min(abs(t - x))]))
  # })

  # res$truth <- truths
  end <- Sys.time()
  runtime <- difftime(end, start, units = "min")
  res$runtime <- runtime
  return(res)
}
