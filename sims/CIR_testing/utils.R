# sampling weights
construct_alpha_n <- function(dat, SL.library){
  w_distinct <- dplyr::distinct(dat$w)
  w_distinct <- cbind("w_index"=c(1:nrow(w_distinct)), w_distinct)
  newW <- w_distinct
  newW$w_index <- NULL
  model_sl <- SuperLearner::SuperLearner(
    Y = dat$s,
    X = dat$w,
    newX = newW,
    family = "binomial",
    SL.library = SL.library
  )
  pred <- as.numeric(model_sl$SL.predict)
  
  fnc <- function(w) {
    cond <- paste0("round(w1,5)==",round(w[1],5))
    for (i in c(2:length(w))) {
      cond <- paste0(cond," & round(w",i,",5)==",round(w[i],5))
    }
    filtered <- dplyr::filter(w_distinct, eval(parse(text=cond)))
    index <- filtered$w_index
    if (length(index)!=1) {
      stop(paste0("w=c(",paste(w, collapse=","),")"))
    }
    return(pred[index])
  }
  
  return(fnc)
}

# Nuisance estimator constructor: mu_n
construct_mu_n <- function(dat, SL.library, Riemann_grid) {
  # Construct newX (all distinct combinations of X and S)
  w_distinct <- dplyr::distinct(dat$w)
  w_distinct <- cbind("w_index"=c(1:nrow(w_distinct)), w_distinct)
  y_distinct <- sort(unique(round(c(dat$y, Riemann_grid), digits = 5)))
  newW <- expand.grid(w_index=w_distinct$w_index, Yprime=y_distinct)
  newW <- dplyr::inner_join(w_distinct, newW, by="w_index")
  newW$w_index <- NULL
  
  model_sl <- SuperLearner::SuperLearner(
    Y = dat$delta[dat$s == 1],
    X = cbind(dat$w[dat$s == 1,], Yprime=dat$y[dat$s == 1]),
    newX = newW,
    family = "binomial",
    SL.library = SL.library
  )
  pred <- as.numeric(model_sl$SL.predict)
  rm(dat,model_sl)
  newW$index <- c(1:nrow(newW))
  
  fnc <- function(y,w) {
    cond <- paste0("round(Yprime,5)==",round(y,5))
    for (i in c(1:length(w))) {
      cond <- paste0(cond," & round(w",i,",5)==",round(w[i],5))
    }
    index <- (dplyr::filter(newW, eval(parse(text=cond))))$index
    if (length(index)!=1) {
      stop(paste0("y=",y,", w=c(",paste(w, collapse=","),")"))
    }
    return(pred[index])
  }
  
  return(fnc)
  
}

construct_f_sIx_n <- function(dat){
  
  # set number of density bins
  n_bins <- c(5, 10)
  # fit hal
  haldensify_fit <- haldensify(A = dat$y[dat$s == 1],
                               W = dat$w[dat$s == 1,],
                               n_bins = n_bins,
                               grid_type = c("equal_range", "equal_mass"))
  
  w_distinct <- dplyr::distinct(dat$w)
  w_distinct <- cbind("w_index"=c(1:nrow(w_distinct)), w_distinct)
  # only get predictions at the breakpoints, since estimator is piecewise constant
  y_distinct <- haldensify_fit$breaks
  newW <- expand.grid(w_index=w_distinct$w_index, y=y_distinct)
  newW <- dplyr::inner_join(w_distinct, newW, by="w_index")
  newW$w_index <- NULL
  
  pred <- predict(haldensify_fit, new_A = newW$y, new_W = newW[,-ncol(newW)])
  
  newW$index <- c(1:nrow(newW))
  
  breaks <- haldensify_fit$breaks
  
  fnc <- function(y,w) {
    # if (s <= min(haldensify_fit$breaks)){
    # left_s <- min(haldensify_fit$breaks)
    # } else{
    left_y <- max(breaks[breaks <= max(y, min(breaks))])
    # }
    
    cond <- paste0("round(y,5)==",round(left_y,5))
    for (i in c(1:length(w))) {
      cond <- paste0(cond," & round(w",i,",5)==",round(w[i],5))
    }
    index <- (dplyr::filter(newW, eval(parse(text=cond))))$index
    # if (length(index)!=1) {
    # stop(paste0("s=",s,", x=c(",paste(x, collapse=","),")"))
    # }
    return(pred[index])
  }
  
  return(list(fnc = fnc, breaks = haldensify_fit$breaks))
}

construct_f_s_n <- function(dat, f_sIx_n) {
  uniq_y <- sort(unique(dat$y))
  f_sIx_ns <- sapply(uniq_y, function(y){
    mean(apply(dat$w, 1, function(w) {
      f_sIx_n(y,as.numeric(w))
    }))})
  
  fnc <- function(y){
    f_sIx_ns[uniq_y == y]
  }
}

construct_g_n <- function(f_sIx_n, f_s_n) {
  function(y,w) { f_sIx_n(y,w) / f_s_n(y) }
}

# One-step estimator constructor: Gamma_n
construct_Gamma_n <- function(dat, mu_n, g_n, alpha_n, f_sIx_n, Riemann_grid) {
  n_orig <- length(dat$y)
  dim_w <- length(dat$w)
  mu_ns <- apply(as_df(dat), 1, function(r) {
    y <- r[["y"]]
    w <- as.numeric(r)[1:dim_w]
    mu_n(y=y, w=w)
  })
  
  g_ns <- apply(as_df(dat), 1, function(r) {
    y <- r[["y"]]
    w <- as.numeric(r)[1:dim_w]
    g_n(y=y, w=w)
  })
  
  alpha_ns <- apply(as_df(dat), 1, function(r){
    w <- as.numeric(r)[1:dim_w]
    alpha_n(w = w)
  })
  
  # piece 1 maps to (\Delta - \mu_n(Y_i, W_i))/g_n(Y_i, W_i)
  piece_1 <- (dat$delta-mu_ns) / g_ns
  
  # since there aren't so many unique s values in my application
  # we can save a lot of time by only computing piece_2 on those ~100 unique
  # s values rather than all 1800 non-unique ones
  unique_y <- sort(unique(dat$y))
  
  unique_piece_2 <- sapply(unique_y, function(y) {
    sum(apply(dat$w, 1, function(w) { mu_n(y=y, w=as.numeric(w)) }))
  })
  
  # match to pre-computed values
  # piece 2 maps to \theta_n(Y_i)
  piece_2 <- sapply(dat$y, function(y) {
    # sum(apply(dat$x, 1, function(x) { mu_n(s=s, x=as.numeric(x)) }))
    unique_piece_2[unique_y == y]
  })
  w_distinct <- dplyr::distinct(dat$w)
  # if there actually is missingness to deal with
  if (sum(dat$s) != length(dat$s)){
    piece_4 <- sapply(Riemann_grid, function(y) {
      mean(apply(dat$w, 1, function(w) { mu_n(y=y, w=as.numeric(w)) }))
    })
    
    
    # diffs <- diff(Riemann_grid)
    # w_distinct <- cbind("w_index"=c(1:nrow(w_distinct)), w_distinct)
    
    # calculate the Riemann integral w.r.t. the conditional density
    # for each unique value of w
    unique_Riemann_integrals <- apply(w_distinct, MARGIN = 1, function(w){
      density_vals <- sapply(Riemann_grid, function(y){
        f_sIx_n(y = y, w = as.numeric(w))
      })
      # trunc_density_vals <- density_vals[-length(density_vals)]
      Riemann_integrals <- sapply(Riemann_grid, function(y){
        # sum(diffs[Riemann_grid <= y] * piece_4[Riemann_grid <= y][-1] * density_vals[Riemann_grid <= y][-1])
        sum(diff(Riemann_grid[Riemann_grid <= y]) * piece_4[Riemann_grid <= y][-1] * density_vals[Riemann_grid <= y][-1])
      })
      # sum(diff(Riemann_grid) * density_vals[-length(density_vals)])
      Riemann_integrals
    })
    
    unique_Riemann_integrals <- t(unique_Riemann_integrals)
  } else{
    unique_Riemann_integrals <- matrix(1, nrow = nrow(w_distinct), ncol = length(Riemann_grid))
  }
  
  fnc <- function(y) {
    piece_3 <- as.integer(dat$y<=y) * dat$s/alpha_ns
    obs_pieces <- (sum(piece_3*piece_1) + mean(piece_3*piece_2))/n_orig
    piece_4_integrals <- apply(dat$w, MARGIN = 1, FUN = function(w){
      unique_Riemann_integrals[which(colSums(t(w_distinct) == w) == ncol(w_distinct)),max(which(Riemann_grid <= y))]
    })
    unobs_pieces <- sum((1 - dat$s/alpha_ns) * piece_4_integrals)/n_orig
    # unobs_pieces <- 0 # to-do
    return(obs_pieces + unobs_pieces)
  }
  
  return(fnc)
}

construct_Phi_n <- function(dat, alpha_n, f_sIx_n, Riemann_grid){
  
  w_distinct <- dplyr::distinct(dat$w)
  # diffs <- diff(Riemann_grid)
  # w_distinct <- cbind("w_index"=c(1:nrow(w_distinct)), w_distinct)
  
  # calculate the Riemann integral w.r.t. the conditional density
  # for each unique value of w
  unique_Riemann_integrals <- apply(w_distinct, MARGIN = 1, function(w){
    density_vals <- sapply(Riemann_grid, function(y){
      f_sIx_n(y = y, w = as.numeric(w))
    })
    # trunc_density_vals <- density_vals[-length(density_vals)]
    Riemann_integrals <- sapply(Riemann_grid, function(y){
      # sum(diffs[Riemann_grid <= y] * density_vals[Riemann_grid <= y][-1])
      sum(diff(Riemann_grid[Riemann_grid <= y]) * density_vals[Riemann_grid <= y][-1])
    })
    # sum(diff(Riemann_grid) * density_vals[-length(density_vals)])
    Riemann_integrals
  })
  
  unique_Riemann_integrals <- t(unique_Riemann_integrals)
  
  # the above step seems to often produce integrals with value > 1, which shouldn't
  # be possible
  
  # w_distinct$w_index <- NULL
  
  # Riemann_integrals <- apply(dat$w, MARGIN = 1, FUN = function(w){
  #   unique_Riemann_integrals[which(colSums(t(w_distinct) == w) == ncol(w_distinct))]
  # })
  
  alpha_ns <- apply(dat$w, MARGIN = 1, FUN = alpha_n)
  
  piece_1 <- dat$s/alpha_ns
  piece_2 <- (1 - dat$s/alpha_ns)
  
  fnc <- function(y){
    
    piece_1_ind <- piece_1 * (dat$y <= y)
    piece_2_integral <- apply(dat$w, MARGIN = 1, FUN = function(w){
      unique_Riemann_integrals[which(colSums(t(w_distinct) == w) == ncol(w_distinct)),max(which(Riemann_grid <= y))]
    })
    piece_2_ind <- piece_2 * piece_2_integral
    return(mean(piece_1_ind + piece_2_ind))
  }
  
  
  # y_distinct <- unique(dat$y)
  # newW <- expand.grid(w_index=w_distinct$w_index, y=y_distinct)
  # newW <- dplyr::inner_join(w_distinct, newW, by="w_index")
  # newW$w_index <- NULL
  # 
  # unique_y <- sort(unique(dat$y))
  # 
  # unique_piece_2 <- sapply(unique_y, function(y) {
  #   sum(apply(dat$w, 1, function(w) { mu_n(y=y, w=as.numeric(w)) }))
  # })
  # 
  # # match to pre-computed values
  # # piece 2 maps to \theta_n(Y_i)
  # piece_2 <- sapply(dat$y, function(y) {
  #   # sum(apply(dat$x, 1, function(x) { mu_n(s=s, x=as.numeric(x)) }))
  #   unique_piece_2[unique_y == y]
  # })
  # 
  # newW$f_sIx_ns <- apply(newW, MARGIN = 1, FUN = function(x){
  #   
  # })
  return(fnc)
}

construct_kappa_n <- function(dat, mu_n, g_n, alpha_n){
  
  
  fnc <- function(y){
    mean(apply(dat$w, 1, function(w) {
      numer1 <- mu_n(y = y, w = as.numeric(w))
      numer <- numer1 * (1 - numer1)
      denom1 <- g_n(y = y, w = as.numeric(w))
      denom2 <- alpha_n(w = as.numeric(w))
      numer / (denom1 * denom2)
    }))
  }
  
  return(fnc)
}

construct_deriv <- function(type="m-spline", r_Mn, dir, y) {
  # r_Mn should be the theta_n function
  # dir should be ?
  # grid should be something like seq(0, 1, by = 0.01)
  if (type=="line") {
    fnc <- function(y) { r_Mn(1)-r_Mn(0) }
  } else {
    if (r_Mn(0)==r_Mn(1)) {
      fnc <- function(y) { 0 }
      warning("Estimated function is flat; variance estimation not possible.")
    }
    # Estimate entire function on grid
    r_Mns <- sapply(y, r_Mn)
    
    # Compute set of midpoints of jump points (plus endpoints)
    points_x <- y[1]
    points_y <- r_Mns[1]
    for (i in 2:length(y)) {
      if (r_Mns[i]-r_Mns[round(i-1)]!=0) {
        points_x <- c(points_x, (y[i]+y[round(i-1)])/2)
        points_y <- c(points_y, mean(c(r_Mns[i],r_Mns[round(i-1)])))
      }
    }
    points_x <- c(points_x, y[length(y)])
    points_y <- c(points_y, r_Mns[length(y)])
    
    if (type=="linear") {
      fnc_pre <- stats::approxfun(x=points_x, y=points_y, method="linear", rule=2)
    }
    
    if (type=="m-spline") {
      fnc_pre <- stats::splinefun(x=points_x, y=points_y, method="monoH.FC")
    }
    
    # Construct numerical derivative
    fnc <- function(y) {
      width <- 0.1 # Note: may need to play around with this value
      x1 <- y - width/2
      x2 <- y + width/2
      if (x1<0) { x2<-width; x1<-0; }
      if (x2>1) { x1<-1-width; x2<-1; }
      if (dir=="decr") {
        return(min((fnc_pre(x2)-fnc_pre(x1))/width,0))
      } else {
        return(max((fnc_pre(x2)-fnc_pre(x1))/width,0))
      }
    }
  }
  return(fnc)
}

# Helper function
as_df <- function(dat) { cbind(dat$w, y=dat$y) }

run_CIR <- function(dat, method){
  dat_obs <- dat %>% filter(s == 1)
  if (method == "ignore"){
    dat <- dat %>% filter(s == 1)
  }
  avi_dat_obs <- list(delta = dat_obs$delta,
                      y = dat_obs$y,
                      s = dat_obs$s,
                      w = dat_obs[,!names(dat_obs) %in% c("delta", "y", "s")])
  avi_dat <- list(delta = dat$delta,
                  y = dat$y,
                  s = dat$s,
                  w = dat[,!names(dat) %in% c("delta", "y", "s")])
  
  avi_dat$w <- data.frame(model.matrix(as.formula(paste("~", paste(names(avi_dat$w), collapse =  "+"))), avi_dat$w)[,-1])
  print(head(avi_dat$w))
  names(avi_dat$w) <- paste("w", 1:ncol(avi_dat$w), sep="")
  print(head(avi_dat$w))
  
  avi_dat_obs$w <- data.frame(model.matrix(as.formula(paste("~", paste(names(avi_dat_obs$w), collapse =  "+"))), avi_dat_obs$w)[,-1])
  names(avi_dat_obs$w) <- paste("w", 1:ncol(avi_dat_obs$w), sep="")
  
  # avi_dat_obs <- lapply(avi_dat, "[", which(avi_dat$s == 1))
  
  any_missing <- (sum(avi_dat$s) != length(avi_dat$s))
  
  # Run CIR procedure
  tune <- list(ntrees = c(250, 500, 1000), max_depth = c(1, 2), minobspernode = 10, shrinkage = 0.01)
  xgb_grid <- create.SL.xgboost(tune = tune)
  SL.library <- c("SL.mean", "SL.glm", "SL.earth", "SL.gam", "SL.ranger")#, xgb_grid$names)
  
  # estimate conditional density (only among observed)
  cond_density_fit <- construct_f_sIx_n(dat=avi_dat)
  f_sIx_n <- cond_density_fit$fnc
  Riemann_grid <- c(0, cond_density_fit$breaks)
  # estimate marginal density (marginalizing the conditional density over whole sample)
  f_s_n <- construct_f_s_n(dat=avi_dat, f_sIx_n=f_sIx_n)
  # estimate density ratio
  g_n <- construct_g_n(f_sIx_n=f_sIx_n, f_s_n=f_s_n)
  
  # estimate outcome regression (only among observed)
  mu_n <- construct_mu_n(dat=avi_dat, SL.library=SL.library, Riemann_grid = Riemann_grid)
  
  y_vals <- sort(unique(avi_dat$y))
  
  
  if (any_missing){ # to-do
    alpha_n <- construct_alpha_n(dat = avi_dat, SL.library=SL.library) # sampling weights
    F_n <- construct_Phi_n(dat = avi_dat, alpha_n = alpha_n, f_sIx_n = f_sIx_n, Riemann_grid = Riemann_grid) # debiased cdf estimate
    F_ns <- sapply(y_vals, FUN = F_n)
    F_n_inverse <- function(t){
      if (any(F_ns >= t)){
        val <- y_vals[min(which(F_ns >= t))]
      } else{
        val <- max(y_vals)
      }
     
    }
  } else{
    # if no missingness, can use the empirical cdf for F_0 and the sampling weights are just 1
    F_n <- ecdf(avi_dat$y)
    F_n_inverse <- function(t){
      quantile(avi_dat$y, probs = t, type = 1)
    }
    alpha_n <- function(w) return(1)
  }
  
  Gamma_n <- construct_Gamma_n(dat=avi_dat, mu_n=mu_n, g_n=g_n, alpha_n = alpha_n, f_sIx_n = f_sIx_n, Riemann_grid = Riemann_grid)
  kappa_n <- construct_kappa_n(dat = avi_dat, mu_n = mu_n, g_n = g_n, alpha_n = alpha_n)

  gcm_x_vals <- sapply(y_vals, F_n)
  inds_to_keep <- !base::duplicated(gcm_x_vals)
  gcm_x_vals <- gcm_x_vals[inds_to_keep]
  gcm_y_vals <- sapply(y_vals[inds_to_keep], Gamma_n)
  if (!any(gcm_x_vals==0)) {
    gcm_x_vals <- c(0, gcm_x_vals)
    gcm_y_vals <- c(0, gcm_y_vals)
  }
  gcm <- fdrtool::gcmlcm(x=gcm_x_vals, y=gcm_y_vals, type="gcm")
  theta_n <- stats::approxfun(
    x = gcm$x.knots[-length(gcm$x.knots)],
    y = gcm$slope.knots,
    method = "constant",
    rule = 2,
    f = 0
  )
  n_eval_pts <- 101
  theta_prime <- construct_deriv(r_Mn = theta_n, type = "m-spline",
                                 dir = "incr", y = seq(0, 1, length.out = n_eval_pts))
  
  # Compute estimates
  ests <- sapply(seq(0,1,length.out = n_eval_pts), theta_n)
  deriv_ests <- sapply(seq(0, 1, length.out = n_eval_pts), theta_prime)
  uniq_y <- sort(unique(avi_dat$y))
  kappa_ests <- sapply(uniq_y, kappa_n)
  # kappa_ests <- sapply(avi_dat$y, function(x){
  #   ind <- which(uniq_y == x)
  #   kappa_ests[ind]
  # })
  # transform kappa to quantile scale
  kappa_ests_rescaled <- sapply(seq(0, 1, length.out = n_eval_pts), function(x){
    # ind <- which(uniq_y == quantile(uniq_y, probs = x, type = 1))
    # ind <- which(uniq_y == quantile(avi_dat$y, probs = x, type = 1)) # this one seems to work!! 
    ind <- which(uniq_y == F_n_inverse(x)) # this one is more general b/c any form of F_n_inverse can be given
    kappa_ests[ind]
  })
  tau_ests <- deriv_ests * kappa_ests_rescaled
  q <- ChernoffDist::qChern(p = 0.975)
  half_intervals <- sapply(1:n_eval_pts, function(x){
    (4*tau_ests[x]/length(avi_dat$y))^{1/3}*q
  })
  cils <- ests - half_intervals
  cius <- ests + half_intervals
  
  # Plot estimates vs true values
  df_plot <- data.frame(
    # x = seq(min(avi_dat$s),max(avi_dat$s),length.out = n_eval_pts),
    # x = quantile(avi_dat$y, probs = seq(0, 1, length.out = n_eval_pts), type = 1), # this one works!
    x = sapply(seq(0, 1, length.out = n_eval_pts), F_n_inverse), # this form is more general b/c any form of F_n_inverse can be given
    x_quants = seq(0, 1, length.out = n_eval_pts),
    y = ests,
    y_low = cils,
    y_hi = cius
  )
  
  return(df_plot)
}
