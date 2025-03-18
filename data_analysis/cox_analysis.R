set.seed(70124)
library(icenReg)
library(tidyverse)

B <- 1000

risk_factors_VL <- c("age_bin", # age
                     "core_sex", # sex
                     "comorbid_cardio", # cardiometabolic baseline risk
                     "comorbid_resp", # respiratory baseline risk
                     "comorbid_immun", # immunological baseline risk
                     "comorbid_allergy", # seasonal allergies
                     "symp_resp", # upper resp symptoms at baseline
                     "symp_gi", # gi symptoms at baseline
                     "symp_tired", # tired at baseline
                     "symp_systemic",# trouble breathing at baselines
                     "days_since_last_vax_cat",
                     "viral_load")
y <- "time_event"
delta <- "resolution"

dat <- readRDS("/home/cwolock/long_covid_data.rds")
dat <- dat %>% select(all_of(c(risk_factors_VL, y, delta))) %>%
  filter(!is.na(resolution))
fit <- icenReg::ic_sp(icenReg::cs2ic(time = time_event,
                                        eventOccurred = as.logical(resolution)) ~
                           age_bin + core_sex +
                           symp_resp + symp_gi + symp_systemic + symp_tired +
                           comorbid_cardio + comorbid_resp + comorbid_immun +
                           comorbid_allergy +
                           days_since_last_vax_cat + viral_load,
                         model = "ph",
                         bs_samples = B,
                         data = dat)


point_est = round(exp(fit$coefficients), digits = 3)
cil = round(exp(fit$coefficients - 1.96*sqrt(diag(fit$var))), digits = 3)
ciu = round(exp(fit$coefficients + 1.96*sqrt(diag(fit$var))), digits = 3)
pvals = round(2*pnorm(-abs(fit$coefficients/sqrt(diag(fit$var)))), digits = 3)
reject = ifelse(2*pnorm(-abs(fit$coefficients/sqrt(diag(fit$var)))) < 0.05, 1, 0)

p <- length(point_est)

# AGE
age_contrasts <- t(cbind(matrix(c(1,rep(0, p-1))),
                         matrix(c(0, 1, rep(0, p-2))),
                         matrix(c(0, 0, 1, rep(0, p-3)))))
age_hypotheses <- matrix(c(0,0,0))
covmtx <- fit$var
covmtx <- age_contrasts %*% covmtx %*% t(age_contrasts)
Fstat <- t(age_contrasts %*% fit$coefficients - age_hypotheses) %*%
  solve(covmtx) %*% (age_contrasts %*% fit$coefficients - age_hypotheses) / 3
age_pval <- stats::pchisq(3*Fstat,3, lower.tail=FALSE)

# Viral load
vl_contrasts <- t(cbind(matrix(c(rep(0, p-2), 1, 0)),
                        matrix(c(rep(0, p-1), 1))))
vl_hypotheses <- matrix(c(0,0))
covmtx <- fit$var
covmtx <- vl_contrasts %*% covmtx %*% t(vl_contrasts)
Fstat <- t(vl_contrasts %*% fit$coefficients - vl_hypotheses) %*%
  solve(covmtx) %*% (vl_contrasts %*% fit$coefficients - vl_hypotheses) / 2
vl_pval <- stats::pchisq(2*Fstat,2,lower.tail=FALSE)

# Time since vax
vax_contrasts <- t(cbind(matrix(c(rep(0, p-4), 1, 0, 0, 0)),
                         matrix(c(rep(0, p-3), 1, 0, 0))))
vax_hypotheses <- matrix(c(0,0))
covmtx <- fit$var
covmtx <- vax_contrasts %*% covmtx %*% t(vax_contrasts)
Fstat <- t(vax_contrasts %*% fit$coefficients - vax_hypotheses) %*%
  solve(covmtx) %*% (vax_contrasts %*% fit$coefficients - vax_hypotheses) / 2
vax_pval <- stats::pchisq(2*Fstat,2,lower.tail=FALSE)
