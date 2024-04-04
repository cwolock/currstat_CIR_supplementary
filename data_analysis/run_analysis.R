set.seed(72724)
source("/home/cwolock/chu_lab/susan/code/CIR_testing/utils.R")
# source("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/utils.R")
# Causal Isotonic Regression code

# library(SuperLearner, lib = "/home/cwolock/R_lib")
# library(dplyr, lib = "/home/cwolock/R_lib")
# library(haldensify, lib = "/home/cwolock/R_lib")
library(SuperLearner)
library(dplyr)
library(haldensify)

# dat <- readRDS("/home/cwolock/chu_lab/susan/data/long_covid_vaxxed_allyears_withCT_recID_inclnonresponders.rds")
dat <- readRDS("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_allyears_withCT_recID_inclnonresponders.rds")

dat <- dat %>% filter(!is.na(viral_load)) %>%
  select(-c(crt, record_id))

for (i in 1:ncol(dat)){
  if (sum(is.na(dat[,i])) > 0){
    print(i)
    print(names(dat)[i])
    print(sum(is.na(dat[,i])))
  }
}

names(dat)[names(dat) == "time_event"] <- "y"
names(dat)[names(dat) == "resolution"] <- "delta"
dat <- dat %>% mutate(s = ifelse(is.na(y), 0, 1),
                      y = ifelse(is.na(y), 0, y),
                      delta = ifelse(is.na(delta), 0, delta))

res_correct <- run_CIR(dat, "handle")
res_cc <- run_CIR(dat, "ignore")
res_nomiss <- run_CIR(dat[dat$s == 1,], "none")
saveRDS(res_correct, "/home/cwolock/chu_lab/susan/data/CIR_results_correct_032324.rds")
saveRDS(res_cc, "/home/cwolock/chu_lab/susan/data/CIR_results_cc_032324.rds")
saveRDS(res_nomiss, "/home/cwolock/chu_lab/susan/data/CIR_results_nomiss_032324.rds")
