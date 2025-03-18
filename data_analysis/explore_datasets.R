dat_oldest <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_021825_noinconclusives_untoucheddates.rds")
dat_older <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_021825_noinconclusives_fixedinvitedates.rds")
dat_new <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_021825_noinconclusives_fixedexpdates.rds")
dat_newest <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_022425_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")

dat_oldest <- dat_oldest %>% arrange(record_id)
dat_older <- dat_older %>% arrange(record_id)
dat_new <- dat_new %>% arrange(record_id)
dat_newest <- dat_newest %>% arrange(record_id)

identical(dat_oldest, dat_older) # futzing around with invite dates (unsurprisingly) did not affect the analysis dataset

invited_both_years <- dat_newest %>% filter(!(record_id %in% dat_new$record_id))
new_datapoints <- dat_new %>% filter(!(record_id %in% dat_older$record_id))
lost_datapoints <- dat_older %>% filter(!(record_id %in% dat_new$record_id))

# look at changes, among people in common
both <- inner_join(dat_new, dat_oldest, by = c("record_id", "core_sex","comorbid_cardio", "comorbid_resp", "comorbid_immun",
                                               "comorbid_allergy", "age_bin", "resolution")) %>%
  select(-c(core_sex, starts_with("comorbid")))
# obviously times, symptoms, days since last vax, VL will change

both <- both %>% mutate(y_diff = time_event.x - time_event.y,
                        created_na = ifelse((is.na(time_event.x ) & !is.na(time_event.y)) |
                                              (is.na(time_event.y) & !is.na(time_event.x)),
                                            TRUE, FALSE)) %>%
  arrange(y_diff)
