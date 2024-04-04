library(tidyverse)

### full timespan (all years), fully vaxxed only (with time to last vaxx),
# sex != "Not answer"

# dat <- read.csv("C:/Users/cwolo/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long.covid_Sep30.csv")
# to_remove <- read.csv("C:/Users/cwolo/Dropbox/UW/DISSERTATION/chu_lab/susan/data/records_remove_Nov13.csv")
# dat <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long.covid_Sep30.csv")
# to_remove <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/records_remove_Nov13.csv")
# dat <- dat %>% filter(!(record_id %in% to_remove$record_id))
#
# sum(dat$results_c19_primary_status != 3)
# sum(is.na(dat$results_c19_primary_date) & dat$results_c19_primary_status == 3)
# table(dat$core_sex)
#
# dat <- dat %>% filter(!is.na(results_c19_primary_date)) %>% # something like 112 people are vaxxed but we don't know when
#   mutate(days_since_last_vax = ifelse(is.na(results_c19_booster_date),
#                                       -difftime(results_c19_primary_date, exposure_date),
#                                       -difftime(results_c19_booster_date, exposure_date)),
#          days_since_last_vax_cat = case_when(days_since_last_vax < 90 ~ "A",
#                                              days_since_last_vax >= 90 & days_since_last_vax < 180 ~ "B",
#                                              days_since_last_vax >= 180 ~ "C")) %>%
#   select(-c(starts_with("long_symptom"), starts_with("long_covid_care"), # get rid of post-baseline variables
#                          starts_with("care_from"), starts_with("long_covid_which"),
#                          core_birthdate, missed_days, long_covid_household_sick, house_care, household_missed_days, # get rid of unnecessary variables
#                          short_answer, health_before, health_now, affiliation,
#                          month_followup_complete, # this is the same for everyone (maybe it's whether they completed survey?)
#                          collection_date # NA for many people - seems to be day of (or just after) exposure_dat (day of positive test)
#                          )) %>%
#   filter(core_sex != "dont_say" & results_c19_primary_status == 3) %>% # need people to have filled out sex, filter to only fully vaxxed
#   mutate(resolution = as.numeric(long_covid_illness == "no"), # have symptoms resolved at time of survey response?
#          comorbid_cardio = as.numeric((core_health_risk___cvd + core_health_risk___diabetes + core_health_risk___hbp) > 0),
#          comorbid_resp = as.numeric((core_health_risk___asthma + core_health_risk___copd + core_health_risk___bronchitis) > 0),
#          comorbid_immun = as.numeric((core_health_risk___cancer + core_health_risk___allergy + core_health_risk___immsup) > 0),
#          symp_tired = results_symptoms___tired,
#          symp_breathe = results_symptoms___breathe,
#          age_bin = case_when(birthday_years <= 25 ~ "A",
#                              birthday_years > 25 & birthday_years <= 40 ~ "B",
#                              birthday_years > 40 & birthday_years <= 60 ~ "C",
#                              birthday_years > 60 ~ "D")) %>%
#   # get rid of other useless variables or variables I've already used to create composites
#   select(-c(starts_with("core_health_risk"), starts_with("results_symptoms"), X,
#             birthday_years,
#             long_covid_illness, results_c19_primary_status,
#             starts_with("core_race"),
#             starts_with("daily"),
#             starts_with("covid_home"),
#             starts_with("results"),
#             starts_with("y2"),
#             starts_with("y3"),
#             # record_id,
#             month_followup_timestamp,
#             first_pos_flag,
#             starts_with("exposure"),
#             sars_cov_2_status,
#             days_since_last_vax,
#             year,
#             fever_chills)) # do we want to keep race in?
#
# dat$core_sex <- factor(dat$core_sex)
# dat$days_since_last_vax_cat <- factor(dat$days_since_last_vax_cat)
# dat$age_bin <- factor(dat$age_bin)

# saveRDS(dat, file = "C:/Users/cwolo/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_allyears.rds")

### potentially important variables
# month_followup_timestamp: date of response
# exposure_date: date of positive test
# do we have date survey was sent?
# year: when survey response was recorded
# exposure cat: 1 = antigen, 2 = pcr. we have 823 pcr tests after all filtering

### full timespan (all years), fully vaxxed only (with time to last vaxx),
# sex != "Not answer"
# with CT data (should be PCR only, although some of these people have exposure_cat = 1, meaning RAT)
# split out RAT (no CT), low CT, high CT

#### everything above this is without the CT data, so not really necessary anymore

dat <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long.covid_Sep30.csv")
to_remove <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/records_remove_Nov13.csv")
dat <- dat %>% filter(!(record_id %in% to_remove$record_id))

all_invitees <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/invitees.longCovidSurvey.csv")
nonresponders <- all_invitees %>% filter(!(record_id %in% dat$record_id))

ct_dat1 <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/y2.ct_Nov1.csv")
ct_dat2 <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/y3.ct_Nov1.csv")
# ct_dat1 <- ct_dat1 %>% select(record_id, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2, collection_date) %>%
#   mutate(ct = (as.numeric(sars_cov_2_orf1b_crt_1) + as.numeric(sars_cov_2_orf1b_crt_2))/2,
#          exposure_date = collection_date) %>%
#   select(record_id, ct, exposure_date)
# ct_dat2 <- ct_dat2 %>% select(record_id, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2, results_collection_date) %>%
#   mutate(ct = (as.numeric(sars_cov_2_orf1b_crt_1) + as.numeric(sars_cov_2_orf1b_crt_2))/2,
#          exposure_date = results_collection_date) %>%
#   select(record_id, ct, exposure_date)

ct_dat1 <- ct_dat1 %>% #elect(record_id, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2, collection_date) %>%
  mutate(exposure_date = collection_date) %>%
  select(record_id, exposure_date, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2)
ct_dat2 <- ct_dat2 %>% #select(record_id, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2, results_collection_date) %>%
  mutate(exposure_date = results_collection_date) %>%
  select(record_id, exposure_date, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2)

ct_dat <- rbind(ct_dat1, ct_dat2)
ct_dat <- ct_dat %>% mutate(crt = case_when(sars_cov_2_orf1b_crt_1 == "Undetermined" &
                                              sars_cov_2_orf1b_crt_2 == "Undetermined" ~ NA,
                                            sars_cov_2_orf1b_crt_1 == "Undetermined" &
                                              sars_cov_2_orf1b_crt_2 != "Undetermined" ~ as.numeric(sars_cov_2_orf1b_crt_2),
                                            sars_cov_2_orf1b_crt_2 == "Undetermined" &
                                              sars_cov_2_orf1b_crt_1 != "Undetermined" ~ as.numeric(sars_cov_2_orf1b_crt_1),
                                            sars_cov_2_orf1b_crt_1 != "Undetermined" & sars_cov_2_orf1b_crt_1 != "" &
                                              sars_cov_2_orf1b_crt_2 != "Undetermined" & sars_cov_2_orf1b_crt_2 != "" ~
                                              (as.numeric(sars_cov_2_orf1b_crt_1) + as.numeric(sars_cov_2_orf1b_crt_2))/2,
                                            sars_cov_2_orf1b_crt_1 == "" & sars_cov_2_orf1b_crt_2 == "" ~ NA))

ct_dat <- ct_dat %>% group_by(record_id, exposure_date) %>%
  summarize(crt = mean(crt)) %>%
  ungroup()


# nonresponders_ct_dat <- left_join(nonresponders, ct_dat, by = c("record_id"))
# nonresponders_RAT_ct_dat <- left_join(nonresponder_RAT, ct_dat, by = c("record_id"))
## TO DO - get exposure date for invitees from Susan (although it looks like there aren't multiple records per
## among the invitees, so maybe not a problem)
nonresponders_ct_dat <- left_join(nonresponders, ct_dat, by = c("record_id", "exposure_date"))
ct_dat <- left_join(dat, ct_dat, by = c("record_id", "exposure_date"))

ct_dat <- ct_dat %>% mutate(viral_load = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                                   is.numeric(crt) & crt <= 30 ~ "B",
                                                   is.na(crt) & exposure_cat == 1 ~ "C",
                                                   is.na(crt) & exposure_cat == 2 ~ NA),
                            crt_raw = crt,
                            crt = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                            is.numeric(crt) & crt <= 30 ~ "B",
                                            is.na(crt) ~ NA))

# the invitee list has "test_type" instead of "exposure_cat" but they seem to map 1-to-1
nonresponders_ct_dat <- nonresponders_ct_dat %>%
  mutate(exposure_cat = ifelse(test_type == "antigen", 1, 2)) %>%
  mutate(viral_load = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                                   is.numeric(crt) & crt <= 30 ~ "B",
                                                   is.na(crt) & exposure_cat == 1 ~ "C",
                                                   is.na(crt) & exposure_cat == 2 ~ NA),
                            crt_raw = crt,
                            crt = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                            is.numeric(crt) & crt <= 30 ~ "B",
                                            is.na(crt) ~ NA))

# ct_dat <- inner_join(dat, ct_dat, by = c("record_id", "exposure_date")) %>%
# filter(!is.na(ct))

ct_dat <- ct_dat %>% filter(!is.na(results_c19_primary_date)) %>% # something like 112 people are vaxxed but we don't know when
  mutate(days_since_last_vax = ifelse(is.na(results_c19_booster_date),
                                      -difftime(results_c19_primary_date, exposure_date),
                                      -difftime(results_c19_booster_date, exposure_date)),
         days_since_last_vax_cat = case_when(days_since_last_vax < 90 ~ "A",
                                             days_since_last_vax >= 90 & days_since_last_vax < 180 ~ "B",
                                             days_since_last_vax >= 180 ~ "C")) %>%
  select(-c(starts_with("long_symptom"), starts_with("long_covid_care"), # get rid of post-baseline variables
            starts_with("care_from"), starts_with("long_covid_which"),
            core_birthdate, missed_days, long_covid_household_sick, house_care, household_missed_days, # get rid of unnecessary variables
            short_answer, health_before, health_now, affiliation,
            month_followup_complete, # this is the same for everyone (maybe it's whether they completed survey?)
            collection_date # NA for many people - seems to be day of (or just after) exposure_dat (day of positive test)
  )) %>%
  # 178 unvaxxed or unknown vax
  # 8 don't say about sex
  # 112 vaxxed but we don't know when
  filter(core_sex != "dont_say" & results_c19_primary_status == 3) %>% # need people to have filled out sex, filter to only fully vaxxed
  mutate(resolution = as.numeric(long_covid_illness == "no"), # have symptoms resolved at time of survey response?
         comorbid_cardio = as.numeric((core_health_risk___cvd + core_health_risk___diabetes + core_health_risk___hbp) > 0),
         comorbid_resp = as.numeric((core_health_risk___asthma + core_health_risk___copd + core_health_risk___bronchitis) > 0),
         comorbid_immun = as.numeric((core_health_risk___cancer + core_health_risk___immsup) > 0),
         comorbid_allergy = core_health_risk___allergy,
         symp_tired = results_symptoms___tired,
         symp_breathe = results_symptoms___breathe,
         age_bin = case_when(birthday_years <= 25 ~ "A",
                             birthday_years > 25 & birthday_years <= 40 ~ "B",
                             birthday_years > 40 & birthday_years <= 60 ~ "C",
                             birthday_years > 60 ~ "D")) %>%
  # get rid of other useless variables or variables I've already used to create composites
  select(-c(starts_with("core_health_risk"), starts_with("results_symptoms"), X,
            birthday_years,
            long_covid_illness, results_c19_primary_status,
            starts_with("core_race"),
            starts_with("daily"),
            starts_with("covid_home"),
            starts_with("results"),
            starts_with("y2"),
            starts_with("y3"),
            exposure_cat,
            # record_id,
            month_followup_timestamp,
            first_pos_flag,
            sars_cov_2_status,
            days_since_last_vax,
            year,
            fever_chills)) # do we want to keep race in?
# select(-c(days_since_last_vax, starts_with("results_c19"), starts_with("covid_home")))

nonresponders_ct_dat <- nonresponders_ct_dat %>% # for some reason the vax dates when missing are blank rather
  # than NA, so change blanks to NAs just to match
  mutate(results_c19_primary_date = ifelse(results_c19_primary_date == "", NA, results_c19_primary_date),
         results_c19_booster_date = ifelse(results_c19_booster_date == "", NA, results_c19_booster_date)) %>%
  filter(!is.na(results_c19_primary_date)) %>% # something like 112 people are vaxxed but we don't know when
  mutate(days_since_last_vax = ifelse(is.na(results_c19_booster_date),
                                      -difftime(results_c19_primary_date, exposure_date),
                                      -difftime(results_c19_booster_date, exposure_date)),
         days_since_last_vax_cat = case_when(days_since_last_vax < 90 ~ "A",
                                             days_since_last_vax >= 90 & days_since_last_vax < 180 ~ "B",
                                             days_since_last_vax >= 180 ~ "C")) %>%
  select(-c(affiliation)) %>%
  # 20 missing sex
  # 890 either unknown vax or not vaxxed
  # 143 people aer actually vaxxed, but we don't know when
  filter(core_sex != "dont_say" & core_sex != "other" & results_c19_primary_status == 3) %>% # need people to have filled out sex, filter to only fully vaxxed
  mutate(
         comorbid_cardio = as.numeric((core_health_risk___cvd + core_health_risk___diabetes + core_health_risk___hbp) > 0),
         comorbid_resp = as.numeric((core_health_risk___asthma + core_health_risk___copd + core_health_risk___bronchitis) > 0),
         comorbid_immun = as.numeric((core_health_risk___cancer + core_health_risk___immsup) > 0),
         comorbid_allergy = core_health_risk___allergy,
         symp_tired = results_symptoms___tired,
         symp_breathe = results_symptoms___breathe,
         # for some reason, these next two are already created in susan's other dataset, but not
         # in the list of invitees. easy enough to create myself
         symp_gi = as.numeric((results_symptoms___diarrhea + results_symptoms___nausea) > 0),
         symp_resp = as.numeric((results_symptoms___fever +
                                   results_symptoms___headache +
                                   results_symptoms___cough +
                                   results_symptoms___chills +
                                   results_symptoms___sweats +
                                   results_symptoms___throat +
                                   results_symptoms___nose +
                                   results_symptoms___ache +
                                   results_symptoms___rash +
                                   results_symptoms___ear +
                                   results_symptoms___eye +
                                   results_symptoms___smell_taste) > 0),
         age_bin = case_when(birthday_years <= 25 ~ "A",
                             birthday_years > 25 & birthday_years <= 40 ~ "B",
                             birthday_years > 40 & birthday_years <= 60 ~ "C",
                             birthday_years > 60 ~ "D")) %>%
  # get rid of other useless variables or variables I've already used to create composites
  select(-c(starts_with("core_health_risk"), starts_with("results_symptoms"), X,
            birthday_years,
            results_c19_primary_status,
            starts_with("core_race"),
            starts_with("daily"),
            starts_with("covid_home"),
            starts_with("results"),
            starts_with("y2"),
            starts_with("y3"),
            exposure_cat,
            # record_id,
            sars_cov_2_status,
            days_since_last_vax,
            invite_date,
            invite_yr,
            rapid_test_result,
            test_type,
            starts_with("rnase"),
            strain_name,
            starts_with("sars"),
            nwgc_id,
            gisaid_accession,
            genbank_accession,
            pango_lineage,
            pango_lineage_time,
            core_birthdate,
            enrollment_questionnaire_timestamp))

nonresponders_ct_dat <- nonresponders_ct_dat %>% mutate(resolution = NA, time_event = NA)

# tons of missingness in the results_symptom_onset_date and exposure_date for the nonresponders...why?
nonresponders_ct_dat <- nonresponders_ct_dat %>% mutate(exposure_date = ifelse(exposure_date == "", NA, exposure_date))
nonresponders_ct_dat <- nonresponders_ct_dat %>% filter(!is.na(exposure_date))

ct_dat <- bind_rows(ct_dat, nonresponders_ct_dat)

ct_dat$core_sex <- factor(ct_dat$core_sex)
ct_dat$days_since_last_vax_cat <- factor(ct_dat$days_since_last_vax_cat)
ct_dat$age_bin <- factor(ct_dat$age_bin)

sum(is.na(ct_dat$viral_load)) # these are people who tested via PCR but we don't have CRT for

ct_dat_no_exp_date <- ct_dat %>% select(-exposure_date)

# saveRDS(ct_dat_no_exp_date, file = "C:/Users/cwolo/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_allyears_withCTraw.rds")

ct_dat_no_exp_date <- ct_dat_no_exp_date %>% select(-crt_raw)

# get rid of NA viral loads
# ct_dat_no_exp_date <- ct_dat_no_exp_date %>% filter(!is.na(viral_load)) %>%
  # select(-crt)

# saveRDS(ct_dat_no_exp_date, file = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_allyears_withCT_recID.rds")
saveRDS(ct_dat_no_exp_date, file = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_allyears_withCT_recID_inclnonresponders.rds")

### everything below here is stratified by calendar time
ct_dat_no_exp_date <- ct_dat_no_exp_date %>% filter(!is.na(viral_load))
full_dat <- read.csv("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long.covid_Sep30.csv")
full_dat <- full_dat %>% filter(record_id %in% ct_dat_no_exp_date$record_id)

ct_dat <- ct_dat %>% select(-crt_raw)
ct_dat_early <- ct_dat %>% filter(exposure_date <= "2022-06-10") %>%
  select(-exposure_date)
ct_dat_late <- ct_dat %>% filter(exposure_date > "2022-06-10") %>%
  select(-exposure_date)

saveRDS(ct_dat_early, file = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_bef0622_withCT_recID.rds")
saveRDS(ct_dat_late, file = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/data/long_covid_vaxxed_aft0622_withCT_recID.rds")

### number 982 says "I did not contract covid, I read the question wrong one time and accidentally said yes to having covid, I apologize for the miscommunication!"
