library(tidyverse)

# read in data on responders, and remove accidental invitees
dat <- read.csv("/home/cwolock/currstat_CIR_supplementary/data_analysis/long.covid_Sep30.csv")
to_remove <- read.csv("/home/cwolock/currstat_CIR_supplementary/data_analysis/records_remove_Nov13.csv")
dat <- dat %>% filter(!(record_id %in% to_remove$record_id))

# read in data on all invitees
all_invitees <- read.csv("/home/cwolock/currstat_CIR_supplementary/data_analysis/invitees.longCovidSurvey.csv")
nonresponders <- all_invitees %>% filter(!(record_id %in% dat$record_id))

# read in Ct data, and reconcile the two files
ct_dat1 <- read.csv("/home/cwolock/currstat_CIR_supplementary/data_analysis/y2.ct_Nov1.csv")
ct_dat2 <- read.csv("/home/cwolock/currstat_CIR_supplementary/data_analysis/y3.ct_Nov1.csv")

ct_dat1 <- ct_dat1 %>%
  mutate(exposure_date = collection_date) %>%
  select(record_id, exposure_date, sars_cov_2_orf1b_crt_1, sars_cov_2_orf1b_crt_2)
ct_dat2 <- ct_dat2 %>%
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

# add ct data to other data
nonresponders_ct_dat <- left_join(nonresponders, ct_dat, by = c("record_id", "exposure_date"))
ct_dat <- left_join(dat, ct_dat, by = c("record_id", "exposure_date"))

# viral load categories
ct_dat <- ct_dat %>% mutate(viral_load = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                                   is.numeric(crt) & crt <= 30 ~ "B",
                                                   is.na(crt) & exposure_cat == 1 ~ "C",
                                                   is.na(crt) & exposure_cat == 2 ~ NA),
                            crt_raw = crt,
                            crt = case_when(is.numeric(crt) & crt > 30 ~ "A",
                                            is.numeric(crt) & crt <= 30 ~ "B",
                                            is.na(crt) ~ NA))

# the invitee data has "test_type" instead of "exposure_cat" but they map 1-to-1
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

# filter to only people with 120 days followup
tau <- 120
ct_dat <- ct_dat %>% mutate(fu_time = as.numeric(difftime(as.Date("2023-06-23"), exposure_date)))
ct_dat <- ct_dat %>% filter(fu_time >= tau) %>%
  select(-fu_time)
nonresponders_ct_dat <- nonresponders_ct_dat %>% mutate(fu_time = as.numeric(difftime(as.Date("2023-06-23"), exposure_date)))
nonresponders_ct_dat <- nonresponders_ct_dat %>% filter(fu_time >= tau) %>%
  select(-fu_time)

# create baseline risk factors
ct_dat <- ct_dat %>% filter(!is.na(results_c19_primary_date)) %>%
  mutate(days_since_last_vax = ifelse(is.na(results_c19_booster_date),
                                      -difftime(results_c19_primary_date, exposure_date),
                                      -difftime(results_c19_booster_date, exposure_date)),
         days_since_last_vax_cat = case_when(days_since_last_vax < 90 ~ "A",
                                             days_since_last_vax >= 90 & days_since_last_vax < 180 ~ "B",
                                             days_since_last_vax >= 180 ~ "C")) %>%
  select(-c(starts_with("long_symptom"), starts_with("long_covid_care"), # get rid of unused variables
            starts_with("care_from"), starts_with("long_covid_which"),
            core_birthdate, missed_days, long_covid_household_sick, house_care, household_missed_days,
            short_answer, health_before, health_now, affiliation,
            month_followup_complete,
            collection_date
  )) %>%
  filter(core_sex != "dont_say" & results_c19_primary_status == 3) %>% # need people to have filled out sex, filter to only fully vaxxed
  mutate(resolution = as.numeric(long_covid_illness == "no"), # have symptoms resolved at time of survey response?
         comorbid_cardio = as.numeric((core_health_risk___cvd + core_health_risk___diabetes + core_health_risk___hbp) > 0),
         comorbid_resp = as.numeric((core_health_risk___asthma + core_health_risk___copd + core_health_risk___bronchitis) > 0),
         comorbid_immun = as.numeric((core_health_risk___cancer + core_health_risk___immsup) > 0),
         comorbid_allergy = core_health_risk___allergy,
         symp_tired = results_symptoms___tired,
         symp_systemic = as.numeric((results_symptoms___fever + results_symptoms___chills +
                                       results_symptoms___sweats + results_symptoms___ache + results_symptoms___headache) > 0),
         symp_resp = as.numeric((results_symptoms___cough +
                                   results_symptoms___breathe + results_symptoms___throat +
                                   results_symptoms___nose + results_symptoms___rash +
                                   results_symptoms___ear + results_symptoms___eye +
                                   results_symptoms___smell_taste) > 0),
         symp_gi = as.numeric((results_symptoms___nausea + results_symptoms___diarrhea) > 0),
         age_bin = case_when(birthday_years <= 25 ~ "A",
                             birthday_years > 25 & birthday_years <= 40 ~ "B",
                             birthday_years > 40 & birthday_years <= 60 ~ "C",
                             birthday_years > 60 ~ "D")) %>%
  # get rid of other unused variables
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
            month_followup_timestamp,
            first_pos_flag,
            sars_cov_2_status,
            days_since_last_vax,
            year,
            fever_chills))

nonresponders_ct_dat <- nonresponders_ct_dat %>% # the vax dates when missing are blank
  mutate(results_c19_primary_date = ifelse(results_c19_primary_date == "", NA, results_c19_primary_date),
         results_c19_booster_date = ifelse(results_c19_booster_date == "", NA, results_c19_booster_date)) %>%
  filter(!is.na(results_c19_primary_date)) %>%
  mutate(days_since_last_vax = ifelse(is.na(results_c19_booster_date),
                                      -difftime(results_c19_primary_date, exposure_date),
                                      -difftime(results_c19_booster_date, exposure_date)),
         days_since_last_vax_cat = case_when(days_since_last_vax < 90 ~ "A",
                                             days_since_last_vax >= 90 & days_since_last_vax < 180 ~ "B",
                                             days_since_last_vax >= 180 ~ "C")) %>%
  select(-c(affiliation)) %>%
  filter(core_sex != "dont_say" & core_sex != "other" & results_c19_primary_status == 3) %>% # need people to have filled out sex, filter to only fully vaxxed
  mutate(
    comorbid_cardio = as.numeric((core_health_risk___cvd + core_health_risk___diabetes + core_health_risk___hbp) > 0),
    comorbid_resp = as.numeric((core_health_risk___asthma + core_health_risk___copd + core_health_risk___bronchitis) > 0),
    comorbid_immun = as.numeric((core_health_risk___cancer + core_health_risk___immsup) > 0),
    comorbid_allergy = core_health_risk___allergy,
    symp_tired = results_symptoms___tired,
    symp_systemic = as.numeric((results_symptoms___fever + results_symptoms___chills +
                                  results_symptoms___sweats + results_symptoms___ache + results_symptoms___headache) > 0),
    symp_resp = as.numeric((results_symptoms___cough +
                              results_symptoms___breathe + results_symptoms___throat +
                              results_symptoms___nose + results_symptoms___rash +
                              results_symptoms___ear + results_symptoms___eye +
                              results_symptoms___smell_taste) > 0),
    symp_gi = as.numeric((results_symptoms___nausea + results_symptoms___diarrhea) > 0),
    age_bin = case_when(birthday_years <= 25 ~ "A",
                        birthday_years > 25 & birthday_years <= 40 ~ "B",
                        birthday_years > 40 & birthday_years <= 60 ~ "C",
                        birthday_years > 60 ~ "D")) %>%
  # get rid of other unused variables
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

# outcomes are NA for nonresponders
nonresponders_ct_dat <- nonresponders_ct_dat %>% mutate(resolution = NA, time_event = NA)

# combine datasets
ct_dat <- bind_rows(ct_dat, nonresponders_ct_dat) %>% filter(!is.na(viral_load))

ct_dat$core_sex <- factor(ct_dat$core_sex)
ct_dat$days_since_last_vax_cat <- factor(ct_dat$days_since_last_vax_cat)
ct_dat$age_bin <- factor(ct_dat$age_bin)

# for people with repsonse times greater than tau, consider nonresponders
ct_dat <- ct_dat %>%
  mutate(time_event = ifelse(time_event >= tau | is.na(time_event), tau, time_event),
         resolution = ifelse(time_event == tau, NA, resolution)) #%>%

ct_dat <- ct_dat %>% select(-c(crt, record_id, crt_raw, exposure_date))

fname <- paste0("/home/cwolock/currstat_CIR_supplementary/data_analysis/long_covid_truncated_",
                 tau, ".rds")
saveRDS(ct_dat, file = fname)
