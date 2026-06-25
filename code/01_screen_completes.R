#' -----------------------------------------------------------------------------
#' Date created: May 18, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Compares CR demographics with what was reported in our study
#' Examines trends in the data to determine which ones are acceptable
#' -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(tidyREDCap)
library(tigris)
library(zipcodeR)

box_health <- "/Users/smarte4/Library/CloudStorage/Box-Box/[Box Health - External] CCHS_Study/[Box Health - External] "

#' Test Participants

test_aids <- c(
  "69d7b912-fccf-219b-6f9d-eb058501c8ca",
  "69d7b8d4-e164-c817-af13-f1c5d989ae3a"
)

#' List of Illinois County names
il_counties <- counties(state = "IL")
county_names <- tolower(il_counties$NAME)

il_zips <- search_state("IL")

#' Participants who were already invited (update each subsequent survey)
pp_data <- read_csv(paste0(box_health, 
                           "cchs_pp_redcap/", 
                           "ILCCHSPrimePanels_DATA_2026-06-25_1413.csv")) %>%
  filter(cchs_questionnaire_complete == 2) %>%
  filter(!(aid %in% test_aids))

#' Flagged as complete
pp_data2 <- filter(pp_data, cchs_questionnaire_complete == 2)

#' ZIP codes in IL
pp_data3 <- filter(pp_data2, demo_1 %in% as.numeric(il_zips$zipcode))

#' Comparing reported demographics to those from CR
cr_demo <- read_xlsx(paste0(box_health, 
                            "cchs_pp_redcap/", 
                            "CR_Demos_Matched_06.02.26.xlsx")) %>%
  mutate(aid = RID)

table(cr_demo$Race)
table(cr_demo$Gender)
table(cr_demo$Ethnicity)
hist(as.numeric(cr_demo$Age))
summary(as.numeric(cr_demo$Age))

missing_demo_data <- filter(pp_data2, !(aid %in% cr_demo$aid))
write_csv(select(missing_demo_data, aid),
          here::here(paste0(box_health, 
                            "cchs_pp_redcap/",
                            "cchs_demographic_aids_06.25.26.csv")))

data_check <- filter(pp_data3, !(aid %in% missing_demo_data$aid)) %>%
  filter(aid != "[%RID%]") %>%
  mutate(screen_1b = as.numeric(screen_1b)) %>%
  mutate(redcap_age = year(today()) - demo_4,
         redcap_gender = ifelse(demo_5 == 1, "Female",
                                ifelse(demo_5 == 2, "Male", "Other")),
         redcap_white = ifelse(demo_6___4 == 1, 1, 0),
         redcap_black = ifelse(demo_6___3 == 1, 1, 0),
         redcap_hispanic = ifelse(demo_6___5 == 1, 1, 0),
         redcap_api = ifelse(demo_6___2 == 1, 1, 0),
         redcap_aian = ifelse(demo_6___1 == 1, 1, 0),
         redcap_other_re = ifelse(demo_6___6 == 1 | demo_6___7 == 1 | demo_6___8 == 1, 1, 0),
         redcap_only_hispanic = ifelse(redcap_hispanic == 1 & redcap_white == 0 &
                                         redcap_black == 0 & redcap_api == 0 & redcap_aian == 0 &
                                         redcap_other_re == 0, 1, 0),
         number_roles = screen_1a___1 + screen_1a___2 + screen_1a___3) 

#' Demo groups in data set
table(data_check$demo_6___1) #AI/AN
table(data_check$demo_6___2) #API
table(data_check$demo_6___3) #Black
table(data_check$demo_6___4) #White
table(data_check$demo_6___5) #Hispanic
table(data_check$demo_6___6) #MENA
table(data_check$demo_6___7) #Other
table(data_check$demo_6___8) #Prefer not to say

#' Gender groups in data set
table(data_check$demo_5)

#' Age in data set
summary(data_check$redcap_age)

#' Parents vs. Guardians vs. Foster Parents
table(data_check$screen_1a___1) #' Parents
table(data_check$screen_1a___2) #' Guardians
table(data_check$screen_1a___3) #' Foster Parents

summary(data_check$number_roles)

#' Age by relationship
parents <- filter(data_check, screen_1a___1 == 1)
summary(parents$redcap_age)
hist(parents$redcap_age)

guardians <- filter(data_check, screen_1a___2 == 1)
summary(guardians$redcap_age)
hist(guardians$redcap_age)

fosters <- filter(data_check, screen_1a___3 == 1)
summary(fosters$redcap_age)
hist(fosters$redcap_age)

data_check <- data_check %>%
  left_join(cr_demo, by = "aid") %>%
  mutate(cr_hispanic = ifelse(str_detect(Ethnicity, "Yes"), 1, 0),
         Age = as.numeric(Age)) %>%
  mutate(gender_mismatch = ifelse(redcap_gender != Gender, 1, 0),
         #no_dob = ifelse(is.na(demo_4), 1, 0),
         age_mismatch = ifelse(is.na(demo_4) | abs(redcap_age - Age) > 1, 1, 0),
         age_ineligible = ifelse(redcap_age < 18 | Age < 18, 1, 0),
         hispanic_mismatch = ifelse(cr_hispanic != redcap_hispanic, 1, 0),
         race_mismatch = ifelse((Race == "White" & redcap_white == 0) |
                                (Race == "Black, or African American" & redcap_black == 0) |
                                (str_detect(Race, "Asian|Pacific Islanter") & redcap_api == 0) |
                                (Race == "American Indian or Alaska Native" & redcap_aian == 0) |
                                (str_detect(Race, "Some other race") & redcap_other_re == 0) |
                                (str_detect(Race, "Prefer not to answer") & redcap_other_re == 0),
                                1, 0),
         age_difference = redcap_age - Age) 

table(data_check$race_mismatch)
table(data_check$hispanic_mismatch)
table(data_check$race_mismatch, data_check$hispanic_mismatch)

table(data_check$Race, data_check$redcap_aian)
table(data_check$Race, data_check$redcap_api)
table(data_check$Race, data_check$redcap_white)
table(data_check$Race, data_check$redcap_black)
table(data_check$Race, data_check$redcap_other_re)

table(data_check$cr_hispanic, data_check$redcap_hispanic)
table(data_check$redcap_only_hispanic, data_check$hispanic_mismatch)
table(data_check$redcap_only_hispanic, data_check$race_mismatch)

hispanic_only <- filter(data_check, redcap_only_hispanic == 1) %>%
  select(redcap_white:redcap_only_hispanic, Race, Ethnicity)
table(hispanic_only$Race)

#' Other data issues:
#' number of times moved is last year is greater than length lived in the house
data_check <- data_check %>%
  mutate(home_tenure_issue = ifelse(fh_7 > 1 & demo_7 > 1, 1, 0))
home_tenure_check <- select(data_check, aid, fh_7, demo_7, home_tenure_issue)

#' time lived in the home exceed age
data_check <- data_check %>%
  mutate(tenure_exceeds_age = ifelse(demo_7 > (redcap_age + 1), 1, 0))
tenure_age_check <- select(data_check, aid, redcap_age, demo_7, tenure_exceeds_age)

#' How many adults live in the home at least half time?
summary(data_check$demo_12)
num_adults_check <- select(data_check, aid, demo_12)
quantile(data_check$demo_12, probs = c(0.01, 0.05, 0.50, 0.95, 0.99, 1),
         na.rm = T)
data_check$num_adults_check = ifelse(data_check$demo_12 > 7, 1, 0)
num_adults_check <- select(data_check, aid, demo_12, num_adults_check)

#' How many children under 5? How many children total?
summary(data_check$screen_1b)
summary(data_check$screen_1c)
data_check <- data_check %>%
  mutate(num_kid_check = ifelse(screen_1b > screen_1c, 1, 0)) 
num_kids_check <- select(data_check, aid, screen_1b, screen_1c, num_kid_check)

#' email shows up more than once
data_check <- data_check %>%
  mutate(dup_email = ifelse((!is.na(contact_1a) & (duplicated(contact_1a) | duplicated(contact_1a, fromLast = T))) | 
                              (!is.na(contact_2a) & (duplicated (contact_2a) | duplicated(contact_2a, fromLast = T))), 1, 0))
table(data_check$dup_email)
dup_email_check <- select(data_check, aid, contact_1a, contact_2a, dup_email) %>%
  arrange(desc(dup_email), contact_1a, contact_2a)
dup_emails <- filter(data_check, dup_email == 1) %>%
  arrange(contact_1a, contact_2a) %>%
  select(aid, contact_2a, contact_1a, dup_email)

#' Time to complete (in minutes)
summary(data_check$survey_duration)
quantile(data_check$survey_duration, probs = c(0, 0.01, 0.05, 0.1, 0.90, 0.95, 0.99, 1))
duration_check <- select(data_check, survey_duration)

#'----------------------------------------
#'Screen people
#'----------------------------------------

#' First, drop anyone who has a gender or age mismatch or other data quality 
#' issues (tenure in home exceeds age, number of children doesn't add up)
ineligible_1 <- data_check %>%
  filter(gender_mismatch == 1 | age_mismatch == 1 | age_ineligible == 1 |
         home_tenure_issue == 1 | num_kid_check == 1 | #num_adults_check == 1 |
         dup_email == 1) 

#' Second, drop anyone who has a race mismatch (other than those who only checked
#' hispanic)
ineligible_2 <- data_check %>%
  filter(redcap_only_hispanic == 0) %>%
  filter(race_mismatch == 1)

#' Third, drop anyone who selected hispanic only in REDCap and did not report
#' hispanic in CR
ineligible_3 <- data_check %>%
  filter(redcap_only_hispanic == 1) %>%
  filter(hispanic_mismatch == 1)

ineligible <- bind_rows(ineligible_1, ineligible_2, ineligible_3) %>%
  select(aid, demo_4, redcap_age:dup_email)

table(ineligible$gender_mismatch)
table(ineligible$hispanic_mismatch)
table(ineligible$race_mismatch)
table(ineligible$age_mismatch)
table(ineligible$age_ineligible)

#' Of the remaining surveys, what does time to completion look like
eligible <- filter(data_check, !(aid %in% ineligible$aid))

#' Parents vs. Guardians vs. Foster Parents
table(eligible$screen_1a___1) #' Parents
table(eligible$screen_1a___2) #' Guardians
table(eligible$screen_1a___3) #' Foster Parents

summary(eligible$number_roles)

#' Age by relationship
parents2 <- filter(eligible, screen_1a___1 == 1)
summary(parents2$redcap_age)
hist(parents2$redcap_age)

guardians2 <- filter(eligible, screen_1a___2 == 1)
summary(guardians2$redcap_age)
hist(guardians2$redcap_age)

fosters2 <- filter(eligible, screen_1a___3 == 1)
summary(fosters2$redcap_age)
hist(fosters2$redcap_age)

#' Childcare providers?
table(eligible$screen_3)

table(eligible$screen_3a___1) #' Family-based
table(eligible$screen_3a___2) #' Center-based
table(eligible$screen_3a___3) #' Nanny
table(eligible$screen_3a___4) #' Babysitter
table(eligible$screen_3a___5) #' Other

summary(eligible$survey_duration)
hist(eligible$survey_duration[which(eligible$survey_duration < 40)],
     breaks = 40)
duration_check <- select(eligible, record_id, aid, survey_duration) 

#' Check for straight-lining (SS, PSS and SASSY)
eligible <- eligible %>%
  rowwise() %>%
  mutate(ss_sd = sd(c_across(ss_1:ss_12), na.rm = T),
         ps_sd = sd(c_across(ps_1:ps_18), na.rm = T),
         bn2_sd = sd(c_across(bn_2a:bn_2g), na.rm = T),
         ne2_sd = sd(c_across(ne_2a:ne_2g), na.rm = T),
         sassy_sd = sd(c_across(sassy_1:sassy_4), na.rm = T),
         pa_sd = sd(c_across(pa_1:pa_12), na.rm = T)) %>%
  ungroup()

#' Social stressors
summary(eligible$ss_sd)
quantile(eligible$ss_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$ss_sd, breaks = 20)
  
#' Parenting stress
summary(eligible$ps_sd)
quantile(eligible$ps_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$ps_sd, breaks = 20)
  
#' Basic needs
summary(eligible$bn2_sd)
quantile(eligible$bn2_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$bn2_sd, breaks = 20)

#' Neighborhood environment
summary(eligible$ne2_sd)
quantile(eligible$ne2_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$ne2_sd, breaks = 20)

#' SASSY
summary(eligible$sassy_sd)
quantile(eligible$sassy_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$sassy_sd, breaks = 20)

#' Physical activity (ACTS-MG)
summary(eligible$pa_sd)
quantile(eligible$pa_sd, probs = c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20),
         na.rm = T)
hist(eligible$pa_sd, breaks = 20)

#' Flag straight-liners and sum how many scales are straightlined
eligible <- eligible %>%
  mutate(ss_straight = ifelse(ss_sd == 0, 1, 0),
         ps_straight = ifelse(ps_sd == 0, 1, 0),
         bn2_straight = ifelse(bn2_sd == 0, 1, 0),
         ne2_straight = ifelse(ne2_sd == 0, 1, 0),
         sassy_straight = ifelse(sassy_sd == 0, 1, 0),
         pa_straight = ifelse(pa_sd == 0, 1, 0),
         num_straights = ss_straight + ps_straight + bn2_straight + ne2_straight +
           sassy_straight + pa_straight)
summary(eligible$num_straights)
hist(eligible$num_straights)

straight_check <- select(eligible, record_id, aid, ss_straight:num_straights, 
                         survey_duration) 
straightliners <- filter(straight_check, ps_straight == 1)

#' Basic demographics
table(eligible$redcap_gender, useNA = "ifany")
summary(eligible$redcap_age, useNA = "ifany")
table(eligible$redcap_white, useNA = "ifany")
table(eligible$redcap_black, useNA = "ifany")
table(eligible$redcap_hispanic, useNA = "ifany")
summary(eligible$screen_1c, useNA = "ifany")
summary(eligible$screen_1b, useNA = "ifany")
table(eligible$screen_3, useNA = "ifany")
table(eligible$cn_2, useNA = "ifany")
table(eligible$demo_3, useNA = "ifany")
table(eligible$contact_2, useNA = "ifany")
table(eligible$contact_2, eligible$demo_3, useNA = "ifany")

#' Eligible participants
file_name2 <- paste0(paste0(box_health, "cchs_pp_redcap/", 
                            "eligible_", today(), ".csv"))
write_csv(eligible, file_name2)

file_name3 <- paste0(paste0(box_health, "cchs_pp_redcap/", 
                            "ineligible_", today(), ".csv"))
write_csv(ineligible, file_name3)

file_name4 <- paste0(paste0(box_health, "cchs_pp_redcap/", 
                            "ineligible_aids_", today(), ".csv"))
write_csv(select(ineligible, aid), file_name4)

#' Deidentified data set
eligible_deid <- filter(pp_data, aid %in% eligible$aid) %>%
  select(-c(contact_1a, contact_2a, redcap_survey_identifier:cchs_screen_timestamp,
            cchs_screen_complete, cchs_questionnaire_timestamp, cchs_questionnaire_complete,
            survey_duration))

write_csv(eligible_deid, here::here("raw_data", "cchs_deidentified_raw.csv"))
