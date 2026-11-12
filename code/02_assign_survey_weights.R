#' -----------------------------------------------------------------------------
#' Date created: June 2, 2026
#' Last revised: November 12, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#'
#' Description: Assign survey weights to the responses using age groups, gender
#' (male and female), mutually exclusive race/ethnicity groups, and educational
#' attainment
#'
#' Revisions (Nov 12, 2026):
#'   - PUMS variables converted to numeric before use. get_pums() returns them
#'     as character, so AGEP < 18 and SCHL <= 15 were text comparisons:
#'       * Children aged 2-9 did not count as minors ("2" > "18" as text), so
#'         households whose only children were 2-9 dropped out of the parent
#'         universe and the age targets came out too old
#'       * SCHL codes 2-9 (no schooling past 6th grade) got a missing education
#'         code, so the "HS diploma/GED or less" target came out too small
#'   - Respondents checking both White and MENA are now coded White (ACS codes
#'     MENA as White); previously they were Other/Multiple
#'   - Convergence check added (weighted totals vs. PUMS targets)
#'   - Comparison with the previous weights added
#' -----------------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(survey)
library(srvyr)

#census_api_key("census_api_token", install = TRUE)

survey_year <- 2026
file_date <- "20261112"         #' date stamp for output files
old_file_date <- "20260629"     #' previous version, for comparison

#' --------------------------------------------------
#' First, format the variables used to assign weights
#' --------------------------------------------------

data1 <- read_csv(here::here("raw_data", "cchs_deidentified_raw.csv"))

#' The technical documentation reports 1,054 participants in the final data set
nrow(data1)

data1 <- data1 %>%
  mutate(

    # age (demo_4) and 10-year age groups
    # Note: age from birth year only, so can overstate age by up to 1 year
    age = survey_year - as.integer(demo_4),
    age_cat = cut(
      age,
      breaks = c(17, 24, 34, 44, 54, Inf),
      labels = c("18-24", "25-34", "35-44", "45-54", "55+")
    ),

    # gender (demo_5)
    # 1 Female, 2 Male, 3 non-binary/other, 4 other, 5 prefer not to answer.
    # 3/4/5 = NA (limitations in ACS gender variable).
    gender = case_when(
      demo_5 == 1 ~ "Female",
      demo_5 == 2 ~ "Male",
      TRUE        ~ NA_character_
    ),

    # edu (demo_14) collapsed from 6 survey groups to 4 weighting groups
    # 7 (prefer not to answer) = NA.
    edu = case_when(
      demo_14 == 1 ~ "HS diploma/GED or less", #"HS or less, no diploma",
      demo_14 == 2 ~ "HS diploma/GED or less",
      demo_14 == 3 ~ "Some college/Associate", #' Some college/no degree
      demo_14 == 4 ~ "Some college/Associate", #' Associate
      demo_14 == 5 ~ "Bachelor's",
      demo_14 == 6 ~ "Graduate/professional",
      TRUE         ~ NA_character_
    )
  )

# race_eth (demo_6 checkbox: select all that apply)
# Need to collapse this variable into mutually exclusive categories:
#   Hispanic (any race) / NHWhite / NHBlack / NHOther-Multiple
#   (AIAN and Asian/PI collapsed into Other/Multiple due to small cells)

data1 <- data1 %>%
  mutate(
    # number of distinct *race* groups checked (excludes Hispanic/Other/refused)
    # White and MENA count as one group because ACS codes MENA as White
    .n_race = demo_6___1 + demo_6___2 + demo_6___3 + pmax(demo_6___4, demo_6___6),

    race_eth = case_when(
      demo_6___5 == 1                          ~ "Hispanic",
      .n_race > 1                              ~ "Other/Multiple",
      .n_race == 1 & demo_6___1 == 1           ~ "Other/Multiple", #"AIAN",
      .n_race == 1 & demo_6___2 == 1           ~ "Other/Multiple", #"Asian/PI",
      .n_race == 1 & demo_6___3 == 1           ~ "Black",
      .n_race == 1 & (demo_6___4 == 1 |
                        demo_6___6 == 1)       ~ "White",   # White and/or MENA
      demo_6___7 == 1                          ~ "Other/Multiple",  # Other only
      demo_6___8 == 1                          ~ NA_character_,     # refused only
      TRUE                                     ~ NA_character_
    )
  ) %>%
  select(-.n_race)

table(data1$race_eth, useNA = "ifany")

#' How many respondents checked both White and MENA (recoded to White)?
sum(data1$demo_6___4 == 1 & data1$demo_6___6 == 1 & data1$demo_6___5 != 1, na.rm = T)

#' Factor variables for the weighting variables
data1 <- data1 %>%
  mutate(
    age_cat  = factor(age_cat,
                      levels = c("18-24","25-34","35-44","45-54","55+")),
    gender   = factor(gender, levels = c("Female","Male")),
    race_eth = factor(race_eth,
                      levels = c("Hispanic","White","Black",
                                 "Other/Multiple")),
    edu     = factor(edu,
                     levels = c("HS diploma/GED or less",
                                "Some college/Associate",
                                "Bachelor's","Graduate/professional"))
  )

# Identify number of observations where we have all of the variables we need
# Respondents missing any weighting variable are excluded from raking and
# receive a weight of 0
rake_vars <- c("age_cat","gender","race_eth","edu")

data1_complete <- data1 %>%
  filter(if_all(all_of(rake_vars), ~ !is.na(.)))

#' Number missing each weighting variable, and number excluded overall
data1 %>% summarize(across(all_of(rake_vars), ~ sum(is.na(.x))))
nrow(data1) - nrow(data1_complete)

#' --------------------------------------------------
#' Second, use ACS data to obtain the weights
#' Going to use PUMS because they are meant to be
#' representative of the population in IL
#' --------------------------------------------------

# Download PUMS data from API
# 2024 Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2024.pdf
il <- get_pums(
  variables = c("AGEP","SEX","RAC1P","HISP","SCHL","RELSHIPP"),
  state = "IL", survey = "acs5", year = 2024)

# get_pums() returns these as character; convert so comparisons are numeric
il <- il %>%
  mutate(across(c(AGEP, SEX, RAC1P, HISP, SCHL, RELSHIPP), as.numeric))
glimpse(il)

# Identify parents of a co-resident child <18
# RELSHIPP codes 20 = householder; 21 = opp-sex spouse;
# 22 = opp-sex unmarried partner; 23 = same-sex spouse; 24 = same-sex partner;
# 25 = bio child; 26 = adopted child; 27 = stepchild; 35 = foster child
# Note: guardian is not easy to identify using this data set, and parents who
# live in someone else's household (e.g., with their own parents) are not
# captured by this definition
child_codes  <- c(25, 26, 27, 35)
parent_codes <- c(20, 21, 22, 23, 24)

hh_with_minor <- il %>%
  filter(RELSHIPP %in% child_codes, AGEP < 18) %>%
  distinct(SERIALNO)

#' Check: ages of children used to identify households (should include 0-17)
il %>%
  filter(SERIALNO %in% hh_with_minor$SERIALNO, RELSHIPP %in% child_codes, AGEP < 18) %>%
  count(AGEP)

parents <- filter(il, SERIALNO %in% hh_with_minor$SERIALNO) %>%
  filter(RELSHIPP %in% parent_codes, AGEP >= 18)

# Variables to match the weighting variables in our data set
parents <- parents %>%
  mutate(
    age_cat = cut(AGEP, breaks = c(17,24,34,44,54,Inf),
                  labels = c("18-24","25-34","35-44","45-54","55+")),
    gender = case_when(SEX == 1 ~ "Male", SEX == 2 ~ "Female"),
    race_eth = case_when(
      HISP != 1                           ~ "Hispanic",   # any Hispanic origin
      RAC1P == 1                          ~ "White",
      RAC1P == 2                          ~ "Black",
      RAC1P %in% c(6,7)                   ~ "Other/Multiple", #"Asian/PI",
      RAC1P %in% c(3,4,5)                 ~ "Other/Multiple", #"AIAN",
      TRUE                                ~ "Other/Multiple"
    ),
    edu = case_when(
      SCHL <= 15 ~ "HS diploma/GED or less", #"HS or less, no diploma",   # through 12th grade no diploma
      SCHL %in% 16:17 ~ "HS diploma/GED or less", #"HS diploma/GED",
      SCHL %in% 18:19 ~ "Some college/Associate", #"Some college, no degree",
      SCHL == 20 ~ "Some college/Associate", #Associate
      SCHL == 21 ~ "Bachelor's",
      SCHL %in% 22:24 ~ "Graduate/professional"
    )
  )

#' Check: no parent should be missing a weighting variable
parents %>% summarize(across(c(age_cat, gender, race_eth, edu), ~ sum(is.na(.x))))

# Weighted population margins from PUMS
# Create the survey object and then use the weights to calculate the numbers
pums_svy <- parents %>% as_survey_design(weights = PWGTP)

mk <- function(var) {
  pums_svy %>% group_by({{var}}) %>% summarize(n = survey_total()) %>%
    filter(!is.na({{var}})) %>% select(1, Freq = n)
}

pop_age <- mk(age_cat) %>% rename(age_cat = 1)
pop_gender <- mk(gender) %>% rename(gender = 1)
pop_race <- mk(race_eth) %>% rename(race_eth = 1)
pop_edu <- mk(edu) %>% rename(edu = 1)

#' Population targets as percentages (for the technical documentation)
pop_targets <- bind_rows(
  pop_age %>% rename(level = age_cat) %>% mutate(variable = "age_cat"),
  pop_gender %>% rename(level = gender) %>% mutate(variable = "gender"),
  pop_race %>% rename(level = race_eth) %>% mutate(variable = "race_eth"),
  pop_edu %>% rename(level = edu) %>% mutate(variable = "edu")
) %>%
  mutate(level = as.character(level)) %>%
  group_by(variable) %>%
  mutate(target_pct = round(100 * Freq / sum(Freq), 1)) %>%
  ungroup()
pop_targets

#' --------------------------------------------------
#' Create survey object
#' No clusters (ids ~ 1)
#' Convenience sample
#' --------------------------------------------------

svy1 <- svydesign(ids = ~1, data = data1_complete, weights = ~1)

# Use the rake() function for iterative post-stratification
svy1_raked <- rake(svy1,
              sample.margins     = list(~age_cat, ~gender, ~race_eth, ~edu),
              population.margins = list(pop_age, pop_gender, pop_race, pop_edu),
              control = list(maxit = 50))

#' Convergence check: weighted totals should equal the PUMS targets
convergence_check <- map(rake_vars, function(v) {
  data1_complete %>%
    mutate(w = weights(svy1_raked)) %>%
    group_by(level = as.character(.data[[v]])) %>%
    summarize(weighted_total = sum(w)) %>%
    mutate(variable = v)
}) %>%
  bind_rows() %>%
  left_join(select(pop_targets, variable, level, Freq), by = c("variable", "level")) %>%
  mutate(pct_diff = round(100 * (weighted_total - Freq) / Freq, 3))
convergence_check   #' pct_diff should be ~0 for every row

data1_complete$raw_weight <- weights(svy1_raked)
summary(data1_complete$raw_weight) #' These are on a population scale
round(max(data1_complete$raw_weight) / min(data1_complete$raw_weight), 1)

#' Diagnostics
data1_complete %>%
  mutate(w = weights(svy1_raked)) %>%
  arrange(desc(w)) %>%
  select(w, age_cat, gender, race_eth, edu) %>%
  head(15)

count(data1_complete, edu)
count(data1_complete, age_cat)
count(data1_complete, age_cat, gender)
count(data1_complete, edu, race_eth)

#' Normalized weights
data1_complete <- data1_complete %>%
  mutate(normalized_weight = raw_weight / mean(raw_weight))
summary(data1_complete$normalized_weight)
round(max(data1_complete$normalized_weight) / min(data1_complete$normalized_weight), 1)
mean(data1_complete$normalized_weight > 5)

#' Calculate the effective sample size
ess <- function(w) sum(w)^2 / sum(w^2)

n     <- nrow(data1_complete)
n_eff <- ess(data1_complete$normalized_weight)

c(n = n, n_eff = round(n_eff, 1),
  efficiency = round(n_eff / n, 3),
  deff = round(n / n_eff, 3))

#' --------------------------------------------------
#' Final data set
#' --------------------------------------------------
#' IMPORTANT (privacy): before sharing, confirm the file contains no direct
#' identifiers. Per the study consent, only ZIP code OR county may be released
#' for linkage; remove free-text "specify" fields (demo_5a, demo_6a, demo_9a),
#' email, and any other identifiers. Adjust the drop list to your actual columns.

data1_weights <- select(data1_complete, record_id, raw_weight:normalized_weight)

data1_weighted <- left_join(data1, data1_weights, by = "record_id")
data1_weighted$normalized_weight[is.na(data1_weighted$normalized_weight)] <- 0

#' Row count should not change in the join
c(data1 = nrow(data1), weighted = nrow(data1_weighted),
  zero_weight = sum(data1_weighted$normalized_weight == 0))

write_csv(data1_weighted, here::here("data", paste0("il_cchs_data_", file_date, ".csv")))

#' --------------------------------------------------
#' Compare with the previous weights
#' --------------------------------------------------

old_file <- here::here("data", paste0("il_cchs_data_", old_file_date, ".csv"))

weight_compare <- data1_weighted %>%
  select(record_id, new_weight = normalized_weight, age_cat, race_eth, edu) %>%
  left_join(read_csv(old_file) %>% select(record_id, old_weight = normalized_weight),
            by = "record_id") %>%
  filter(new_weight > 0, old_weight > 0) %>%
  mutate(ratio = new_weight / old_weight)

#' Overall agreement
weight_compare %>%
  summarize(n = n(),
            correlation = round(cor(new_weight, old_weight), 3),
            median_ratio = round(median(ratio), 2),
            pct_changed_25 = round(100 * mean(abs(ratio - 1) > 0.25), 1)) %>%
  print()

#' Which groups changed most? (expect younger parents up, older parents down)
weight_compare %>%
  group_by(age_cat) %>%
  summarize(mean_old = round(mean(old_weight), 2),
            mean_new = round(mean(new_weight), 2)) %>%
  print()

weight_compare %>%
  group_by(edu) %>%
  summarize(mean_old = round(mean(old_weight), 2),
            mean_new = round(mean(new_weight), 2)) %>%
  print()

#' --------------------------------------------------
#' Summary statistics
#' --------------------------------------------------

cchs <- data1_weighted %>%
  as_survey_design(ids = 1, weights = normalized_weight)

#' Variables to summarize and their display labels
wt_vars <- c(
  age_cat  = "Age group",
  gender   = "Gender",
  race_eth = "Race/ethnicity",
  edu      = "Educational attainment"
)

summarize_one <- function(varname, label) {

  # --- Unweighted counts and percentages (NA shown as its own row) ---
  unwtd <- data1_weighted %>%
    count(level = .data[[varname]], name = "n") %>%
    mutate(
      level = as.character(level),
      level = if_else(is.na(level), "(Missing)", level),
      pct_unwtd = 100 * n / sum(n)
    )

  # --- Weighted percentages among non-missing (survey-based, with SE) ---
  wtd <- cchs %>%
    filter(!is.na(.data[[varname]])) %>%
    group_by(level = .data[[varname]]) %>%
    summarize(p = survey_mean(vartype = "se")) %>%
    mutate(
      level     = as.character(level),
      pct_wtd   = 100 * p,
      pct_wtd_se = 100 * p_se
    ) %>%
    select(level, pct_wtd, pct_wtd_se)

  unwtd %>%
    left_join(wtd, by = "level") %>%
    mutate(variable = label, .before = 1)
}

demo_table <- imap_dfr(wt_vars, ~ summarize_one(.y, .x))

#' Round for presentation
demo_table_print <- demo_table %>%
  mutate(across(c(pct_unwtd, pct_wtd, pct_wtd_se), ~ round(.x, 1)))
demo_table_print

write_csv(demo_table_print, here::here("results", paste0("cchs_demographics_", file_date, ".csv")))
write_csv(pop_targets, here::here("results", paste0("cchs_pums_targets_", file_date, ".csv")))
