#' -----------------------------------------------------------------------------
#' Date created: June 2, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Assign survey weights to the responses using age groups, gender 
#' (male and female), mutually exclusive race/ethnicity groups, and educational
#' attainment
#' -----------------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(survey)
library(srvyr)

#census_api_key("census_api_token", install = TRUE)

#' --------------------------------------------------
#' First, format the variables used to assign weights
#' --------------------------------------------------

survey_year <- 2026

data1 <- read_csv(here::here("raw_data", "cchs_deidentified_raw.csv"))

data1 <- data1 %>%
  mutate(
    
    # age (demo_4) and 10-year age groups
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
    
    # edu (demo_14) following the same 6 groups that ACS uses
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
#   Hispanic (any race) / NHWhite / NHBlack / NHAsian-PI / NHAIAN / 
#   and NHOther-Multiple

race_cols <- c("demo_6___1","demo_6___2","demo_6___3","demo_6___4",
               "demo_6___6")  # countable race boxes (MENA inc in White)

data1 <- data1 %>%
  mutate(
    # number of distinct *race* boxes checked (excludes Hispanic/Other/refused)
    .n_race = demo_6___1 + demo_6___2 + demo_6___3 + demo_6___4 + demo_6___6,
    
    race_eth = case_when(
      demo_6___5 == 1                          ~ "Hispanic",
      .n_race > 1                              ~ "Other/Multiple",
      .n_race == 1 & demo_6___1 == 1           ~ "Other/Multiple", #"AIAN",
      .n_race == 1 & demo_6___2 == 1           ~ "Other/Multiple", #"Asian/PI",
      .n_race == 1 & demo_6___3 == 1           ~ "Black",
      .n_race == 1 & (demo_6___4 == 1 |
                        demo_6___6 == 1)       ~ "White",   # White or MENA
      demo_6___7 == 1                          ~ "Other/Multiple",  # Other only
      demo_6___8 == 1                          ~ NA_character_,     # refused only
      TRUE                                     ~ NA_character_
    )
  ) %>%
  select(-.n_race)

table(data1$race_eth)

#' Factor variables for the weighting variables (age group, gender, race/eth)
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
rake_vars <- c("age_cat","gender","race_eth","edu")

data1_complete <- data1 %>%
  filter(if_all(all_of(rake_vars), ~ !is.na(.)))

#' Missing at least one variable for 11 participants (only 1%)
print(sapply(data1[rake_vars], function(x) sum(is.na(x))))

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

# Identify parents of a co-resident child <18 
# RELSHIPP codes 20 = householder; 21 = opp-sex spouse;
# 22 = opp-sex unmarried partner; 23 = same-sex spouse; 24 = same-sex partner;
# 25 = bio child; 26 = adopted child; 27 = stepchild; 35 = foster child 
# Note: guardian is not easy to identify using this data set
child_codes  <- c(25, 26, 27, 35)
parent_codes <- c(20, 21, 22, 23, 24)

hh_with_minor <- il %>%
  filter(RELSHIPP %in% child_codes, AGEP < 18) %>%
  distinct(SERIALNO)

parents <- filter(il, SERIALNO %in% hh_with_minor$SERIALNO) %>%
  filter(RELSHIPP %in% parent_codes, AGEP >= 18)

# Variables to match the weighting variables in our data set
parents <- parents %>%
  mutate(
    age_cat = cut(as.numeric(AGEP), breaks = c(17,24,34,44,54,Inf),
                  labels = c("18-24","25-34","35-44","45-54","55+")),
    gender = case_when(SEX == 1 ~ "Male", SEX == 2 ~ "Female"),
    race_eth = case_when(
      HISP != "01"                       ~ "Hispanic",   # any Hispanic origin
      RAC1P == 1                         ~ "White",
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

# Weighted population margins from PUMS 
# Create the survey object and then use the weights to caluclate the numbers
pums_svy <- parents %>% as_survey_design(weights = PWGTP)

mk <- function(var) {
  pums_svy %>% group_by({{var}}) %>% summarize(n = survey_total()) %>%
    filter(!is.na({{var}})) %>% select(1, Freq = n)
}

pop_age <- mk(age_cat) %>% rename(age_cat = 1)
pop_gender <- mk(gender) %>%rename(gender = 1)
pop_race <- mk(race_eth) %>%rename(race_eth = 1)
pop_edu <- mk(edu) %>% rename(edu = 1)

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

data1_complete$raw_weight <- weights(svy1_raked)
summary(data1_complete$raw_weight) #' These are on a population scape
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

summary(data1_complete$normalized_weight)
mean(data1_complete$normalized_weight > 5) 

normalized_design <- svy1_raked
normalized_design$pweights <- data1_complete$normalized_weight

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

write_csv(data1_weighted, here::here("data", "il_cchs_data_20260629.csv"))

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

write_csv(demo_table_print, here::here("results", "cchs_demographics_20260629.csv")) 
