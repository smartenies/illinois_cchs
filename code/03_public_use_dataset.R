#' -----------------------------------------------------------------------------
#' Date created: Jun 2, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Prepares the data set for public release
#' 
#' Required per our IRB protocol:
#' - Safe Harbor rules for deidentification
#' - County released with >5 participants are included
#' -----------------------------------------------------------------------------

library(tidyverse)

small_cell <- 5          # protocol: ZIP/county with <= this many participants is suppressed
drop_demo <- TRUE        # release collapsed weighting vars; drop raw source vars

pub_df <- read_csv(here::here("data", "il_cchs_data_20260629.csv"))
n_start <- nrow(pub_df)

#' --------------------------------------------------
#' Direct identifiers that are to be removed:
#' --------------------------------------------------
#' Identifiers flagged in the data dictionary (Identifier? = y) and all free-text
#' "please specify" fields, which can contain names/employers/unique detail.
#'   - demo_5a          : free-text gender specify
#'   - demo_6a          : free-text race/ethnicity specify
#'   - demo_9a          : free-text language specify
#'   - screen_3b        : free-text childcare role specify

direct_identifiers <- c(
  "demo_5a", "demo_6a", "demo_9a",  # free-text "specify" fields
  "screen_3b",                      # free-text childcare role specify
  "cn_2a", "cn_4a", "cn_5a"         # free-text childcare needs questions
)

recontact_flags <- c("contact_1", "contact_2")

age_vars <- c("demo_4", "age") #' year of birth and age

drop_now <- c(direct_identifiers, recontact_flags, age_vars)

pub_df <- pub_df %>% select(-any_of(drop_now))

#' --------------------------------------------------
#' <=5 suppression rule for geography
#' --------------------------------------------------
#' demo_1 = ZIP code (flagged Identifier = y), demo_2 = county.
#' Rule: if 5 or fewer participants share a ZIP (or county), suppress that
#' identifier for those participants.

has_zip    <- "demo_1" %in% names(pub_df)
has_county <- "demo_2" %in% names(pub_df)

zip_suppressed <- county_suppressed <- 0L

if (has_zip) {
  pub_df <- pub_df %>% 
    group_by(demo_1) %>% mutate(.zip_n = n()) %>% ungroup()
  zip_suppressed <- sum(pub_df$.zip_n <= small_cell & !is.na(pub_df$demo_1))
  pub_df <- pub_df %>%
    mutate(zip = if_else(.zip_n <= small_cell, NA_character_, as.character(demo_1)))
}

if (has_county) {
  pub_df <- pub_df %>% group_by(demo_2) %>% mutate(.cnty_n = n()) %>% ungroup()
  county_suppressed <- sum(pub_df$.cnty_n <= small_cell & !is.na(pub_df$demo_2))
  pub_df <- pub_df %>%
    mutate(county = if_else(.cnty_n <= small_cell, NA_integer_, as.integer(demo_2)))
}

geo_drop <- "zip" #zip or county
geo_keep <- "county"

pub_df <- pub_df %>%
  select(-any_of(c("demo_1", "demo_2", ".zip_n", ".cnty_n", geo_drop)))

table(pub_df$county)

#' --------------------------------------------------
#' new public ID
#' --------------------------------------------------

set.seed(20260625)
pub_df <- pub_df %>%
  mutate(public_id = sample(seq_len(n()), n())) %>%
  relocate(public_id)

id_crosswalk <- select(pub_df, public_id, record_id)
write_csv(id_crosswalk, here::here("data", "cchs_id_crosswalk_20260629.csv"))

pub_df <- pub_df %>%
  select(-any_of("record_id"))

#' --------------------------------------------------
#' Drop raw demographic variables and keep collapsed groups
#' --------------------------------------------------

raw_demo_source <- c(
  paste0("demo_6___", 1:8),   # raw race/ethnicity checkboxes
  "demo_6",                   # raw race/ethnicity (if present as single col)
  "demo_14",                  # raw 6-level education
  "demo_5"                    # raw gender (gender_weight retains Female/Male)
)

pub_df <- pub_df %>% select(-any_of(raw_demo_source))

#' --------------------------------------------------
#' Small-cell scan across released demographic combinations
#' --------------------------------------------------

quasi_ids <- c("age_cat", "gender", "race_eth", "edu", geo_keep)

small_combos <- pub_df %>%
  filter(if_any(all_of(geo_keep), ~ !is.na(.))) %>%   # only rows with geography
  count(across(all_of(quasi_ids)), name = "n_cell") %>%
  filter(n_cell <= small_cell) %>%
  arrange(n_cell)
write_csv(small_combos, here::here("results", "small_combos.csv"))

#' 439 combinations are <= 5 in a cell. Going to supress county for now
pub_df <- select(pub_df, -county)

#' --------------------------------------------------
#' 8. Final checks and export
#' --------------------------------------------------
#' Confirm no obvious identifier columns remain.
suspect <- names(pub_df)[str_detect(names(pub_df),
                                 regex("email|aid|name|phone|ssn|address|timestamp|_complete$|specify",
                                       ignore_case = TRUE))]

out_csv <- here::here("public_data", "cchs_public_use_data_20260626.csv")
write_csv(pub_df, out_csv)

#' Write a suppression / de-identification log for the audit trail
deid_log <- tibble(
  step = c(
    "Records in (weighted file)",
    "Records out (public file)",
    "Direct identifiers / free-text dropped",
    "Re-contact flags dropped",
    "Timestamp/process fields dropped",
    "Birth year (demo_4) dropped; age_cat retained",
    "Small-cell threshold",
    "ZIP records suppressed",
    "County records suppressed",
    "Raw demographic source vars dropped",
    "Demographic x geo combos <= threshold (flagged for review)",
    "record_id replaced with public_id"
  ),
  detail = c(
    n_start,
    nrow(pub_df),
    paste(intersect(direct_identifiers, c(direct_identifiers)), collapse = "; "),
    paste(recontact_flags, collapse = "; "),
    "exact timestamps removed (year-level only retained where applicable)",
    "yes",
        small_cell,
    zip_suppressed,
    county_suppressed,
    ifelse(drop_demo, "yes", "no"),
    nrow(small_combos),
    "yes (crosswalk kept in restricted environment only)"
  )
)
deid_log
write_csv(deid_log, here::here("public_data", "cchs_deidentification_log_20260629.csv"))

