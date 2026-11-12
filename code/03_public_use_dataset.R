#' -----------------------------------------------------------------------------
#' Date created: Jun 2, 2026
#' Last revised: Nov 12, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#'
#' Description: Prepares the data set for public release
#'
#' Required per our IRB protocol:
#' - Safe Harbor rules for deidentification
#' - County released with >5 participants are included
#'
#' Revisions (Nov 12, 2026):
#'   - Reads the re-weighted file from 02_assign_survey_weights.R (file_date)
#'   - Suspect-column check is printed for review
#'   - drop_demo setting now controls whether raw demographic vars are dropped
#'   - select(-geo_keep) changed to select(-all_of(geo_keep))
#' -----------------------------------------------------------------------------

library(tidyverse)

small_cell <- 5          # protocol: ZIP/county with <= this many participants is suppressed
drop_demo <- TRUE        # release collapsed weighting vars; drop raw source vars
file_date <- "20261112"  # must match file_date in 02_assign_survey_weights.R

pub_df <- read_csv(here::here("data", paste0("il_cchs_data_", file_date, ".csv")))
n_start <- nrow(pub_df)
cols_start <- names(pub_df)

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

#' REDCap process fields: survey timestamps and form "_complete" status
process_fields <- names(pub_df)[str_detect(names(pub_df),
                                           regex("timestamp|_complete$", ignore_case = T))]
process_fields

drop_now <- c(direct_identifiers, recontact_flags, age_vars, process_fields)

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
#' Same seed as the original release; if the input rows are in the same order,
#' each participant keeps the same public_id

set.seed(20260625)
pub_df <- pub_df %>%
  mutate(public_id = sample(seq_len(n()), n())) %>%
  relocate(public_id)

id_crosswalk <- select(pub_df, public_id, record_id)
write_csv(id_crosswalk, here::here("data", paste0("cchs_id_crosswalk_", file_date, ".csv")))

pub_df <- pub_df %>%
  select(-any_of("record_id"))

#' --------------------------------------------------
#' Drop raw demographic variables and keep collapsed groups
#' --------------------------------------------------

raw_demo_source <- c(
  paste0("demo_6___", 1:8),   # raw race/ethnicity checkboxes
  "demo_6",                   # raw race/ethnicity (if present as single col)
  "demo_14",                  # raw 6-level education
  "demo_5"                    # raw gender (gender retains Female/Male)
)

if (drop_demo) {
  pub_df <- pub_df %>% select(-any_of(raw_demo_source))
}

#' --------------------------------------------------
#' Small-cell scan across released demographic combinations
#' --------------------------------------------------

quasi_ids <- c("age_cat", "gender", "race_eth", "edu", geo_keep)

small_combos <- pub_df %>%
  filter(if_any(all_of(geo_keep), ~ !is.na(.))) %>%   # only rows with geography
  count(across(all_of(quasi_ids)), name = "n_cell") %>%
  filter(n_cell <= small_cell) %>%
  arrange(n_cell)
write_csv(small_combos, here::here("results", paste0("small_combos_", file_date, ".csv")))

#' 439 combinations were <= 5 in a cell in the original release, so county is
#' suppressed for now (pending OPRS/IRB decision)
nrow(small_combos)
pub_df <- select(pub_df, -all_of(geo_keep))

#' --------------------------------------------------
#' Final checks and export
#' --------------------------------------------------
#' Confirm no obvious identifier columns remain (review this list by hand;
#' an empty result is expected)
suspect <- names(pub_df)[str_detect(names(pub_df),
                                    regex("email|name|phone|ssn|address|timestamp|_complete$|specify",
                                          ignore_case = T))]
suspect

#' Records in vs. out (should match; no rows are removed in this script)
c(records_in = n_start, records_out = nrow(pub_df))

out_csv <- here::here("public_data", paste0("cchs_public_use_data_", file_date, ".csv"))
write_csv(pub_df, out_csv)

#' Write a suppression / de-identification log for the audit trail
cols_dropped <- setdiff(cols_start, names(pub_df))
cols_dropped

deid_log <- tibble(
  step = c(
    "Records in (weighted file)",
    "Records out (public file)",
    "Direct identifiers / free-text dropped",
    "Re-contact flags dropped",
    "Timestamp/process fields dropped",
    "Birth year (demo_4) and age dropped; age_cat retained",
    "Small-cell threshold",
    "ZIP records suppressed",
    "County records suppressed",
    "Raw demographic source vars dropped",
    "Demographic x geo combos <= threshold (flagged for review)",
    "Geography released",
    "record_id replaced with public_id",
    "All columns dropped"
  ),
  detail = c(
    n_start,
    nrow(pub_df),
    paste(intersect(direct_identifiers, cols_start), collapse = "; "),
    paste(intersect(recontact_flags, cols_start), collapse = "; "),
    ifelse(length(process_fields) > 0, paste(process_fields, collapse = "; "), "none present"),
    paste(intersect(age_vars, cols_start), collapse = "; "),
    small_cell,
    zip_suppressed,
    county_suppressed,
    ifelse(drop_demo, paste(intersect(raw_demo_source, cols_start), collapse = "; "), "no"),
    nrow(small_combos),
    "none (ZIP and county both withheld)",
    "yes (crosswalk kept in restricted environment only)",
    paste(cols_dropped, collapse = "; ")
  )
)
deid_log
write_csv(deid_log, here::here("public_data", paste0("cchs_deidentification_log_", file_date, ".csv")))
