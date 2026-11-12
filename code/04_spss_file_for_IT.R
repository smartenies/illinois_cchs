#' -----------------------------------------------------------------------------
#' Date created: Jun 29, 2026
#' Last revised: Nov 12, 2026
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#'
#' Description: Export the public-use data set to SPSS (.sav) format, with
#' variable and value labels pulled from the REDCap-style data dictionary
#'
#' Revisions (Nov 12, 2026):
#'   - Reads the re-weighted public file (file_date)
#'   - Choice lists without codes (age_cat, gender, edu, race_eth) previously
#'     produced blank value labels; these text variables now keep their values
#'     as-is with no value labels
#'   - Columns read as logical (all missing) are converted to numeric so
#'     labelled() does not fail
#'   - Matrix items (bn_2a to bn_2g) get their question stem in the label
#'   - Checks added: variables missing from the dictionary, and a read-back of
#'     the exported .sav file
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(haven)

file_date <- "20261112"   #' must match file_date in 02 and 03
dict_date <- "20260629"   #' data dictionary version

#'==============================================================================
#' 1. Read the data dictionary and data set
#'==============================================================================

dict <- read_excel(here::here("public_data",
                              paste0("cchs_public_data_dictionary_", dict_date, ".xlsx")),
                   sheet = "Public-Use Dictionary")

df <- read_csv(here::here("public_data",
                          paste0("cchs_public_use_data_", file_date, ".csv")))

dim(df)

#' Columns read as logical are entirely missing; make them numeric so value
#' labels can be applied
logical_cols <- names(df)[map_lgl(df, is.logical)]
logical_cols
df <- df %>% mutate(across(all_of(logical_cols), as.numeric))

#'==============================================================================
#' 2. Helper: parse a "Choices, Calculations, OR Slider Labels" string into
#'    a named vector suitable for haven::labelled()
#'    Input format:  "1, Yes | 2, No | 3, Don't know"
#'    Output:         c(Yes = 1, No = 2, "Don't know" = 3)
#'    Returns NULL when there are no choices, or when the choices have no codes
#'    (e.g., "Female | Male"), since those variables are stored as text
#'==============================================================================

parse_choices <- function(choices_str) {
  if (is.na(choices_str) || str_trim(choices_str) == "") return(NULL)

  pairs <- tibble(pair = str_trim(str_split(choices_str, "\\|")[[1]]))

  #' Choices without "code, label" pairs: no value labels
  if (!all(str_detect(pairs$pair, ","))) return(NULL)

  #' Split only on the FIRST comma, since labels can contain commas
  pairs <- pairs %>%
    mutate(code = str_trim(str_extract(pair, "^[^,]+")),
           label = str_trim(str_remove(pair, "^[^,]+,")),
           code_num = suppressWarnings(as.numeric(code)))

  #' Numeric codes (typical REDCap coding); otherwise character codes
  if (!any(is.na(pairs$code_num))) {
    set_names(pairs$code_num, pairs$label)
  } else {
    set_names(pairs$code, pairs$label)
  }
}

#'==============================================================================
#' 3. Build a lookup: variable name -> variable label and value labels
#'==============================================================================

#' Question stems for matrix items, whose field labels are only the row text
matrix_stems <- tribble(
  ~prefix, ~stem,
  "bn_2", "Financial challenge or concern for you or your family:"
)

value_label_lookup <- dict %>%
  select(var_name = `Variable / Field Name`,
         field_label = `Field Label`,
         choices = `Choices, Calculations, OR Slider Labels`) %>%
  mutate(field_label = str_squish(field_label),
         prefix = str_extract(var_name, "^bn_2"),
         value_labels = map(choices, parse_choices)) %>%
  left_join(matrix_stems, by = "prefix") %>%
  mutate(field_label = ifelse(!is.na(stem), paste(stem, field_label), field_label)) %>%
  select(-prefix, -stem)

#' Variables in the data that are not in the dictionary (exported unlabelled)
setdiff(names(df), value_label_lookup$var_name)

#' Variables in the dictionary that are not in the data (fine; for reference)
setdiff(value_label_lookup$var_name, names(df))

#'==============================================================================
#' 4. Apply labels to the public-use data frame
#'==============================================================================

apply_dictionary_labels <- function(df, lookup) {
  for (v in names(df)) {

    row <- lookup %>% filter(var_name == v)
    if (nrow(row) == 0) next  #' variable not in dictionary; leave as-is

    #' Value labels, if this variable has coded choices
    vl <- row$value_labels[[1]]
    if (!is.null(vl)) {
      df[[v]] <- labelled(df[[v]], labels = vl)
    }

    #' Variable (column) label, if present
    if (!is.na(row$field_label[1]) && row$field_label[1] != "") {
      attr(df[[v]], "label") <- row$field_label[1]
    }
  }
  df
}

df_labelled <- apply_dictionary_labels(df, value_label_lookup)

#'==============================================================================
#' 5. Export to SPSS format and check the file
#'==============================================================================

sav_file <- here::here("public_data", paste0("cchs_public_use_data_", file_date, ".sav"))
write_sav(df_labelled, sav_file)

#' Read the file back in and compare with the data set
sav_check <- read_sav(sav_file)

c(rows_csv = nrow(df), rows_sav = nrow(sav_check),
  cols_csv = ncol(df), cols_sav = ncol(sav_check))

#' Spot-check labels for a few variables
check_vars <- intersect(c("bn_1", "bn_2a", "fh_1", "cn_8", "edu", "normalized_weight"),
                        names(sav_check))

map(check_vars, function(v) {
  tibble(variable = v,
         variable_label = attr(sav_check[[v]], "label") %||% NA_character_,
         value_labels = paste(names(attr(sav_check[[v]], "labels")), collapse = " | "))
}) %>%
  bind_rows() %>%
  print(width = Inf)

#' Number of variables with a variable label and with value labels
tibble(with_variable_label = sum(map_lgl(sav_check, ~ !is.null(attr(.x, "label")))),
       with_value_labels = sum(map_lgl(sav_check, ~ !is.null(attr(.x, "labels")))),
       total = ncol(sav_check))
