# IL Childcare and Children's Health Study (ILCCHS) — Public-Use Data

Public-use data and documentation from the first wave of the Illinois Childcare and Children's Health Study (CCHS), a cross-sectional online survey of Illinois parents and guardians of children under 18. The study covers demographics, social and environmental stressors, caregiving experiences and needs, perceptions of the environment, and physical activity.

**Study:** IRB25-1145 (University of Illinois Urbana-Champaign) · PI: Sheena Martenies, PhD · Sponsor: Institute for Government and Public Affairs

## Contents

- `cchs_public_use20260626.csv` — de-identified respondent-level data
- `cchs_public_data_dictionary_20260626.xlsx` — variable definitions and scale references
- `cchs_public_use_technical_doc_20260626.docx` — technical documentation

## Survey weights

Responses come from a non-probability (convenience) panel. Post-survey weights (`normalized_weight`) were built by raking to American Community Survey 5-year PUMS (2020–2024) targets for Illinois adults with a co-resident child under 18, across age, gender, race/ethnicity, and education. Use the weights for population estimates, e.g. in R:

Weighting efficiency ≈ 0.72 (effective n ≈ 748). 

## De-identification

Data are de-identified under the HIPAA Safe Harbor method. Direct identifiers, free-text fields, exact dates, and birth year were removed; age is released in bands. Anonymity cannot be guaranteed.

## Citation & contact

Please cite the study when using these data. Questions: Sheena Martenies (smarte4@illinois.edu).

*Released per the study's approved data-sharing plan. Identifiable data are available to researchers under an approved IRB protocol and data use agreement.*
