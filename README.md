# IL Childcare and Children's Health Study (ILCCHS) — Public-Use Data

Public-use data and documentation from the Illinois Childcare and Children's Health Study (CCHS), a cross-sectional online survey of Illinois parents, guardians, and foster parents of children under 18. The study covers demographics, social and environmental stressors, caregiving experiences and needs, perceptions of the environment, and physical activity.

**Study:** IRB25-1145 (University of Illinois Urbana-Champaign) · PI: Sheena Martenies, PhD · Sponsor: Institute for Government and Public Affairs

## Repository Contents

### Public-use data (`public_data/`)

- `cchs_public_use_data_20261112.csv` — de-identified respondent-level data (1,053 respondents; 1,042 with a survey weight)
- `cchs_public_use_data_20261112.sav` — the same data in SPSS format, with variable and value labels
- `cchs_public_data_dictionary_20260629.xlsx` — variable definitions and scale references
- `cchs_survey.pdf` — the survey instrument

### Documentation (`documentation/`)

- `cchs_public_use_technical_doc_20261124.docx` — technical documentation: eligibility screening and participant flow, survey weights, demographics of the final data set, preparation of the public-use file, and guidance for analysts (including scoring notes)

### Code (`code/`)

Scripts are numbered in the order they are run:

- `01_screen_completes.R` — Screens survey completes for eligibility and data quality, removes test responses, and saves a de-identified raw file. *Reads identifiable data stored on Box Health; included for transparency but cannot be run outside the study team.*
- `02_assign_survey_weights.R` — Builds post-survey weights by raking to ACS PUMS targets and checks that weighted totals match the targets.
- `03_public_use_dataset.R` — Prepares the public-use file: applies Safe Harbor de-identification, withholds geography, assigns new random IDs, and writes the de-identification log.
- `04_spss_file_for_IT.R` — Exports the public-use data to SPSS (.sav) format with labels from the data dictionary.

Intermediate files (`raw_data/`, `data/`, `results/`) contain identifiable or restricted information and are not included in this repository.

## Survey Domains

The study included several validated questionnaires including:

- All of Us Social Determinants of Health Survey (Koleck et al., 2024)
- Parental Stress Scale (Berry & Jones, 1995)
- USDA Food Security Module (Bickel et al., 2000)
- Survey of Household Economics and Decisionmaking (Federal Reserve System, 2024)
- Multidimensional Scale of Perceived Social Support (Zimet et al., 1988)
- Six Americas Super Short Survey (Chryst et al., 2018)
- Activity Support Scale for Multiple Groups (Lampard et al., 2016)

## Survey Weights

Responses come from a non-probability (convenience) panel. Post-survey weights (`normalized_weight`) were built by raking to American Community Survey 5-year PUMS (2020–2024) targets for Illinois adults with a co-resident child under 18, across age, gender, race/ethnicity, and education. Household income was not used in the weighting.

- Eleven respondents were missing a weighting variable and have a weight of 0.
- Normalized weights range from 0.16 to 3.53. Kish effective sample size ≈ 797 for the 1,042 weighted respondents (efficiency ≈ 0.76).

To use the weights in R:

```r
library(survey)
des <- svydesign(ids = ~1, weights = ~normalized_weight, data = cchs)
```

Handle the zero-weight respondents with `subset()` on the design rather than deleting them first.

The weights correct only for the four demographics above. Levels of hardship (for example, food insecurity) are higher in this sample than in national estimates, so comparisons between groups are more reliable than overall prevalence. See the technical documentation for details.

## De-identification

Data are de-identified under the HIPAA Safe Harbor method. Direct identifiers, free-text fields, exact dates, and birth year were removed; age is released in bands, and race/ethnicity, gender, and education are released only as collapsed categories. ZIP code and county are not included in this release. Anonymity cannot be guaranteed. See `cchs_deidentification_log_20261112.csv` for details.

## Software

All processing was done in R using the following packages: tidyverse, here, lubridate, readxl, tidyREDCap, tigris, zipcodeR, tidycensus, survey, srvyr, and haven. Script `02_assign_survey_weights.R` downloads ACS PUMS data with tidycensus, which requires a free Census API key.

## Version History

- **2026-11-24** — Technical documentation revised: participant flow, weighting details and diagnostics, public-use file preparation, and guidance for analysts. Moved to `documentation/`.
- **2026-11-12** — Corrected an error in the survey weights. PUMS variables were being read as text, which excluded some households with young children from the weighting targets and undercounted the lowest education category. Respondents who selected both White and Middle Eastern or North African are now coded White, consistent with ACS. Weights were re-estimated and all public files were updated. Files dated 20260629 are superseded.
- **2026-06-29** — Initial public release.

## Citation & Contact

Please cite the study when using these data:

Martenies, S. E., Davis Koester, B., & Raj, M. (2026). *Illinois Childcare and Children's Health Study* [Data set]. University of Illinois Urbana-Champaign. DOI TBA.

Questions: Sheena Martenies (smarte4@illinois.edu).

## References

References for the survey instruments and for methods cited in the technical documentation.

Berry, J. O., & Jones, W. H. (1995). The Parental Stress Scale: Initial psychometric evidence. *Journal of Social and Personal Relationships*, *12*(3), 463–472. https://doi.org/10.1177/0265407595123009

Bickel, G., Nord, M., Price, C., Hamilton, W., & Cook, J. (2000). *Guide to measuring household food security, revised 2000*. U.S. Department of Agriculture, Food and Nutrition Service. https://nhis.ipums.org/nhis/resources/FSGuide.pdf

Board of Governors of the Federal Reserve System. (2024). *Survey of Household Economics and Decisionmaking*. https://www.federalreserve.gov/consumerscommunities/shed_data.htm

Chryst, B., Marlon, J., van der Linden, S., Leiserowitz, A., Maibach, E., & Roser-Renouf, C. (2018). Global Warming's "Six Americas Short Survey": Audience segmentation of climate change views using a four question instrument. *Environmental Communication*, *12*(8), 1109–1122. https://doi.org/10.1080/17524032.2018.1508047

Kish, L. (1965). *Survey sampling*. John Wiley & Sons.

Koleck, T. A., Dreisbach, C., Zhang, C., Grayson, S., Lor, M., Deng, Z., Conway, A., Higgins, P. D. R., & Bakken, S. (2024). User guide for Social Determinants of Health Survey data in the All of Us Research Program. *Journal of the American Medical Informatics Association*, *31*, 3032–3041. https://doi.org/10.1093/jamia/ocae214

Kolenikov, S. (2014). Calibrating survey data using iterative proportional fitting (raking). *Stata Journal*, *14*(1), 22–59. https://doi.org/10.1177/1536867X1401400102

Lampard, A. M., Nishi, A., Baskin, M. L., Carson, T. L., & Davison, K. K. (2016). The Activity Support Scale for Multiple Groups (ACTS-MG): Child-reported physical activity parenting in African American and non-Hispanic White families. *Behavioral Medicine*, *42*(2), 112–119. https://doi.org/10.1080/08964289.2014.979757

Lumley, T. (2004). Analysis of complex survey samples. *Journal of Statistical Software*, *9*(1), 1–19. https://doi.org/10.18637/jss.v009.i08

Lumley, T. (2024). *survey: Analysis of complex survey samples*. R package version 4.5. https://CRAN.R-project.org/package=survey

U.S. Census Bureau. (2024). *American Community Survey 5-year Public Use Microdata Sample: 2020–2024*. U.S. Department of Commerce. https://www.census.gov/programs-surveys/acs/microdata.html

Valliant, R. (2020). Comparing alternatives for estimation from nonprobability samples. *Journal of Survey Statistics and Methodology*, *8*(2), 231–263. https://doi.org/10.1093/jssam/smz003

Walker, K., & Herman, M. (2026). *tidycensus: Load US Census boundary and attribute data as 'tidyverse' and 'sf'-ready data frames* [R package]. https://CRAN.R-project.org/package=tidycensus

Zimet, G. D., Dahlem, N. W., Zimet, S. G., & Farley, G. K. (1988). The Multidimensional Scale of Perceived Social Support. *Journal of Personality Assessment*, *52*(1), 30–41. https://doi.org/10.1207/s15327752jpa5201_2

*Released per the study's approved data-sharing plan. Identifiable data are available to researchers under an approved IRB protocol and data use agreement.*
