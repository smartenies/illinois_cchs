# IL Childcare and Children's Health Study (ILCCHS) — Public-Use Data

Public-use data and documentation from the Illinois Childcare and Children's Health Study (CCHS), a cross-sectional online survey of Illinois parents and guardians of children under 18. The study covers demographics, social and environmental stressors, caregiving experiences and needs, perceptions of the environment, and physical activity.

**Study:** IRB25-1145 (University of Illinois Urbana-Champaign) · PI: Sheena Martenies, PhD · Sponsor: Institute for Government and Public Affairs

## Contents

- `cchs_public_use_data_20260629.csv` — de-identified respondent-level data
- `cchs_public_data_dictionary_20260629.xlsx` — variable definitions and scale references
- `cchs_public_use_technical_doc_20260629.docx` — technical documentation

## Survey Domains

The study included several validated questionnaires including:

- All of Us Social Determinants of Health Survey (Koleck et al., 2024)
- Parental Stress Scale (Berry & Jones, 1995)
- USDA Food Security Module (Bickel et al., 2000)
- Survey of Household Economics and Decisionmaking (Federal Reserve System, 2024)
- Perceived Social Support (Zimet et al., 1988)
- Six Americas Super Short Survey (Chryst et al., 2018)
- Activity Support Scale for Multiple Groups (Lampard et al., 2016)

## Survey weights

Responses come from a non-probability (convenience) panel. Post-survey weights (`normalized_weight`) were built by raking to American Community Survey 5-year PUMS (2020–2024) targets for Illinois adults with a co-resident child under 18, across age, gender, race/ethnicity, and education.

Weighting efficiency ≈ 0.72 (effective n ≈ 748). 

## De-identification

Data are de-identified under the HIPAA Safe Harbor method. Direct identifiers, free-text fields, exact dates, and birth year were removed; age is released in bands. Anonymity cannot be guaranteed.

## Citation & Contact

Please cite the study when using these data:

Martenies, S. E., Davis Koester, B., & Raj, M. (2026). *Illinois Childcare and Children's Health Study* [Data set]. University of Illinois Urbana-Champaign. DOI TBA.

Questions: Sheena Martenies (smarte4@illinois.edu).

## References

Berry, J. O., & Jones, W. H. (1995). The Parental Stress Scale: Initial psychometric evidence. *Journal of Social and Personal Relationships*, *12*(3), 463–472. https://doi.org/10.1177/0265407595123009

Bickel, G., Nord, M., Price, C., Hamilton, W., & Cook, J. (2000). *Guide to measuring household food security, revised 2000*. U.S. Department of Agriculture, Food and Nutrition Service. https://nhis.ipums.org/nhis/resources/FSGuide.pdf

Board of Governors of the Federal Reserve System. (2024). *Survey of Household Economics and Decisionmaking*. https://www.federalreserve.gov/consumerscommunities/shed_data.htm

Chryst, B., Marlon, J., van der Linden, S., Leiserowitz, A., Maibach, E., & Roser-Renouf, C. (2018). Global Warming's "Six Americas Short Survey": Audience segmentation of climate change views using a four question instrument. *Environmental Communication*, *12*(8), 1109–1122. https://doi.org/10.1080/17524032.2018.1508047

Koleck, T. A., Dreisbach, C., Zhang, C., Grayson, S., Lor, M., Deng, Z., Conway, A., Higgins, P. D. R., & Bakken, S. (2024). User guide for Social Determinants of Health Survey data in the All of Us Research Program. *Journal of the American Medical Informatics Association*, *31*, 3032–3041. https://doi.org/10.1093/jamia/ocae214

Kolenikov, S. (2014). Calibrating survey data using iterative proportional fitting (raking). *Stata Journal*, *14*(1), 22–59. https://doi.org/10.1177/1536867X1401400102

Lampard, A. M., Nishi, A., Baskin, M. L., Carson, T. L., & Davison, K. K. (2016). The Activity Support Scale for Multiple Groups (ACTS-MG): Child-reported physical activity parenting in African American and non-Hispanic White families. *Behavioral Medicine*, *42*(2), 112–119. https://doi.org/10.1080/08964289.2014.979757

Lumley, T. (2004). Analysis of complex survey samples. *Journal of Statistical Software*, *9*(1), 1–19. https://doi.org/10.18637/jss.v009.i08

Lumley, T. (2020). *survey: Analysis of complex survey samples*. R package version 4.0. https://CRAN.R-project.org/package=survey

U.S. Census Bureau. (2024). *American Community Survey 5-year Public Use Microdata Sample: 2020–2024*. U.S. Department of Commerce. https://www.census.gov/programs-surveys/acs/microdata.html

Valliant, R. (2020). Comparing alternatives for estimation from nonprobability samples. *Journal of Survey Statistics and Methodology*, *8*(2), 231–263. https://doi.org/10.1093/jssam/smz003

Walker, K., & Herman, M. (2026). *tidycensus: Load US Census boundary and attribute data as 'tidyverse' and 'sf'-ready data frames* [R package]. https://CRAN.R-project.org/package=tidycensus

Zimet, G. D., Dahlem, N. W., Zimet, S. G., & Farley, G. K. (1988). The Multidimensional Scale of Perceived Social Support. *Journal of Personality Assessment*, *52*(1), 30–41. https://doi.org/10.1207/s15327752jpa5201_2

*Released per the study's approved data-sharing plan. Identifiable data are available to researchers under an approved IRB protocol and data use agreement.*
