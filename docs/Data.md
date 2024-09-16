
# Data files

## Data from external resources
#### Demography

- **uk_mor.xlsx**: background mortality
- **uk_pop_data.xlsx**: historical population sizes
- **uk_pop_proj_2020.xls**: population sizes projections

#### Population norm
- [qale_shortfall.csv](data/qale_shortfall.csv) Arranged population norm variations
- Selected variants:
  - EQ-5D-5L: health state profiles from 2017-2018 waves of the Health Survey for England (HSE)=
  - Alava et al. crosswalk method relating EQ-5D-5L to EQ-5D-3L scoring
  - Non-institutionalised general population
- Source: McNamara S, Schneider PP, Love-Koh J, Doran T, Gutacker N. Quality-Adjusted Life Expectancy Norms for the English Population. Value in Health. 2022 Aug 12. [link](https://doi.org/10.1016/j.jval.2022.07.005)

## Generated datasets
#### Supplementary data for demographics [Sup_demo.rdata](data/Sup_demo.rdata):

- **sup_demo**: demographic inputs
- **sup_demo_s**: demographic inputs stratified by sex 
- Age, Year, Sex, Background mortality, and population norm
- Age: 0-100
- Year: 2012 - 2070
- Sex: binary
- norm_leoss for population norm smoothed with LEOSS spline on age

#### EQ5D related to herpes zoster
- [27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv](data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv)
- individual data fetched from reviewed studies listed in **studies/**


## Reformed datasets for analysis 
- [qol_reformed.rdata](data/qol_reformed.rdata)
- Using 2023 population sizes and mortalities
- Health: boolean indicator of health status affected by HZ (0: QoL affected by HZ, 1: not affected)
- Q_rescaled: EQ-5D scores rescaled into [0, 1]
- Norm_rescaled: population norm rescaled using the ranges as Q_rescaled
- ti: time in days



