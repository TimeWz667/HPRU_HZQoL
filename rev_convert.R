library(haven)
library(tidyverse)


## 

# EQ-5D-3L to health index conversions. 
# UK, NL summarized in:
# Szenda A, Janssen B, Cabases J, eds 2014. Self-Reported Population Health: An 
# International Perspective based on EQ-5D. Springer Open. Available at:
# https://link.springer.com/book/10.1007/978-94-007-7596-1
# See table 2.2, p. 13
#
# UK: MVH Group (1995) The measurement and valuation of health. Final report on 
# the modelling of valuation tariffs. MVH Group, Centre for Health Economics, 
# York
QALY_UK = function(EqMo, EqSC, EqUA, EqPD, EqAD) {
  if (missing(EqSC)) {
    EqMo = as.numeric(EqMo)
    EqAD = EqMo %% 10
    EqPD = (EqMo %/% 10) %% 10
    EqUA = (EqMo %/% 100) %% 10
    EqSC = (EqMo %/% 1000) %% 10
    EqMo = (EqMo %/% 10000)
  }
  deduct = 
    ifelse(EqMo >= 2 | EqSC >= 2 | EqUA >= 2 | EqPD >= 2 | EqAD >= 2, 0.081, 0) +
    ifelse(EqMo == 3 | EqSC == 3 | EqUA == 3 | EqPD == 3 | EqAD == 3, 0.269, 0) +
    ifelse(EqMo == 2, 0.069, 0) +
    ifelse(EqMo == 3, 0.314, 0) +
    ifelse(EqSC == 2, 0.104, 0) +
    ifelse(EqSC == 3, 0.214, 0) +
    ifelse(EqUA == 2, 0.036, 0) +
    ifelse(EqUA == 3, 0.094, 0) +
    ifelse(EqPD == 2, 0.123, 0) +
    ifelse(EqPD == 3, 0.386, 0) +
    ifelse(EqAD == 2, 0.071, 0) +
    ifelse(EqAD == 3, 0.236, 0)
  1 - deduct
}

# NL: Lamers LM, McDonnell J, Stalmeier PFM, Krabbe PFM, Busschbach JJV (2006) 
# The Dutch tariff: results and arguments for a cost-effective design for 
# national EQ-5D valuation studies. Health Econ 15(10):1121–1132
QALY_NL = function(EqMo, EqSC, EqUA, EqPD, EqAD) {
  if (missing(EqSC)) {
    EqMo = as.numeric(EqMo)
    EqAD = EqMo %% 10
    EqPD = (EqMo %/% 10) %% 10
    EqUA = (EqMo %/% 100) %% 10
    EqSC = (EqMo %/% 1000) %% 10
    EqMo = (EqMo %/% 10000)
  }
  deduct = 
    ifelse(EqMo >= 2 | EqSC >= 2 | EqUA >= 2 | EqPD >= 2 | EqAD >= 2, 0.071, 0) +
    ifelse(EqMo == 3 | EqSC == 3 | EqUA == 3 | EqPD == 3 | EqAD == 3, 0.234, 0) +
    ifelse(EqMo == 2, 0.036, 0) +
    ifelse(EqMo == 3, 0.161, 0) +
    ifelse(EqSC == 2, 0.082, 0) +
    ifelse(EqSC == 3, 0.152, 0) +
    ifelse(EqUA == 2, 0.032, 0) +
    ifelse(EqUA == 3, 0.057, 0) +
    ifelse(EqPD == 2, 0.086, 0) +
    ifelse(EqPD == 3, 0.329, 0) +
    ifelse(EqAD == 2, 0.124, 0) +
    ifelse(EqAD == 3, 0.325, 0)
  1 - deduct
}

# LA: Zarate V, Kind P, Chuang LH (2008) Hispanic Valuation of the EQ-5D Health 
# States: A Social Value Set for Latin Americans. Value in Health 11: 1170-1177
# This is the N3 + X4 model, and applies to Spanish-speaking Hispanics.
QALY_LA = function(EqMo, EqSC, EqUA, EqPD, EqAD) {
  if (missing(EqSC)) {
    EqMo = as.numeric(EqMo)
    EqAD = EqMo %% 10
    EqPD = (EqMo %/% 10) %% 10
    EqUA = (EqMo %/% 100) %% 10
    EqSC = (EqMo %/% 1000) %% 10
    EqMo = (EqMo %/% 10000)
  }
  deduct = 
    ifelse(EqMo >= 2 | EqSC >= 2 | EqUA >= 2 | EqPD >= 2 | EqAD >= 2, 0.125, 0) + # Constant
    ifelse(EqMo == 3 | EqSC == 3 | EqUA == 3 | EqPD == 3 | EqAD == 3, 0.079, 0) + # N3
    ifelse((EqMo >= 2) + (EqSC >= 2) + (EqUA >= 2) + 
             (EqPD >= 2) + (EqAD >= 2) >= 4, -0.074, 0) + # X4 (yes, supposed to be negative)
    ifelse(EqMo == 2, 0.047, 0) +
    ifelse(EqMo == 3, 0.290, 0) +
    ifelse(EqSC == 2, 0.054, 0) +
    ifelse(EqSC == 3, 0.176, 0) +
    ifelse(EqUA == 2, 0.081, 0) +
    ifelse(EqUA == 3, 0.181, 0) +
    ifelse(EqPD == 2, 0.103, 0) +
    ifelse(EqPD == 3, 0.202, 0) +
    ifelse(EqAD == 2, 0.060, 0) +
    ifelse(EqAD == 3, 0.122, 0)
  1 - deduct
}

# JN: Tsuchiya A, Ikeda S, Ikegami N, Nishimura S, Sakai I, Fukuda T, 
# Hamashima C, Hisashige A, Tamura M (2002) Estimating an EQ-5D population 
# value set: the case of Japan. Health Econ 11: 341-353
# This is the N3 model.
QALY_JP = function(EqMo, EqSC, EqUA, EqPD, EqAD) {
  if (missing(EqSC)) {
    EqMo = as.numeric(EqMo)
    EqAD = EqMo %% 10
    EqPD = (EqMo %/% 10) %% 10
    EqUA = (EqMo %/% 100) %% 10
    EqSC = (EqMo %/% 1000) %% 10
    EqMo = (EqMo %/% 10000)
  }
  deduct = 
    ifelse(EqMo >= 2 | EqSC >= 2 | EqUA >= 2 | EqPD >= 2 | EqAD >= 2, 0.148, 0) +
    ifelse(EqMo == 3 | EqSC == 3 | EqUA == 3 | EqPD == 3 | EqAD == 3, 0.014, 0) +
    ifelse(EqMo == 2, 0.078, 0) +
    ifelse(EqMo == 3, 0.418, 0) +
    ifelse(EqSC == 2, 0.053, 0) +
    ifelse(EqSC == 3, 0.101, 0) +
    ifelse(EqUA == 2, 0.040, 0) +
    ifelse(EqUA == 3, 0.128, 0) +
    ifelse(EqPD == 2, 0.083, 0) +
    ifelse(EqPD == 3, 0.189, 0) +
    ifelse(EqAD == 2, 0.062, 0) +
    ifelse(EqAD == 3, 0.108, 0)
  1 - deduct
}

# Create tables of EQ5D3 codes and index values
#EQ5D3L = CJ(EqMo = 1:3, EqSC = 1:3, EqUA = 1:3, EqPD = 1:3, EqAD = 1:3)
#EQ5D3L[, code := paste0(EqMo, EqSC, EqUA, EqPD, EqAD)]
#TableUK = EQ5D3L[, structure(QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]
#TableLA = EQ5D3L[, structure(QALY_LA(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]
#TableJN = EQ5D3L[, structure(QALY_JN(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]


eq5d_vset <- crossing(EqMo = 1:3, EqSC = 1:3, EqUA = 1:3, EqPD = 1:3, EqAD = 1:3) %>% 
  mutate(
    code = paste0(EqMo, EqSC, EqUA, EqPD, EqAD),
    Q_UK = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD),
    Q_LA = QALY_LA(EqMo, EqSC, EqUA, EqPD, EqAD),
    Q_JP = QALY_JP(EqMo, EqSC, EqUA, EqPD, EqAD),
    Q_NL = QALY_NL(EqMo, EqSC, EqUA, EqPD, EqAD)
  )

#all(TableUK == eq5d_vset %>% pull(Q_UK))
#all(TableLA == eq5d_vset %>% pull(Q_LA))
#all(TableJN == eq5d_vset %>% pull(Q_JN))


eq5d <- list()

## Scott et al data -----
eq5d$Scott <- local({
  sc <- read_csv(here::here("data", "raw", "Data_Scott.csv"))

  sc <- sc %>% 
    # Exclude patients who withdrew consent
    filter(!(V2STATUS == 2 | V3STATUS == 2 | V4STATUS == 2)) %>% 
    # Exclude patients marked as "not shingles"
    filter(!(V2STATUS == 4 | V3STATUS == 4 | V4STATUS == 4)) %>% 
    mutate(
      RASHSTDA = dmy(RASHSTDA),
      V1DATE = dmy(V1DATE),
      V2DATE = dmy(V2DATE),
      V3DATE = dmy(V3DATE),    # 1 failed to parse
      VISDAT4 = dmy(VISDAT4)  # 2 failed to parse
    )
  
  sc_reg <- sc %>% 
    mutate(
      country = "United Kingdom",
      study = "Scott et al. 2006",
      t_1 = as.numeric(V1DATE - RASHSTDA),
      t_2 = as.numeric(V2DATE - RASHSTDA),
      t_3 = as.numeric(V3DATE - RASHSTDA),
      t_4 = as.numeric(VISDAT4 - RASHSTDA)
    ) %>% 
    rename(
      id = ID, 
      age = AGEV1,
      EqMo_1 = EQ_MOBIL, EqSC_1 = EQ_SELFC, EqUA_1 = EQ_USACT, EqPD_1 = EQ_PAIND, EqAD_1 = EQ_ANXD,
      EqMo_2 = EQ_MOBIL, EqSC_2 = EQSELFC2, EqUA_2 = EQUSACT2, EqPD_2 = EQPAIND2, EqAD_2 = EQANXD2,
      EqMo_3 = EQ_MOBIL, EqSC_3 = EQSELFC3, EqUA_3 = EQUSACT3, EqPD_3 = EQPAIND3, EqAD_3 = EQANXD3,
      EqMo_4 = EQ_MOBIL, EqSC_4 = EQSELFC4, EqUA_4 = EQUSACT4, EqPD_4 = EQPAIND4, EqAD_4 = EQANXD4
    )  %>% 
    select(country, study, id, age, id, matches("t_\\d"), matches("EQ\\w{2}_\\d")) %>% 
    mutate(across(matches("EQ\\w{2}_\\d"), as.numeric)) %>% 
    pivot_longer(-c(country, study, id, age), names_sep = "_", names_to = c("name", "visit")) %>% 
    pivot_wider() %>% 
    mutate(
      visit = as.numeric(visit), 
      id = as.character(id), 
      is_baseline = F,
      is_perfect = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD) >= 1
    )

  sc_reg
})



## Master data -----
eq5d$Master <- local({
  master <- as_tibble(read_sas(here::here("data", "raw", "Data_Master_All Patients_14MAY2017.sas7bdat")))
  
  # Note - the MASTER study appears to have calculated several QALY values for the
  # LA dataset with several entries that are "off" by 0.74 or 0.79 from the
  # values calculated above
  master <- with(eq5d_vset, {
    master %>% 
      mutate(
        code_uk = sapply(EUROQOL_UK, \(v) code[which.min(abs(v - Q_UK))[1]]),
        code_jp = sapply(EUROQOL_JAN, \(v) code[which.min(abs(v - Q_JP))[1]]),
        code_la = sapply(EUROQOL_LA, \(v) code[which.min(abs(v - Q_LA))[1]])
      )
  })

  master <- master %>% 
    mutate(
      EqMo = as.numeric(substr(code_uk, 1, 1)),
      EqSC = as.numeric(substr(code_uk, 2, 2)),
      EqUA = as.numeric(substr(code_uk, 3, 3)),
      EqPD = as.numeric(substr(code_uk, 4, 4)),
      EqAD = as.numeric(substr(code_uk, 5, 5)),
      visit_number = as.numeric(stringr::str_remove_all(Visit, "[A-Z]"))
    ) %>% 
    arrange(Country, USUBJID, visit_number)
  
  
  # Add age
  master_age <- read_csv(here::here("data", "raw", "Data_Master_Baseline Characteristics_All Patients_14MAY2017.csv"))
  
  master_age <- master_age %>% 
    select(Country, USUBJID, age = Age_BL, onset = Duration_Rash_Onset)

  # Get timesteps for patients in Canadian data
  master_age_cad <- read_sas(here::here("data", "raw", "Data_master.sas7bdat"))
  
  master_age_cad <- master_age_cad %>% 
    select(USUBJID = unique, starts_with("days_since_onset_")) %>% 
    pivot_longer(-USUBJID, names_pattern = "days_since_onset_(\\d+)", names_to = "visit_number", values_to = "t") %>% 
    mutate(
      USUBJID = as.character(USUBJID),
      visit_number = as.numeric(visit_number)
    )
  

  master_bind <- master %>% 
    left_join(master_age, by = c("Country", "USUBJID"))
  
  sch_time <- tibble(
    mea_time = c(-Inf, 0, 7, 14, 21, 30, 60, 90, 120, 150, 181),
    visit_number = 0:10
  )
  
  
  master_bind <- bind_rows(
    master_bind %>% 
      filter(Country == "Canada") %>% 
      left_join(master_age_cad, by = c("USUBJID", "visit_number")) %>% 
      left_join(sch_time) %>% 
      mutate(
        t = ifelse(is.na(t), onset + mea_time, t)
      ) %>% select(-mea_time),
    master_bind %>% 
      filter(Country != "Canada") %>% 
      left_join(sch_time, by = "visit_number") %>% 
      mutate(
        t = onset + mea_time
      ) %>% select(-mea_time)
  ) %>% 
    mutate(
      age = floor(age)
    ) %>% 
    select(-onset)
  
  master_bind <- master_bind %>% 
    mutate(
      is_baseline = Visit == "SV0",
      is_perfect = EUROQOL_UK == 1,
      study = "MASTER"
    ) %>% 
    select(country = Country, study, id = USUBJID, age, visit = visit_number, t,
           EqMo, EqSC, EqUA, EqPD, EqAD, is_baseline, is_perfect)

  master_bind
})


## Van Wijck data -----
eq5d$Wijck <- local({
  df <- read_sav(here::here("data", "raw", "Data_Wijck.sav")) %>% 
    select(id = PtNoT0d, age = Age, rash_onset = VsclT0d, starts_with("Eq"), matches("sntT\\d{1}p"))

  df <- df %>% 
    select(id, age, rash_onset, matches("sntT\\d{1}p")) %>% 
    pivot_longer(-c(id, age, rash_onset), names_pattern = "T(\\d{1})p", names_to = c("visit"), values_to = "date") %>% 
    left_join(
      df %>% 
        select(id, age, rash_onset, starts_with("Eq")) %>% 
        pivot_longer(-c(id, age, rash_onset), names_pattern = "(\\w+)T(\\d{1})p", names_to = c("name", "visit")) %>% 
        mutate(value = as.numeric(value)) %>% 
        pivot_wider(),
      by = c("id", "age", "rash_onset", "visit")
    ) %>% 
    mutate(
      visit = as.numeric(visit)
    )

  
  vw_method = "updated"

  if (vw_method == "original") {
    # According to original methodology, assign specific time to each visit
    vw_times = c(4, 18, 34, 94, 184, 274, 369)
    vw[, t := vw_times[visit]]
  } else {
    df1 <- df %>% 
      select(-EQ5D) %>% 
      rename(EqMo = EqM) %>% 
      group_by(id) %>% 
      arrange(id, visit) %>% 
      mutate(
        t0 = date[visit == 0],
        date1 = pmin(rash_onset, t0),
        date1 = ifelse(id == 102424, t0, date1), # Fix irregular rash onset date for one participant, first visit - onset date > yr 
        t = as.numeric(date - date1),
        visit = 1:n()
      ) %>% 
      ungroup()
    
    time_travellers <- df1 %>% 
      group_by(id) %>% 
      filter(!is.na(t)) %>% 
      summarise(sel = any(diff(t) <= 0)) %>% 
      filter(sel) %>% 
      pull(id)
    
    
    df2 <- df1 %>% 
      left_join(tibble(visit = 1:7, t_reg = c(4, 18, 34, 94, 184, 274, 369)), by = "visit") %>% 
      mutate(
        t = ifelse(id %in% time_travellers, t_reg, t),
        id = as.character(id),
        is_baseline = F,
        is_perfect = QALY_NL(EqMo, EqSC, EqUA, EqPD, EqAD) >= 1
      ) %>% 
      filter(!is.na(EqMo))
  }
  
  df_reg <- df2 %>% 
    mutate(country = "Netherlands", study = "Van Wijck et al. 2016") %>% 
    select(country, study, id, age, visit, t, EqMo, EqSC, EqUA, EqPD, EqAD, is_baseline, is_perfect)

  df_reg  
})


eq5d <- bind_rows(eq5d) %>% 
  mutate(
    study = case_when(
      country == "Canada" ~ "Drolet et al. 2010",
      startsWith(study, "MASTER") ~ paste("Rampakakis et al. 2017", country),
      T ~ study
    ),
    id = case_when(
      startsWith(study, "Drolet") ~ paste("M Canada", id),
      startsWith(study, "Van") ~ paste("Van Wijck", id),
      startsWith(study, "Scott") ~ paste("Scott", id),
      T ~ paste(country, id)
    )
  )


save(eq5d, file = here::here("data", "processed", "eq5d_all.rdata"))


eq5d %>% 
  summarise(
    N = length(unique(id)),
    N_record = n(),
    n_base = sum(is_baseline),
    p_base = sum(is_baseline * is_perfect, na.rm = T),
    t_min = min(t[!is.na(t) & is.finite(t)]),
    t_max = max(t[!is.na(t) & is.finite(t)])
  )


eq5d %>% 
  group_by(study) %>% 
  summarise(
    N = length(unique(id)),
    N_record = n(),
    n_base = sum(is_baseline),
    p_base = sum(is_baseline * is_perfect, na.rm = T),
    t_min = min(t[!is.na(t) & is.finite(t)]),
    t_max = max(t[!is.na(t) & is.finite(t)])
  )


# Differences in exclusions:
# Scott: We exclude Scott 13 on the basis of >14 days between rash onset and
# first time point. Scott 13 was not excluded in the previous study but should
# have been.
# Drolet: We exclude 125001 on the basis of only 3 timepoints; we exclude
# 156004, 161001, 201001, 210001 because of first recording > 14 days after 
# rash onset. Previous study excluded 104005, 109005, 118009, 159003, 170003, 
# 189007, 195003, 205007, 206004, but these seem fine and so we include them.
# Other MASTER: No differences
# Van Wijck: No differences


exc_id <- eq5d %>%
  filter(!(is.na(EqMo)|is.na(EqSC)|is.na(EqUA)|is.na(EqPD)|is.na(EqAD))) %>% 
  filter(!is_baseline) %>% 
  filter(!is.na(t)) %>% 
  group_by(study, id) %>% 
  arrange(id, t) %>% 
  mutate(
    visit = 1:n()
  ) %>% 
  summarise(
    n_vis = n(),
    is_late = t[visit == 1] > 14,
    is_early = t[visit == 1] < 0
  )

eq5d_is <- eq5d %>% 
  left_join(exc_id) %>% 
  mutate(
    is_complete = !(is.na(EqMo)|is.na(EqSC)|is.na(EqUA)|is.na(EqPD)|is.na(EqAD)),
    across(starts_with("is_"), \(x) ifelse(is.na(x), F, x))
  )


# Divert baseline and follow-up
eq5d_baseline <- eq5d_is %>% 
  filter(is_complete & is_baseline) %>% 
  arrange(study, id)


eq5d_baseline %>% 
  group_by(study, id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)


# Keep eligible records only
eq5d_fu <- eq5d_is %>% 
  filter(is_complete & !is_baseline) %>% 
  filter(!is.na(t))


eq5d_fu_eli <- eq5d_fu %>% 
  filter(!(is_late|is_early)) %>% 
  filter(n_vis >= 4) %>% 
  group_by(id) %>% 
  arrange(study, id, t) %>% 
  mutate(
    visit = 1:n()
  )


# Divert by value sets

eq5d_fu_uk <- eq5d_fu_eli %>% 
  mutate(
    EQ5D = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD),
    value_set = "UK"
  ) %>% 
  select(value_set, study, Patient.ID = id, age, visit, time_points = t, EQ5D)


eq5d_fu_orig <- eq5d_fu_eli %>% 
  mutate(
    EQ5D = ifelse(
      study == "Van Wijck et al. 2016",
      QALY_NL(EqMo, EqSC, EqUA, EqPD, EqAD),
      QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD)
    ),
    value_set = ifelse(study == "Van Wijck et al. 2016", "NL", "UK")
  ) %>% 
  select(value_set, study, Patient.ID = id, age, visit, time_points = t, EQ5D)


write_csv(eq5d_fu_uk, here::here("data", "processed", "eq5d_uk.csv"))
write_csv(eq5d_fu_orig, here::here("data", "processed", "eq5d_orig.csv"))


eq5d_baseline_uk <- eq5d_baseline %>% 
  mutate(
    EQ5D = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD),
    value_set = "UK"
  ) %>% 
  select(value_set, study, Patient.ID = id, age, EQ5D)


eq5d_baseline_orig <- eq5d_baseline %>% 
  mutate(
    EQ5D = ifelse(
      study == "Van Wijck et al. 2016",
      QALY_NL(EqMo, EqSC, EqUA, EqPD, EqAD),
      QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD)
    ),
    value_set = ifelse(study == "Van Wijck et al. 2016", "NL", "UK")
  ) %>% 
  select(value_set, study, Patient.ID = id, age, EQ5D)


write_csv(eq5d_baseline_uk, here::here("data", "processed", "eq5d_baseline_uk.csv"))
write_csv(eq5d_baseline_orig, here::here("data", "processed", "eq5d_baseline_orig.csv"))


