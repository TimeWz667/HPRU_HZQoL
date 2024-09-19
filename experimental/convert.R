library(haven)
library(data.table)
library(ggplot2)

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
QALY_UK = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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
QALY_NL = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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
QALY_LA = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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

# Create tables of EQ5D3 codes and index values
TableUK = numeric(0)
TableLA = numeric(0)
for (a in 1:3) {
  for (b in 1:3) {
    for (c in 1:3) {
      for (d in 1:3) {
        for (e in 1:3) {
          nm = paste0(a, b, c, d, e)
          TableUK[nm] = QALY_UK(a, b, c, d, e)
          TableLA[nm] = QALY_LA(a, b, c, d, e)
        }
      }
    }
  }
}

# Location of Data directory for QOL review
root = "~/OneDrive - London School of Hygiene and Tropical Medicine/Review of QOL after HZ onset/Data"

## Scott et al data
sc = fread(file.path(root, "HZ Judy Breuer data/Data.csv"))

# Failed to parse here is OK -- it means #NULL! in the original character date data.
sc[, RASHSTDA := lubridate::dmy(RASHSTDA)]
sc[, V1DATE := lubridate::dmy(V1DATE)]
sc[, V2DATE := lubridate::dmy(V2DATE)]    # 1 failed to parse
sc[, V3DATE := lubridate::dmy(V3DATE)]    # 4 failed to parse
sc[, VISDAT4 := lubridate::dmy(VISDAT4)]  # 8 failed to parse

# Time points
sc[, t_1 := as.numeric(V1DATE - RASHSTDA)]
sc[, t_2 := as.numeric(V2DATE - RASHSTDA)]
sc[, t_3 := as.numeric(V3DATE - RASHSTDA)]
sc[, t_4 := as.numeric(VISDAT4 - RASHSTDA)]

# Output in common format
sc_out = sc[, .(
    country = "United Kingdom",
    study = "Scott et al. 2006",
    id = ID, 
    age = AGEV1,
    t_1 = as.numeric(V1DATE - RASHSTDA),
    t_2 = as.numeric(V2DATE - RASHSTDA),
    t_3 = as.numeric(V3DATE - RASHSTDA),
    t_4 = as.numeric(VISDAT4 - RASHSTDA),
    EqMo_1 = EQ_MOBIL, EqSC_1 = EQ_SELFC, EqUA_1 = EQ_USACT, EqPD_1 = EQ_PAIND, EqAD_1 = EQ_ANXD,
    EqMo_2 = EQ_MOBIL, EqSC_2 = EQSELFC2, EqUA_2 = EQUSACT2, EqPD_2 = EQPAIND2, EqAD_2 = EQANXD2,
    EqMo_3 = EQ_MOBIL, EqSC_3 = EQSELFC3, EqUA_3 = EQUSACT3, EqPD_3 = EQPAIND3, EqAD_3 = EQANXD3,
    EqMo_4 = EQ_MOBIL, EqSC_4 = EQSELFC4, EqUA_4 = EQUSACT4, EqPD_4 = EQPAIND4, EqAD_4 = EQANXD4)]
sc_out = melt(
    sc_out, id.vars = c("country", "study", "id", "age"), 
        measure.vars = list(
            t    = c(   "t_1",    "t_2",    "t_3",    "t_4"),
            EqMo = c("EqMo_1", "EqMo_2", "EqMo_3", "EqMo_4"),
            EqSC = c("EqSC_1", "EqSC_2", "EqSC_3", "EqSC_4"),
            EqUA = c("EqUA_1", "EqUA_2", "EqUA_3", "EqUA_4"),
            EqPD = c("EqPD_1", "EqPD_2", "EqPD_3", "EqPD_4"),
            EqAD = c("EqAD_1", "EqAD_2", "EqAD_3", "EqAD_4")
        ), variable.name = "visit")


## MASTER data
ma = as.data.table(read_sas(file.path(root, "MASTER studies data/EQ5D - All Patients_14MAY2017.sas7bdat")))

# Reverse engineer EQ5D dimensions
ma[, code := names(TableLA)[
    which(abs(TableUK - EUROQOL_UK) < 0.0005 & abs(TableLA - EUROQOL_LA) < 0.0005)
    ], by = 1:length(USUBJID)]
ma[, EqMo := as.numeric(substr(code, 1, 1))]
ma[, EqSC := as.numeric(substr(code, 2, 2))]
ma[, EqUA := as.numeric(substr(code, 3, 3))]
ma[, EqPD := as.numeric(substr(code, 4, 4))]
ma[, EqAD := as.numeric(substr(code, 5, 5))]

# Extract visit number (0 is baseline)
ma[, visit_number := as.numeric(stringr::str_remove_all(Visit, "[A-Z]"))]
ma = ma[order(Country, USUBJID, visit_number)]

# Add age
ma_age = fread(file.path(root, "MASTER studies data/Copy of Baseline Characteristics - All Patients_14MAY2017.csv"),
    colClasses = c(USUBJID = "character"))
ma = merge(ma, ma_age[, .(Country, USUBJID, age = Age_BL)], by = c("Country", "USUBJID"), all.x = TRUE)

# Need to add time of visit; next plot is odd
plot(data[study %like% "Rampakakis", sort(unique(time_points))])
# TODO left off here
# I think what has happened is that duration since rash onset was used as start
# point, then adding increments to that. Check tomorrow.
# Yup.






ma_out = ma[,
  .(country = Country, study = "MASTER", id = USUBJID, age = NA,
  visit = visit_number, t = NA, EqMo, EqSC, EqUA, EqPD, EqAD)]


mb = as.data.table(read_sas(file.path(root, "Marc Brisson/master.sas7bdat")))
mb[, EQ5D_1  := QALY_UK(mobile_1 , care_1 , usual_1 , discomf_1 , anxiety_1 )]
mb[, EQ5D_2  := QALY_UK(mobile_2 , care_2 , usual_2 , discomf_2 , anxiety_2 )]
mb[, EQ5D_3  := QALY_UK(mobile_3 , care_3 , usual_3 , discomf_3 , anxiety_3 )]
mb[, EQ5D_4  := QALY_UK(mobile_4 , care_4 , usual_4 , discomf_4 , anxiety_4 )]
mb[, EQ5D_5  := QALY_UK(mobile_5 , care_5 , usual_5 , discomf_5 , anxiety_5 )]
mb[, EQ5D_6  := QALY_UK(mobile_6 , care_6 , usual_6 , discomf_6 , anxiety_6 )]
mb[, EQ5D_7  := QALY_UK(mobile_7 , care_7 , usual_7 , discomf_7 , anxiety_7 )]
mb[, EQ5D_8  := QALY_UK(mobile_8 , care_8 , usual_8 , discomf_8 , anxiety_8 )]
mb[, EQ5D_9  := QALY_UK(mobile_9 , care_9 , usual_9 , discomf_9 , anxiety_9 )]
mb[, EQ5D_10 := QALY_UK(mobile_10, care_10, usual_10, discomf_10, anxiety_10)]

mb[, .(unique, EQ5D_1, EQ5D_2, EQ5D_3, EQ5D_4, EQ5D_5, EQ5D_6, EQ5D_7, EQ5D_8, EQ5D_9, EQ5D_10)]
ma1[Country == "Canada" & USUBJID == "101001"]
# for mb, 7006 columns. can cut view_v[1 to 10]_[number], 
# side_v[1 to 10]_[number], body_part_v[1 to 10]_[number], 
# x_v[1 to 10]_[number] 
# jackpot is here:
# mobile_x care_x usual_x discomf_x anxiety_x, where x is visit number 1 to 10

ma1 = as.data.table(read_sas(file.path(root, "MASTER studies data/Active patients only/Age - Duration from Rash Onset_PHE 27MAR2017.sas7bdat")))
ma1 = as.data.table(read_sas(file.path(root, "MASTER studies data/Active patients only/EQ-5D_PHE 27MAR2017.sas7bdat")))
ma1 = as.data.table(read_sas(file.path(root, "MASTER studies data/Baseline Characteristics - All Patients_14MAY2017.sas7bdat")))

# Van Wijck data
dat = as.data.table(read_sav(file.path(root, "Van Wijck/Doctor + PatientzonderDOB.sav")))

# Columns to keep
keep = c(
    "PtNoT0d",   # Patient ID
    "Age",       # Patient age
    # "sntT0d",    # Date of submission T0 doctor
    # "IncT0d",    # Inclusion date T0 doctor
    # "DgnHZT0d",  # Date of Herpes Zoster diagnosis T0 doctor
    "VsclT0d",   # Date of first vesicles T0 doctor
    names(dat)[names(dat) %like% "^Eq"],           # EQ5D measures
    names(dat)[names(dat) %like% "^sntT[0-6]p$"]   # Date of patient submissions
)
dat = dat[, .SD, .SDcols = keep]

# Note. Original methodology used defined days -- 
# i.e. 4, 18, 34, 94, 184, 274, 369.

dat[, Q0 := QALY_NL(EqMT0p, EqSCT0p, EqUAT0p, EqPDT0p, EqADT0p)]
dat[, Q1 := QALY_NL(EqMT1p, EqSCT1p, EqUAT1p, EqPDT1p, EqADT1p)]
dat[, Q2 := QALY_NL(EqMT2p, EqSCT2p, EqUAT2p, EqPDT2p, EqADT2p)]
dat[, Q3 := QALY_NL(EqMT3p, EqSCT3p, EqUAT3p, EqPDT3p, EqADT3p)]
dat[, Q4 := QALY_NL(EqMT4p, EqSCT4p, EqUAT4p, EqPDT4p, EqADT4p)]
dat[, Q5 := QALY_NL(EqMT5p, EqSCT5p, EqUAT5p, EqPDT5p, EqADT5p)]
dat[, Q6 := QALY_NL(EqMT6p, EqSCT6p, EqUAT6p, EqPDT6p, EqADT6p)]

dat2 = melt(dat[, .(Patient.ID = PtNoT0d, Age = Age, Q0, Q1, Q2, Q3, Q4, Q5, Q6)],
    id.vars = c("Patient.ID", "Age"))
dat2 = dat2[order(Patient.ID, variable)]

# Load transformed data
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")
data = data[order(study, Patient.ID, time_points)]
data2 = data[study %like% "Van Wijck"]
data2[time_points == 4, variable := "Q0"]
data2[time_points == 18, variable := "Q1"]
data2[time_points == 34, variable := "Q2"]
data2[time_points == 94, variable := "Q3"]
data2[time_points == 184, variable := "Q4"]
data2[time_points == 274, variable := "Q5"]
data2[time_points == 369, variable := "Q6"]
data2[, Patient.ID := as.numeric(stringr::str_remove(Patient.ID, "Van Wijck "))]

# Merge data
dm = merge(data2, dat2, by = c("Patient.ID", "variable"), all = TRUE)

ggplot(dm) +
    geom_point(aes(x = EQ5D, y = value)) +
    geom_abline(yintercept = 0, slope = 1)

dat2
data2
keep

0126
4, 18, 34, 369

dat[1, sntT0p - VsclT0d]
dat[1, sntT1p - VsclT0d]
dat[1, sntT2p - VsclT0d]
dat[1, sntT6p - VsclT0d]


dat[1, sntT0p - sntT0d]
dat[1, sntT1p - sntT0d]
dat[1, sntT2p - sntT0d]
dat[1, sntT6p - sntT0d]

dat[1, sntT0p - IncT0d]
dat[1, sntT1p - IncT0d]
dat[1, sntT2p - IncT0d]
dat[1, sntT6p - IncT0d]

dat[1, sntT0p - DgnHZT0d]
dat[1, sntT1p - DgnHZT0d]
dat[1, sntT2p - DgnHZT0d]
dat[1, sntT6p - DgnHZT0d]





dat[3, sntT0p - VsclT0d]
dat[3, sntT1p - VsclT0d]
dat[3, sntT2p - VsclT0d]
dat[3, sntT3p - VsclT0d]
dat[3, sntT4p - VsclT0d]
dat[3, sntT5p - VsclT0d]
dat[3, sntT6p - VsclT0d]

dat[3, sntT0p - sntT0d]
dat[3, sntT1p - sntT0d]
dat[3, sntT2p - sntT0d]
dat[3, sntT6p - sntT0d]

dat[3, sntT0p - IncT0d]
dat[3, sntT1p - IncT0d]
dat[3, sntT2p - IncT0d]
dat[3, sntT6p - IncT0d]

dat[3, sntT0p - DgnHZT0d]
dat[3, sntT1p - DgnHZT0d]
dat[3, sntT2p - DgnHZT0d]
dat[3, sntT6p - DgnHZT0d]
