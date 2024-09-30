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
QALY_NL = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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
QALY_LA = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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
QALY_JN = function(EqMo, EqSC, EqUA, EqPD, EqAD)
{
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
EQ5D3L = CJ(EqMo = 1:3, EqSC = 1:3, EqUA = 1:3, EqPD = 1:3, EqAD = 1:3)
EQ5D3L[, code := paste0(EqMo, EqSC, EqUA, EqPD, EqAD)]
TableUK = EQ5D3L[, structure(QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]
TableLA = EQ5D3L[, structure(QALY_LA(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]
TableJN = EQ5D3L[, structure(QALY_JN(EqMo, EqSC, EqUA, EqPD, EqAD), names = code)]

# Location of Data directory for QOL review
root = "~/OneDrive - London School of Hygiene and Tropical Medicine/Review of QOL after HZ onset/Data"
# root = "~/Documents/Data/HZQoL"

## Scott et al data
sc = fread(file.path(root, "HZ Judy Breuer data/Data.csv"))

# Exclude patients who withdrew consent
sc_exclude = sc[V2STATUS == 2 | V3STATUS == 2 | V4STATUS == 2, ID]
sc = sc[!ID %in% sc_exclude]

# Exclude patients marked as "not shingles"
sc_exclude = sc[V2STATUS == 4 | V3STATUS == 4 | V4STATUS == 4, ID]
sc = sc[!ID %in% sc_exclude]

# Failed to parse here is OK -- it means #NULL! in the original character date data.
sc[, RASHSTDA := lubridate::dmy(RASHSTDA)]
sc[, V1DATE := lubridate::dmy(V1DATE)]
sc[, V2DATE := lubridate::dmy(V2DATE)]
sc[, V3DATE := lubridate::dmy(V3DATE)]    # 1 failed to parse
sc[, VISDAT4 := lubridate::dmy(VISDAT4)]  # 2 failed to parse

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
sc_out[, visit := as.numeric(as.character(visit))] # unfactor


## MASTER data
ma = as.data.table(read_sas(file.path(root, "MASTER studies data/EQ5D - All Patients_14MAY2017.sas7bdat")))

# Note - the MASTER study appears to have calculated several QALY values for the
# LA dataset with several entries that are "off" by 0.74 or 0.79 from the
# values calculated above
# ma[, codeUK := paste(names(TableLA)[which(abs(TableUK - EUROQOL_UK ) < 0.0005)], collapse = ";"), by = 1:length(USUBJID)]
# ma[, codeLA := paste(names(TableLA)[which(abs(TableLA - EUROQOL_LA ) < 0.0005)], collapse = ";"), by = 1:length(USUBJID)]
# ma[, codeJN := paste(names(TableLA)[which(abs(TableJN - EUROQOL_JAN) < 0.0005)], collapse = ";"), by = 1:length(USUBJID)]

# Reverse engineer EQ5D code
ma[, code := names(TableUK)[
    which(abs(TableUK - EUROQOL_UK) < 0.0005 & abs(TableJN - EUROQOL_JAN) < 0.0005)
    ], by = 1:length(USUBJID)]

# Extract dimensions
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
ma = merge(ma, ma_age[, .(Country, USUBJID, age = Age_BL, onset = Duration_Rash_Onset)], by = c("Country", "USUBJID"), all.x = TRUE)

# Get timesteps for patients in Canadian data
mb = fread(file.path(root, "Marc Brisson/master.csv"))
canada_times = mb[, .SD, .SDcols = patterns("unique|days_since_onset_")]
names(canada_times) = c("USUBJID", paste0("t", 1:10))
canada_times = melt(canada_times, id.vars = "USUBJID", measure.vars = list(t = paste0("t", 1:10)))
canada_times[, USUBJID := as.character(USUBJID)]
canada_times[, variable := as.numeric(stringr::str_remove_all(variable, "t"))]

# Add back to MASTER data
mac = ma[Country == "Canada"]
mac = merge(mac, canada_times, by.x = c("USUBJID", "visit_number"), by.y = c("USUBJID", "variable"), all.x = TRUE)
ma = rbind(ma[Country != "Canada"], mac, use.names = TRUE, fill = TRUE)

# Imputation of typical delays between visits
canada_times[, visit := 1:.N, by = USUBJID]
canada_times = merge(canada_times, canada_times[visit == 1, .(USUBJID, onset = t)], by = "USUBJID")
canada_times[onset >= 0 & onset <= 14, median(t - onset, na.rm = TRUE), by = visit]

ma_times = c(-Inf, 0, 7, 14, 21, 30, 60, 90, 120, 150, 181)
ma[is.na(t), t := onset + ma_times[visit_number + 1]]

# Floor age
ma[, age := floor(age)]

# Remember IDs of individuals in perfect health at start of study
ma_perfect = ma[Visit == "SV0" & EUROQOL_UK == 1, .(Country, USUBJID)]

# Output in common format
ma_out = ma[, .(country = Country, study = "MASTER", id = USUBJID, age = age,
    visit = visit_number, t = t, EqMo, EqSC, EqUA, EqPD, EqAD)]


## Van Wijck data
vw = as.data.table(read_sav(file.path(root, "Van Wijck/Doctor + PatientzonderDOB.sav")))

# Columns to keep
keep = c(
    "PtNoT0d",   # Patient ID
    "Age",       # Patient age
    # "sntT0d",    # Date of submission T0 doctor
    # "IncT0d",    # Inclusion date T0 doctor
    # "DgnHZT0d",  # Date of Herpes Zoster diagnosis T0 doctor
    "VsclT0d",   # Date of first vesicles T0 doctor
    names(vw)[names(vw) %like% "^Eq"],           # EQ5D measures
    names(vw)[names(vw) %like% "^sntT[0-6]p$"]   # Date of patient submissions
)
vw = vw[, .SD, .SDcols = keep]

# Reformat
vw = melt(vw, id.vars = c("PtNoT0d", "Age", "VsclT0d"),
    measure.vars = list(
        EqMo = c( "EqMT0p",  "EqMT1p",  "EqMT2p",  "EqMT3p",  "EqMT4p",  "EqMT5p",  "EqMT6p"),
        EqSC = c("EqSCT0p", "EqSCT1p", "EqSCT2p", "EqSCT3p", "EqSCT4p", "EqSCT5p", "EqSCT6p"),
        EqUA = c("EqUAT0p", "EqUAT1p", "EqUAT2p", "EqUAT3p", "EqUAT4p", "EqUAT5p", "EqUAT6p"),
        EqPD = c("EqPDT0p", "EqPDT1p", "EqPDT2p", "EqPDT3p", "EqPDT4p", "EqPDT5p", "EqPDT6p"),
        EqAD = c("EqADT0p", "EqADT1p", "EqADT2p", "EqADT3p", "EqADT4p", "EqADT5p", "EqADT6p"),
        date = c("sntT0p", "sntT1p", "sntT2p", "sntT3p", "sntT4p", "sntT5p", "sntT6p")
    )
)
names(vw)[1:4] = c("id", "age", "rash_onset", "visit")

vw_method = "updated"
vw[, visit := as.numeric(as.character(visit))] # unfactor

if (vw_method == "original") {
    # According to original methodology, assign specific time to each visit
    vw_times = c(4, 18, 34, 94, 184, 274, 369)
    vw[, t := vw_times[visit]]
} else {
    # Make onset/reference date whichever is earlier, onset of rash as recorded, or first visit date
    vw = merge(vw, vw[visit == 1, .(id, date_visit1 = date)], by = "id", all = TRUE)
    vw[, date1 := pmin(rash_onset, date_visit1)]
    
    # Calculate t
    vw[, t := as.numeric(date - date1)]
    
    # Fix irregular rash onset date for one participant
    vw[id == 102424, t := as.numeric(date - date_visit1)]
    
    # Remove NA visits and renumber remaining visits
    vw = vw[!is.na(EqMo)]
    vw[, visit := 1:.N, by = id]

    # For irregular entries, assign dates from protocol
    vw_times = c(4, 18, 34, 94, 184, 274, 369)
    time_travellers = vw[!is.na(t), any(diff(t) <= 0), by = id][V1 == TRUE, id]
    vw[id %in% time_travellers, t := vw_times[visit]]
}

# Convert to numeric (remove haven labels)
vw$EqMo = as.numeric(vw$EqMo)
vw$EqSC = as.numeric(vw$EqSC)
vw$EqUA = as.numeric(vw$EqUA)
vw$EqPD = as.numeric(vw$EqPD)
vw$EqAD = as.numeric(vw$EqAD)

vw_out = vw[!is.na(EqMo), 
    .(country = "Netherlands", study = "Van Wijck et al. 2016", id, age, visit, t, 
    EqMo, EqSC, EqUA, EqPD, EqAD)]


## Create output data
dat = rbind(sc_out, ma_out, vw_out)

dat[study %like% "Scott", id := paste("Scott", id)]
dat[country == "Canada", study := "Drolet et al. 2010"]
dat[study %like% "Drolet", id := paste("M Canada", id)]
dat[study %like% "MASTER", study := paste("Rampakakis et al. 2017", country)]
dat[study %like% "Rampakakis", id := paste(country, as.numeric(id))]
dat[study %like% "Van", id := paste("Van Wijck", id)]
setkey(dat, "id")

# Keep only complete cases
dat = dat[complete.cases(dat)]

# Remove any baseline measurements
dat = dat[visit > 0]

# Renumber visits
dat[, visit := 1:.N, by = id]

# Select patients with at least 4 time points
exclude_points = dat[, .(zap = .N < 4), by = id][zap == TRUE, id]
dat = dat[!id %in% exclude_points]

# Exclude if first visit more than 14 days after rash onset
exclude_late = dat[visit == 1 & t > 14, id]
dat = dat[!id %in% exclude_late]

# Exclude if first visit is before rash onset
exclude_early = dat[visit == 1 & t < 0, id]
dat = dat[!id %in% exclude_early]

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

# Reorder to be same order as original data...
dat = dat[order(study, id)]

fwrite(dat, "./data/eq5d_components.csv")

## Produce outputs

dat_uk = dat[, .(time_points = t, age, study, EQ5D = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD), Patient.ID = id, value_set = "UK")]
fwrite(dat_uk, "data/eq5d_uk.csv")
dat_orig = dat[, .(time_points = t, age, study, 
    EQ5D = ifelse(study == "Van Wijck et al. 2016", 
        QALY_NL(EqMo, EqSC, EqUA, EqPD, EqAD),
        QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD)), Patient.ID = id, 
    value_set = ifelse(study == "Van Wijck et al. 2016", "NL", "UK"))]
fwrite(dat_orig, "data/eq5d_orig.csv")

dat_perfect = dat
dat_perfect[, USUBJID := sprintf("%06s", stringr::str_remove_all(id, "[A-Za-z ]*"))]
dat_perfect = merge(dat_perfect, ma_perfect, by.x = c("country", "USUBJID"), by.y = c("Country", "USUBJID"))
dat_perf_uk = dat_perfect[, .(time_points = t, age, study, EQ5D = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD), Patient.ID = id, value_set = "UK")]
fwrite(dat_perf_uk, "data/eq5d_perf_uk.csv")
