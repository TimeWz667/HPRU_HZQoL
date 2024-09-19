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
TableUK = numeric(0)
TableLA = numeric(0)
TableJN = numeric(0)
for (a in 1:3) {
  for (b in 1:3) {
    for (c in 1:3) {
      for (d in 1:3) {
        for (e in 1:3) {
          nm = paste0(a, b, c, d, e)
          TableUK[nm] = QALY_UK(a, b, c, d, e)
          TableLA[nm] = QALY_LA(a, b, c, d, e)
          TableJN[nm] = QALY_JN(a, b, c, d, e)
        }
      }
    }
  }
}

# Location of Data directory for QOL review
# root = "~/OneDrive - London School of Hygiene and Tropical Medicine/Review of QOL after HZ onset/Data"
root = "~/Documents/Data/HZQoL"

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

##### TODO check / rationalize the below.

# Imputation of mean delays between visits
keepers = canada_times[variable == 1 & t <= 14, USUBJID]
mac[USUBJID %in% keepers, median(t-onset, na.rm = TRUE), by = visit_number]

ma_times = c(-Inf, 0, 7, 14, 21, 30, 60, 90, 120, 150, 180)
ma[is.na(t), t := onset + ma_times[visit_number + 1]]

#####

# Floor age
ma[, age := floor(age)]

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

# According to original methodology, assign specific time to each visit
vw_times = c(4, 18, 34, 94, 184, 274, 369)
vw[, t := vw_times[visit]]

# Convert to numeric (remove haven labels)
vw$EqMo = as.numeric(vw$EqMo)
vw$EqSC = as.numeric(vw$EqSC)
vw$EqUA = as.numeric(vw$EqUA)
vw$EqPD = as.numeric(vw$EqPD)
vw$EqAD = as.numeric(vw$EqAD)

vw_out = vw[!is.na(EqMo), 
    .(country = "Netherlands", study = "Van Wijck et al. 2016", id, age, visit, t, 
    EqMo, EqSC, EqUA, EqPD, EqAD)]

vw_out[, visit := as.numeric(as.character(visit))] # unfactor


## Create output data
dat = rbind(sc_out, ma_out, vw_out)

dat[study %like% "Scott", id := paste("Scott", id)]
dat[country == "Canada", study := "Drolet et al. 2010"]
dat[study %like% "Drolet", id := paste("M Canada", id)]
dat[study %like% "MASTER", study := paste("Rampakakis et al. 2017", country)]
dat[study %like% "Rampakakis", id := paste(country, as.numeric(id))]
dat[study %like% "Van", id := paste("Van Wijck", id)]

#### TODO save here I suppose

# Keep only complete cases
dat = dat[complete.cases(dat)]

# Remove any baseline measurements
dat = dat[visit > 0]

# Select patients with at least 4 time points
exclude_points = dat[, .(zap = .N < 4), by = id][zap == TRUE, id]
dat = dat[!id %in% exclude_points]

# Exclude if first visit more than 14 days after rash onset
exclude_late = dat[visit == 1 & t > 14, id]
dat = dat[!id %in% exclude_late]



dat_format = dat[, .(time_points = t, age, study, EQ5D = QALY_UK(EqMo, EqSC, EqUA, EqPD, EqAD), Patient.ID = id)]
fwrite(dat_format, "data/test.csv")

test = fread("./data/test.csv")
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")
setdiff(test[, unique(Patient.ID)], data[, unique(Patient.ID)])
setdiff(data[, unique(Patient.ID)], test[, unique(Patient.ID)])
data[study %like% "Ramp"]

sc_out

##### TODO revisit this

# Make onset/reference date whichever is earlier, onset of rash as recorded, or first visit date
vw = merge(vw, vw[visit == 1, .(id, date_visit1 = date)], by = "id", all = TRUE)
vw[, date1 := pmin(rash_onset, date_visit1)]

# Fix irregular rash onset date for one participant
vw[id == 102424, t := as.numeric(date - date_visit1)]

# Remove NA visits and renumber remaining visits
vw = vw[!is.na(EqMo)]
vw[, visit := 1:.N, by = id]

ggplot(vw) +
    geom_point(aes(x = visit, y = t))

time_travellers = vw[!is.na(t), any(diff(t) < 0), by = id][V1 == TRUE, id]
time_travellers = vw[!is.na(t), any(diff(t) == 0), by = id][V1 == TRUE, id]

ggplot(vw[id %in% time_travellers]) +
    geom_line(aes(x = visit, y = t, group = id))

vw[id %in% time_travellers]

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

########











not_in_mb = setdiff(ma[Country == "Canada", unique(USUBJID)], mb[, unique(unique)])
# there are none other way round: setdiff(mb[, unique(unique)], ma[Country == "Canada", unique(USUBJID)])
View(ma[USUBJID %in% not_in_mb])
# ok so, can't just use the marc brisson data set
canadians = ma[Country == "Canada"]

# steps: first, check marc brisson data set is consistent with the master data set
# second, get actual dates of surveys from marc brisson data set
# third, apply imputation to remaining canadians not in marc brisson

# cas_incident (only 0 or 1)
mb[, table(cas_incident, useNA = "ifany")]
# zhz_onset = onset date;
# zi_date_1, days_since_onset_1, etc to 10

# Need to add time of visit; next plot is odd
plot(data[study %like% "Rampakakis", sort(unique(time_points))])
# TODO left off here
# I think what has happened is that duration since rash onset was used as start
# point, then adding increments to that. Check tomorrow.
# Yup.

ma_out = ma[,
  .(country = Country, study = "MASTER", id = USUBJID, age = NA,
  visit = visit_number, t = NA, EqMo, EqSC, EqUA, EqPD, EqAD)]


mb = fread(file.path(root, "Marc Brisson/master.csv"))
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
