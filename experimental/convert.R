library(haven)
library(data.table)
library(ggplot2)

# of interest... diff scales?
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")
data[, range(EQ5D), by = study]

# scott is breuer, drolet / rampakakis are MASTER

# Load Van Wijck data
path = "~/Documents/Data/HZQoL/Doctor + PatientzonderDOB.sav"
dat = as.data.table(read_sav(path))

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
QALY_UK = function(EqM, EqSC, EqUA, EqPD, EqAD)
{
    deduct = 
        ifelse(EqM >= 2 | EqSC >= 2 | EqUA >= 2 | EqPD >= 2 | EqAD >= 2, 0.081, 0) +
        ifelse(EqM == 3 | EqSC == 3 | EqUA == 3 | EqPD == 3 | EqAD == 3, 0.269, 0) +
        ifelse(EqM  == 2, 0.069, 0) +
        ifelse(EqM  == 3, 0.314, 0) +
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

dat[, Q0 := QALY_UK(EqMT0p, EqSCT0p, EqUAT0p, EqPDT0p, EqADT0p)]
dat[, Q1 := QALY_UK(EqMT1p, EqSCT1p, EqUAT1p, EqPDT1p, EqADT1p)]
dat[, Q2 := QALY_UK(EqMT2p, EqSCT2p, EqUAT2p, EqPDT2p, EqADT2p)]
dat[, Q3 := QALY_UK(EqMT3p, EqSCT3p, EqUAT3p, EqPDT3p, EqADT3p)]
dat[, Q4 := QALY_UK(EqMT4p, EqSCT4p, EqUAT4p, EqPDT4p, EqADT4p)]
dat[, Q5 := QALY_UK(EqMT5p, EqSCT5p, EqUAT5p, EqPDT5p, EqADT5p)]
dat[, Q6 := QALY_UK(EqMT6p, EqSCT6p, EqUAT6p, EqPDT6p, EqADT6p)]

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
