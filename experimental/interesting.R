library(haven)
library(data.table)
library(ggplot2)

## note https://eprints.whiterose.ac.uk/124872/8/st0528.pdf?a


# Some interesting observations.


# In the data set we have been using with EQ5D, although the Van Wijck data use
# the Netherlands weights, all of the Rampakakis / MASTER studies (including 
# Drolet) use the UK EQ5D weights:

# root = "~/OneDrive - London School of Hygiene and Tropical Medicine/Review of QOL after HZ onset/Data"
root = "~/Documents/Data/HZQoL"

ma1 = as.data.table(read_sas(file.path(root, "MASTER studies data/EQ5D - All Patients_14MAY2017.sas7bdat")))

data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")
data = data[order(study, Patient.ID, time_points)]

data[study %like% "Drolet"] # uses UK
ma1[USUBJID == "101005"]

data[study %like% "Argentina"] # uses UK
ma1[USUBJID == "001001" & Country == "Argentina"]

data[study %like% "Costa Rica"] # uses UK
ma1[USUBJID == "106005" & Country == "Costa Rica"]

data[study %like% "Korea"] # uses UK
ma1[USUBJID == "001001" & Country == "Korea"]

data[study %like% "Mexico"] # uses UK
ma1[USUBJID == "001101" & Country == "Mexico"]

data[study %like% "Taiwan"] # uses UK
ma1[USUBJID == "001001" & Country == "Taiwan"]

data[study %like% "Thailand"] # uses UK
ma1[USUBJID == "100001" & Country == "Thailand"]

# Explained thus in the manuscript: Individual scores for each of the 5 
# dimensions of the EQ-5D were valued using national or regional value sets to 
# estimate EQ-5D index scores. The EQ-5D index scores from Rampakakis et al.[21]
# and Van Wijck et al.[2] were valued by the authors and were not recalculated 
# for the purpose of this analysis. Van Wijck et al.[2] used the Dutch value 
# set. The MASTER patient cohorts in Rampakakis et al.[21] were valued using 
# the UK, Japanese and Latin America value sets. The scores valued with the UK 
# value set were selected for analysis as this was the most validated value 
# set[21, 24]. Scott et al.[19] and Drolet et al.[24] provided raw EQ-5D scores 
# for each of the five dimensions. These were matched to the EuroQol EQ-5D-3L 
# time trade off (TTO) value-set for the UK.

# [Note it's not for Latin America, but for Spanish-speaking Hispanics in the US]




# Relationship between JAN/LA/UK QALY judgements
ma = as.data.table(read_sas(file.path(root, "MASTER studies data/EQ5D - All Patients_14MAY2017.sas7bdat")))

ggplot(ma) + 
  geom_point(aes(x = EUROQOL_UK, y = EUROQOL_JAN)) + 
  geom_abline(yintercept = 0, slope = 1)

ggplot(ma) + 
  geom_point(aes(x = EUROQOL_UK, y = EUROQOL_LA)) + 
  geom_abline(yintercept = 0, slope = 1)
