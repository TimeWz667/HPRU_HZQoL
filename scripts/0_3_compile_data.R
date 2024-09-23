library(tidyverse)


norm <- local({
  load(here::here("data", "sup_demo.rdata"))
  
  sup_demo %>% 
    ungroup() %>% 
    filter(Year == 2023) %>% 
    select(Age, Norm = norm_leoss)
})


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))

max_qol <- max(raw$EQ5D)
min_qol <- min(raw$EQ5D)


reformed <- raw %>% 
  mutate(ti = time_points / 365.25) %>% 
  select(SID = study, PID = Patient.ID, Age = age, ti = ti, EQ5D) %>% 
  left_join(norm) %>% 
  mutate(
    Agp = cut(Age, c(0, 30, seq(35, 90, 5), 100), right = F),
    HealthNorm = (EQ5D >= Norm) + 0,
    HealthPerfect = (EQ5D == 1) + 0,
    Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol),
    Norm_rescaled = (Norm - min_qol) / (max_qol - min_qol)
  )

reformed

write_csv(reformed, file = here::here("data", "qol_reformed.csv"))
save(reformed, file = here::here("data", "qol_reformed.rdata"))


## QoL with original value sets
raw <- read_csv(here::here("data", "eq5d_orig.csv"))

max_qol <- max(raw$EQ5D)
min_qol <- min(raw$EQ5D)
paste0("range[", min_qol, ",", max_qol, "]")

dat_qol <- raw %>% 
  mutate(ti = time_points / 365.25) %>% 
  select(SID = study, PID = Patient.ID, Age = age, ti = ti, EQ5D) %>% 
  left_join(norm) %>% 
  mutate(
    Agp = cut(Age, c(0, 30, seq(35, 90, 5), 100), right = F),
    HealthNorm = (EQ5D >= Norm) + 0,
    HealthPerfect = (EQ5D == 1) + 0,
    Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol),
    Norm_rescaled = (Norm - min_qol) / (max_qol - min_qol)
  )

dat_qol

write_csv(dat_qol, file = here::here("data", "qol_orig_set.csv"))
save(dat_qol, file = here::here("data", "qol_orig_set.rdata"))



## QoL with uk value sets
raw <- read_csv(here::here("data", "eq5d_uk.csv"))

max_qol <- max(raw$EQ5D)
min_qol <- min(raw$EQ5D)
paste0("range[", min_qol, ",", max_qol, "]")


dat_qol <- raw %>% 
  mutate(ti = time_points / 365.25) %>% 
  select(SID = study, PID = Patient.ID, Age = age, ti = ti, EQ5D) %>% 
  left_join(norm) %>% 
  mutate(
    Agp = cut(Age, c(0, 30, seq(35, 90, 5), 100), right = F),
    HealthNorm = (EQ5D >= Norm) + 0,
    HealthPerfect = (EQ5D == 1) + 0,
    Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol),
    Norm_rescaled = (Norm - min_qol) / (max_qol - min_qol)
  )

dat_qol

write_csv(dat_qol, file = here::here("data", "qol_uk_set.csv"))
save(dat_qol, file = here::here("data", "qol_uk_set.rdata"))

