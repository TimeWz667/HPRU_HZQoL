library(tidyverse)


dat <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))




## Data description

dat %>% 
  select(study, Patient.ID) %>% 
  group_by(study, Patient.ID) %>% 
  summarise(n = n()) %>% 
  group_by(study) %>% 
  summarise(
    lo = min(n),
    up = max(n),
    avg = mean(n)
  ) %>% 
  left_join(
    dat %>% 
      group_by(study) %>% 
      summarise(
        n_data = n(), 
        n_subject = length(unique(Patient.ID))
      )
  ) %>% 
  bind_rows(
    dat %>% 
      select(Patient.ID) %>% 
      group_by(Patient.ID) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      summarise(
        lo = min(n),
        up = max(n),
        avg = mean(n),
        n_data = sum(n), 
        n_subject = length(unique(Patient.ID))
      ) %>% 
      mutate(
        study = "All"
      )
  )


dat %>% 
  mutate(
    Agp = cut(age, c(0, 50, 60, 70, 80, 100), right = F)
  ) %>% 
  select(Agp, Patient.ID) %>% 
  distinct() %>% 
  group_by(Agp) %>% 
  summarise(n = n())


hist(dat$EQ5D)


dat %>% 
  filter(EQ5D < 1) %>% 
  filter(EQ5D > min(EQ5D)) %>% 
  pull(EQ5D) %>% hist()


dat %>% 
  summarise(
    n = sum(EQ5D == 1),
    mean(EQ5D == 1)
  )


tab_zeros <- dat %>% 
  mutate(
    Agp = cut(age, c(0, 50, 60, 70, 80, 100), right = F)
  ) %>% 
  group_by(Patient.ID, Agp) %>% 
  summarise(
    Z15 = any((EQ5D == 1) & (time_points < 15)),
    Z30 = any((EQ5D == 1) & (time_points < 30)),
    Z45 = any((EQ5D == 1) & (time_points < 45)),
    Z60 = any((EQ5D == 1) & (time_points < 60)),
    Z90 = any((EQ5D == 1) & (time_points < 90))
  ) %>% 
  group_by(Agp) %>% 
  summarise(
    n = n(),
    across(starts_with("Z"), mean)
  ) %>% 
  bind_rows(
    dat %>% 
      group_by(Patient.ID) %>% 
      summarise(
        Z15 = any((EQ5D == 1) & (time_points < 15)),
        Z30 = any((EQ5D == 1) & (time_points < 30)),
        Z45 = any((EQ5D == 1) & (time_points < 45)),
        Z60 = any((EQ5D == 1) & (time_points < 60)),
        Z90 = any((EQ5D == 1) & (time_points < 90))
      ) %>% 
      summarise(
        n = n(),
        across(starts_with("Z"), mean)
      ) %>% 
      mutate(Agp = "All")
  )

