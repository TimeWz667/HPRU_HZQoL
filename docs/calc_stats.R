library(targets)
library(tidyverse)


dat <- tar_read(data_raw, 2)


nrow(dat)
dat


dat %>% 
  summarise(
    n = n(),
    n_subjects = length(unique(PID)),
    t_max = length(unique(PID[ti > 1])),
    
  )


tar_objects()


tar_read(data_tte) %>% 
  summarise(
    Recovered = sum(Recovered),
    Cen = 1 - Recovered / n()
  )


tar_read(pars_shortfall, 2)
tar_read(pars_qol, 2)[[1]] %>% 
  group_by(Cluster) %>% 
  summarise(
    n = n(),
    mean = mean(EQ5D),
    lo = quantile(EQ5D, 0.025),
    hi = quantile(EQ5D, 0.975)
  ) %>% 
  ungroup() %>% 
  mutate(
    prop = n / sum(n)
  )


tar_read(pars_qol, 2)[[1]] %>% 
  summarise(
    n = length(unique(PID)),
    n_severe = length(unique(PID[Cluster == 2])),
  )



tar_read(data_raw, 2) %>% 
  left_join(tar_read(pars_qol, 2)[[1]]) %>% 
  filter(!is.na(Cluster)) %>% 
  summarise(
    n = length(unique(PID)),
    n_null = length(unique(PID[Cluster == 0])),
    n_severe = length(unique(PID[(Cluster == 1) & (ti > (90 / 365.25))])),
  )



tar_read(pars_tte)$Ext %>%
  mutate(
    rd = runif(n()),
    rd = -log(1 - runif(n()))
  ) %>% 
  crossing(Age = c(50, 60, 70, 80)) %>% 
  mutate(
    rate = (r0 * exp(Age * ba1)),
    T_Sim = rd / rate * 365.25
  ) %>% 
  group_by(Age) %>% 
  summarise(
    mean = mean(T_Sim),
    lo = quantile(T_Sim, 0.025),
    hi = quantile(T_Sim, 0.975)
  )


tar_read(sim_shortfall, 2) %>% 
  filter(Age %in% c(50, 90)) %>% 
  group_by(Age) %>% 
  summarise(
    across(c(QLH35, QL35), list(
      mean = mean,
      lo = \(x) quantile(x, 0.025),
      hi = \(x) quantile(x, 0.975)
    ))
  )








