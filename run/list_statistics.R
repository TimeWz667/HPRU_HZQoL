library(tidyverse)
library(xtable)


pars <- tar_read(pars_shortfall, 1)

pars %>% 
  pivot_longer(-Key) %>% 
  group_by(name) %>% 
  summarise(
    m = mean(value),
    l = quantile(value, 0.25),
    u = quantile(value, 0.75)
  )





tabs <- bind_rows(
  read_csv(here::here("docs", "tabs", "fit_time2zero.csv")),
  read_csv(here::here("docs", "tabs", "stats_qol_b_orig.csv")) 
) %>% 
  select(-se_mean) %>% 
  mutate(
    Rhat = pmax(Rhat, 1)
  ) %>% 
  filter(Var != "lp__")


xtable(tabs)



dat <- tar_read(data_raw, 2)

dat %>% 
  select(SID, PID) %>% 
  distinct() %>% 
  group_by(SID) %>% 
  summarise(n())


dat %>% 
  group_by(SID, PID) %>%
  summarise(nt = length(ti), t1 = max(ti)) %>% 
  group_by(SID) %>% 
  summarise(
    mean(nt),
    max(nt),
    median(t1),
    mean(nt == max(nt))
  )


dat %>% 
  group_by(PID) %>%
  summarise(nt = length(ti), t1 = max(ti)) %>% 
  summarise(
    mean(nt),
    max(nt),
    median(t1) * 365.25,
    mean(nt == max(nt)),
    mean(nt >= (max(nt) - 0))
  )


dat %>% 
  filter(EQ5D < 1) %>% 
  group_by(PID) %>%
  summarise(nt = length(ti), t1 = max(ti)) %>% 
  summarise(
    mean(nt),
    max(nt),
    median(t1) * 365.25,
    mean(nt == max(nt)),
    mean(nt >= (max(nt) - 1))
  )

dat %>% pull(EQ5D) %>% summary

dat %>% filter(EQ5D < 1) %>% pull(EQ5D) %>% summary


