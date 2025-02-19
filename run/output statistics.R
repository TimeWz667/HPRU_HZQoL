library(tidyverse)


source(here::here("R", "misc.R"))

pars <- read_csv(here::here("pars", "pars_shortfall_orig.csv"))


tab <- read_csv(here::here("docs", "tabs", "QALY_loss_stats_orig.csv"))

tab %>% pull(Index) %>% unique()

tab_wider <- tab %>% 
  filter(Index %in% c("QL00", "QL15", "QLH00", "QLH15")) %>% 
  mutate(
    value = sprintf("%.3f (%.3f - %.3f)", M, L, U) 
  )  %>% 
  select(Age, name = Index, value) %>% 
  pivot_wider()


tab_wider %>% 
  filter(Age < 99) %>% 
  rename(
    `From perfect health, d = 0%` = QLH00,
    `From perfect health, d = 15%` = QLH15,
    `From population norm, d = 0%` = QL00,
    `From population norm, d = 15%` = QL15
  ) %>% 
  write_csv(here::here("docs", "tabs", "QALY_loss_summary.csv"))


