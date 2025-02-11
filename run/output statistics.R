library(tidyverse)


source(here::here("R", "misc.R"))

pars <- read_csv(here::here("pars", "pars_shortfall_orig.csv"))



pars %>% 
  crossing(Age = c(50, 60, 70, 80, 90)) %>% 
  mutate(
    rate = r0 * exp(Age * ba1),
    dur = 1 / rate * 365
  ) %>% 
  group_by(Age) %>% 
  summarise(
    across(c(rate, dur), amlu) 
  )
