library(tidyverse)

theme_set(theme_bw())


source(here::here("scripts/fn_sim.R"))
pars_time2zero <- read_csv(here::here("posteriors", "post_time2zero_age.csv"))
post_logit_qol_a <- read_csv(here::here("posteriors", "post_logit_qol_a.csv"))
post_logit_qol_at <- read_csv(here::here("posteriors", "post_logit_qol_at.csv"))
post_logit_cap_qol_at <- read_csv(here::here("posteriors", "post_logit_cap_qol_at.csv"))


## Load data
load(here::here("data", "qol_reformed.rdata"))
min_qol <- min(reformed$EQ5D)



## Fitted 

targets <- tibble(
  Key = 1:n_sim
) %>% 
  crossing(Age = 50:99, Ti = seq(0, 3, 0.02))


## T2Z + const qol
proj_m1 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_a(post_logit_qol_a, min_qol) %>% 
  calc_shortfall(pars_demo)


## T2Z + qol(t)
proj_m2 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_at(post_logit_qol_at, min_qol) %>% 
  calc_shortfall(pars_demo)


## Capped logit
proj_m3 <- targets %>% 
  sim_cap_qol_at(post_logit_cap_qol_at, min_qol, norm) %>% 
  calc_shortfall(pars_demo)

