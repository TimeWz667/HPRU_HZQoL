library(tidyverse)

theme_set(theme_bw())


source(here::here("scripts/fn_sim.R"))

## Load data
load(here::here("data", "qol_reformed.rdata"))

min_qol <- min(reformed$EQ5D)

norm <- reformed %>% select(Age, Norm, Norm_rescaled) %>% distinct() %>% arrange(Age)
norm


## Target population
targets <- tibble(
  Key = 1:2000
) %>% 
  crossing(Age = c(50, 60, 70, 80), Ti = seq(0, 0.5, 0.05))


## Functions

pars_time2zero <- read_csv(here::here("posteriors", "post_time2zero_age.csv"))
head(pars_time2zero)


post_logit_qol_a <- read_csv(here::here("posteriors", "post_logit_qol_a.csv"))
head(post_logit_qol_a)


post_logit_qol_at <- read_csv(here::here("posteriors", "post_logit_qol_at.csv"))
head(post_logit_qol_at)


post_logit_cap_qol_at <- read_csv(here::here("posteriors", "post_logit_cap_qol_at.csv"))
head(post_logit_cap_qol_at)



## T2Z + const qol
proj_m1 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_a(post_logit_qol_a, min_qol)

## T2Z + qol(t)
proj_m2 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_at(post_logit_qol_at, min_qol)

## Capped logit
proj_m3 <- targets %>% 
  sim_cap_qol_at(post_logit_cap_qol_at, min_qol, norm)


## Plot projections
proj_m1 %>% 
  group_by(Age, Ti) %>% 
  summarise(
    p_z = mean(p_z),
    qol = mean(qol)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Ti, y = qol, colour = as.character(Age))) + 
  geom_line(aes(x = Ti, y = p_z, colour = as.character(Age))) + 
  scale_y_continuous("Pr(health)", labels = scales::percent) +
  scale_color_discrete("Age") +
  expand_limits(y = 0)


proj_m2 %>% 
  group_by(Age, Ti) %>% 
  summarise(
    p_z = mean(p_z),
    qol = mean(qol)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Ti, y = qol, colour = as.character(Age))) + 
  geom_line(aes(x = Ti, y = p_z, colour = as.character(Age))) + 
  scale_y_continuous("Pr(health)", labels = scales::percent) +
  scale_color_discrete("Age") +
  expand_limits(y = 0)


proj_m3 %>% 
  group_by(Age, Ti) %>% 
  summarise(
    qol = mean(qol),
    norm = mean(norm),
    p_z = mean(p_z)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Ti, y = qol, colour = as.character(Age))) + 
  geom_line(aes(x = Ti, y = norm, colour = as.character(Age)), linetype = 2) + 
  scale_y_continuous("QOL", labels = scales::percent) +
  scale_color_discrete("Age") +
  expand_limits(y = 0:1)


reformed %>% 
  mutate(
    Agp = cut(Age, c(0, 50, 70, 80, 100), right = F)
  ) %>% 
  group_by(Agp) %>%
  arrange(ti, Health) %>% 
  mutate(
    #p = cumsum(EQ5D == 1) / n(),
    p = cumsum(Health) / n(),
    rate = -log(1 - p) / ti
  ) %>% 
  filter(ti >= 0.05) %>% 
  ggplot() +
  geom_line(aes(x = ti, y = rate, colour = Agp))


