library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

## load data
raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


max_qol <- 1
min_qol <- min(raw$EQ5D)


## load parameters

### exogenous parameters to support population dynamics and population norm
load(here::here("data", "sup_demo.rdata"))
pars_demo <- sup_demo %>% 
  filter(Year == 2023) %>% 
  select(age = Age, mr = mortality, norm = norm_leoss) %>% 
  ungroup()


### parameters from model fitting
set.seed(11667)

pars_hz <- read_csv(here::here("results", "fit_qol_lmer(a,t).csv"))
  

## simulate

simulate_qale <- function(age, pars_hz, pars_demo) {
  pars_hz %>% 
    mutate(Key = 1:n()) %>% 
    crossing(age0 = age) %>% 
    mutate(
      b0 = k + b_a1 * age0 + b_a2 * age0 ^ 2,
      b1 = b_t1,
      b2 = b_t2
    ) %>% 
    select(Key, age0, b0, b1, b2) %>% 
    arrange(Key) %>% 
    crossing(ti = seq(age, 100, 0.01) - age) %>% 
    mutate(
      age = age0 + floor(ti)
    )  %>% 
    left_join(pars_demo, by = "age") %>% 
    mutate(
      qol = b0 + pmax(b1 * ti+ b1 * ti ^ 2, 0),
      qol = 1 / (1 + exp(- qol)) * (1 - min_qol) + min_qol,
      qolf = qol,
      qol = pmin(qol, norm)
    ) %>% 
    group_by(Key, age0, age) %>% 
    summarise(
      ti = min(ti), qol = mean(qol), , qolf = mean(qolf)
    ) %>% 
    left_join(pars_demo, by = "age") %>% 
    group_by(Key) %>% 
    mutate(
      qdz = pmin(norm, qol),
      qw = qdz,
      qwf = qol,
      surv = c(1, cumprod(1 - mr)[-n()]),
      d15 = (1 + 0.015) ^ -ti,
      d35 = (1 + 0.035) ^ -ti
    ) %>% 
    group_by(Key, Age = age0) %>% 
    summarise(
      Qol0 = mean(qol),
      LE = sum(surv),
      QALE1 = sum(qw * surv),
      QALE0 = sum(norm * surv),
      QL00 = sum((norm - qw) * surv),
      QL15 = sum((norm - qw) * surv * d15),
      QL35 = sum((norm - qw) * surv * d35),
      QLF00 = sum((1 - qwf) * surv),
      QLF15 = sum((1 - qwf) * surv * d15),
      QLF35 = sum((1 - qwf) * surv * d35)
    ) %>% 
    ungroup()
}



sims <- bind_rows(lapply(20:100, \(age) {
  print(age)
  simulate_qale(age, pars_hz = pars_hz, pars_demo = pars_demo)
}))


write_csv(sims, here::here("results", "sim_ql_lmer.csv"))


g_ql <- ggplot(sims %>% filter(Age < 99)) + 
  stat_lineribbon(aes(x = Age, y = QL35)) +
  scale_y_continuous("QALY loss at 3.5% discounting rate") +
  scale_x_continuous("Age of HZ onset") +
  scale_fill_brewer() + 
  expand_limits(y = 0)

g_ql

ggsave(g_ql, filename = here::here("docs", "figs", "proj_qol_lmer.png"), width = 6, height = 4.5)


res <- sims %>% 
  select(- Key)  %>% 
  group_by(Age) %>% 
  summarise(
    across(everything(), list(
      Avg = mean,
      Q025 = \(x) quantile(x, 0.025),
      Q250 = \(x) quantile(x, 0.25),
      Q500 = \(x) quantile(x, 0.5),
      Q750 = \(x) quantile(x, 0.75),
      Q975 = \(x) quantile(x, 0.975)
    ))
  )


write_csv(res, here::here("docs", "tabs", "projection_lmer.csv"))

