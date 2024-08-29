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

pars_qol <- read_csv(file = here::here("results", "post_qol_(a).csv")) %>% 
  select(qb0 = b0, qb1 = b1, qb2 = b2) %>% 
  mutate(Key = sample(1:n(), n()), min_qol = min_qol) %>% 
  arrange(Key)


head(pars_qol)

pars_ttz <- read_csv( here::here("results", "post_ttz_(a).csv")) %>% 
  select(rb0 = b0, rb1 = b1) %>% 
  mutate(Key = sample(1:n(), n())) %>% 
  arrange(Key)

head(pars_ttz)


pars_hz <- merge(pars_qol, pars_ttz) %>% 
  crossing(age0 = 20:100) %>% 
  mutate(
    qol = qb0 + qb1 * age0 + qb2 * age0 ^ 2,
    qol = 1 / (1 + exp(- qol)),
    qol = qol * (1 - min_qol) + min_qol,
    r2healthy = exp(rb0 + rb1 * age0) 
  ) %>% 
  select(Key, age0, qol, r2healthy)


pars_hz %>% 
  ggplot() +
  stat_lineribbon(aes(x = age0, y = r2healthy)) + 
  expand_limits(y = 0) +
  scale_fill_brewer()

pars_hz %>% 
  ggplot() +
  stat_lineribbon(aes(x = age0, y = qol)) + 
  geom_point(data = raw %>% filter(EQ5D != 1), aes(x = age, y = EQ5D), alpha = 0.05) +
  expand_limits(y = 0:1) +
  scale_fill_brewer()


plot(density(raw$EQ5D[raw$EQ5D != 1]))


## simulate

simulate_qale <- function(age, pars_hz, pars_demo) {
  pars_hz %>% 
    filter(age0 == age) %>% 
    crossing(age = age:100) %>% 
    left_join(pars_demo, by = "age") %>% 
    group_by(Key) %>% 
    mutate(
      ti = 1:n(),
      pd = diff(c(0, pexp(ti, r2healthy)))
    ) %>% 
    mutate(
      qdz = pmin(norm, qol),
      qw = pd * qdz + (1 - pd) * norm,
      qwf = pd * qol + (1 - pd) * 1,
      surv = c(1, cumprod(1 - mr)[-n()]),
      d15 = (1 + 0.015) ^ -(ti - 1),
      d35 = (1 + 0.035) ^ -(ti - 1)
    ) %>% 
    group_by(Key, Age = age0) %>% 
    summarise(
      Qol0 = mean(qol), RRec = mean(r2healthy),
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


write_csv(sims, here::here("results", "sim_ql.csv"))


ggplot(sims) + 
  stat_lineribbon(aes(x = Age, y = QL00)) +
  scale_fill_brewer() 


g_ql <- ggplot(sims %>% filter(Age < 99)) + 
  stat_lineribbon(aes(x = Age, y = QL35)) +
  scale_y_continuous("QALY loss at 3.5% discounting rate") +
  scale_x_continuous("Age of HZ onset") +
  scale_fill_brewer() + 
  expand_limits(y = 0)

g_ql

ggsave(g_ql, filename = here::here("docs", "figs", "proj_qol.png"), width = 6, height = 4.5)


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


write_csv(res, here::here("docs", "tabs", "projection.csv"))


res %>% 
  pivot_longer(-Age) %>% 
  separate(name, c("Index", "Stat"), "_") %>% 
  pivot_wider(names_from = "Stat") %>% 
  filter(startsWith(Index, "QL")) %>% 
  data.frame()


# With the model fits, the trajectory of qALY associated with HZ were simulated with a two step process. 
# - Simulate a time to recovery from an exponential distribution
# - Simulate a
