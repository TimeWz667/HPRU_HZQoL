library(targets)

library(tidyverse)

pars <- tar_read(pars_shortfall, 1)


pars %>% 
  filter(Key < 100) %>% 
  crossing(days = seq(15, 75, 30), Age = seq(50, 90, 10)) %>% 
  mutate(
    ti = days / 365,
    rate = r0 * exp(Age * ba1),
    pr_hz = 1 - pexp(ti, rate),
    Cluster = runif(n()),
    Cluster = case_when(
      Cluster < Prop_0 ~ "0",
      Cluster < Prop_0 + Prop_1 ~ "1",
      T ~ "2",
    ),
    Q = case_when(
      Cluster == "0" ~ Q_0,
      Cluster == "1" ~ Q_1,
      T ~ Q_2
    ),
    qol = (Q_0 * Prop_0 + Q_1 * Prop_1 + Q_2 * Prop_2) * pr_hz + (1 - pr_hz)
  ) %>% 
  group_by(days, Age) %>% 
  summarise(
    qol = mean(qol)
  ) %>% 
  group_by(Age) %>% 
  mutate(
    days = days + 15,
    ql = cumsum(qol * 30 / 365)
  ) %>% 
  arrange(Age)
  
  
  




q0 = q0
q_hz <- function(a0, at) {}

pr_hz <- function(a0, at) {}

surv <- function(a0, at, hz = F) {}


dt = 0.001

QL = 0
for (at in seq(a0, 100, dt)) {
  a = q0 * surv(a0, at, hz = F)
  b = (min(q_hz, q0) *      pr_hz(a0, at)  * surv(a0, at, hz = T)
       b = b +        q0  * (1 - pr_hz(a0, at)) * surv(a0, at, hz = F)
       
       QL = QL + (a - b) * dt
       
       QL = QL + (q0 - min(q_hz, q0)) * pr_hz(a0, at) * surv(a0, at, hz = T) * dt
       
       QL = QL + (q0 - min(q_hz, q0)) * pr_hz(a0, at) * dt
       
}

QL = (q0 - min(q_hz, q0)) * duration(a0)








