library(targets)
library(tidyverse)


source(here::here("R", "misc.R"))

vset <- "orig"
age0 <- 70


pars_shortfall <- tar_read(pars_shortfall, 2)
data_norm <- tar_read(data_norm)



pars_demo <- data_norm %>% 
  filter(Year == 2024) %>% 
  ungroup() %>% 
  select(Age, mr = mortality, norm = norm_leoss) 



n_sim <- max(pars_shortfall$Key)

crossing(Key = 1:n_sim, Age = age0, ti = 0:(100 - age0)) %>% 
  mutate(
    AgeT = floor(Age + ti)
  ) %>% 
  left_join(pars_shortfall, by = "Key") %>% 
  left_join(pars_demo %>% rename(AgeT = Age), by = "AgeT") %>% 
  group_by(Key) %>% 
  mutate(
    rate = r0 * exp(Age * ba1),
    p_health90 = case_when(
      ti == 0 ~ 1 + (exp(-rate * (ti + 90/ 365.25)) - exp(-rate * ti)) / rate,
      T ~ 1
    ),
    p_hz90 = 1 - p_health90,
    p_health = 1 + (exp(-rate * (ti + 1)) - exp(-rate * ti)) / rate,
    p_hz = 1 - p_health,
    ql_ph = Q_0 * Prop_0 + Q_1 * Prop_1 + Q_2 * Prop_2,
    ql_ph90 = (1 - ql_ph) * p_hz90,
    ql_ph = (1 - ql_ph) * p_hz,
    surv = cumprod(1 - mr),
    surv = (surv + c(1, surv[-n()])) / 2
  ) %>% 
  group_by(Key, Age) %>% 
  summarise(
    LE = sum(surv),
    QLH = sum(ql_ph * surv),
    QLH90 = sum(ql_ph90 * surv),
    
  ) %>% 
  mutate(
    PLoss = QLH90 / QLH
  ) %>% 
  group_by(Age) %>% 
  summarise_all(amlu)


#### 32-47% at 70YOA
#### 38-44% at 80YOA

