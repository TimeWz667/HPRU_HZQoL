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


load(here::here("data", "sup_demo.rdata"))
norm <- sup_demo %>% 
  filter(Year == 2023) %>% 
  ungroup() %>% 
  select(Age, mr = mortality, norm = norm_leoss) %>% 
  mutate(
    Norm_rescaled = (norm - min_qol) / (1 - min_qol)
  ) %>% 
  select(Age, Norm_rescaled)

## Fitted 
targets <- reformed %>%
  mutate(Key = as.numeric(as.factor(PID))) %>% 
  select(Key, Age, Ti = ti, Health, EQ5D)


## T2Z + const qol
fitted_m1 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_a(post_logit_qol_a, min_qol)


## T2Z + qol(t)
fitted_m2 <- targets %>% 
  sim_t2z(pars_time2zero) %>% 
  sim_qol_at(post_logit_qol_at, min_qol)


## Capped logit
fitted_m3 <- targets %>% 
  sim_cap_qol_at(post_logit_cap_qol_at, min_qol, norm)


get_roc <- \(df) {
  bind_rows(lapply(seq(0, 1, 0.01), \(b) {
    list(
      Thres = b,
      TP = df %>% 
        filter(Health == 1) %>% 
        summarise(v = mean(p_z > b)) %>% 
        pull(v),
      FP = df %>% 
        filter(Health == 0) %>% 
        summarise(v = mean(p_z > b)) %>% 
        pull(v)
    )
  }))
}

rocs <- bind_rows(
  fitted_m1 %>% get_roc %>% mutate(Model = "M1"), 
  fitted_m2 %>% get_roc %>% mutate(Model = "M2"), 
  fitted_m3 %>% get_roc %>% mutate(Model = "M3")  
) 


rocs %>% 
  ggplot() +
  geom_line(aes(x = FP, y = TP, colour = Model)) +
  geom_abline(slope = 1) +
  scale_y_continuous("True positive, %", labels = scales::percent) +
  scale_x_continuous("False positive, %", labels = scales::percent)


resids <- bind_rows(
  fitted_m1 %>% mutate(Model = "M1"),
  fitted_m2 %>% mutate(Model = "M2"),
  fitted_m3 %>% mutate(Model = "M3")
) %>% 
  filter(Health == 0) %>% 
  mutate(
    q = (EQ5D - min_qol) / (1 - min_qol),
    lq = log(q / (1 - q)),
    qhat = (qol - min_qol) / (1 - min_qol),
    lqhat = log(qhat / (1 - qhat)),
    resid_lo = lq - lqhat,
    resid_original = EQ5D - qol
  ) 

resids %>% 
  group_by(Model) %>% 
  summarise(
    mse = mean(resid_original ^ 2)
  )


resids %>% 
  ggplot() +
  geom_density(aes(x = resid_lo, fill = Model), alpha = 0.3)


resids %>% 
  ggplot() +
  geom_point(aes(x = qol, y = resid_lo, colour = Model), alpha = 0.3) +
  facet_wrap(.~Model)



resids %>% 
  ggplot() +
  geom_density(aes(x = resid_original, fill = Model), alpha = 0.3) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~Model) + 
  expand_limits(y = 6)





dat <- reformed %>% 
  filter(Health == 0)


dat %>% 
  mutate(
    cluster = kmeans(EQ5D, 2)$cluster,
    cluster = as.character(cluster)
  ) %>% 
  ggplot() + 
  geom_point(aes(x = ti, y = EQ5D, colour = cluster))


clusters <- reformed %>% 
  #filter(Health == 0) %>% 
  group_by(PID) %>%
  slice(1:3) %>%
  summarise(EQ5D = mean(EQ5D)) %>%
  ungroup() %>%
  mutate(
    cluster = kmeans(EQ5D, 2)$cluster
  ) %>% 
  select(PID, cluster)


dd <- reformed %>% 
  #filter(Health == 0) %>% 
  group_by(PID) %>%
  slice(1:2) %>% 
  select(PID, EQ5D) %>% 
  mutate(SID = paste0("S", 1:n())) %>% 
  pivot_wider(names_from = SID, values_from = EQ5D) %>% 
  ungroup()


dd <- dd %>% 
  mutate(
    cluster = kmeans(cbind(dd$S1, dd$S2), 2)$cluster
  ) %>% 
  select(PID, cluster)



reformed %>% 
  left_join(dd, by = "PID") %>% 
  ggplot() + 
  geom_density(aes(x = EQ5D, fill = as.character(cluster)), alpha = 0.2)










