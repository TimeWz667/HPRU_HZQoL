library(tidyverse)
library(rstan)
library(loo)


load(file = here::here("data", "sub_qol.rdata"))

## Bayesian approach

training <- dat_qol$training 

ds_nz <- training %>% filter(EQ5D < 1)

clu <- kmeans(ds_nz$EQ5D, 2)

if (clu$centers[1] > clu$centers[2]) {
  ds_nz <- ds_nz %>% 
    mutate(
      cluster = clu$cluster
    )
} else {
  ds_nz <- ds_nz %>% 
    mutate(
      cluster = 3 - clu$cluster
    )
}


ds_c1 <- ds_nz %>% filter(cluster == 1)
ds_c2 <- ds_nz %>% filter(cluster == 2)
ds_zero <- training %>% 
  mutate(
    cluster = (EQ5D >= 1) + 0
  )

ds_clu1 <- ds_nz %>% 
  mutate(
    cluster = (cluster == 1) + 0
  )


options(mc.cores = 8)
rstan_options(auto_write = TRUE)

model_norm <- stan_model(here::here("models", "reff_norm_0.stan"))
model_norm_t <- stan_model(here::here("models", "reff_norm_t.stan"))
model_norm_tg <- stan_model(here::here("models", "reff_norm_tg.stan"))

model_logit <- stan_model(here::here("models", "reff_logistic_0.stan"))
model_logit_d <- stan_model(here::here("models", "reff_logistic_d.stan"))
model_logit_ad <- stan_model(here::here("models", "reff_logistic_ad.stan"))



## C1 -----
ds <- local({
  ds <- ds_c1 %>%
    sample_n(200) %>% 
    mutate(
      Gs = as.numeric(as.factor(SID)),
      Ys = EQ5D,
      Ts = ti
    ) %>% 
    select(Gs, Ys, Ts) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})


post_0 <- sampling(model_norm, data = ds[c("Ys", "N", "Gs", "N_gp")], chains = 3, pars = c("b0"))
post_1 <- sampling(model_norm_t, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0", "bt"))
post_2 <- sampling(model_norm_tg, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0"))


tab_loo <- cbind(
  loo(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_loo # -374.594615 -370.165848 -373.790026

tab_waic <- cbind(
  waic(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_waic # -377.171196 -373.980083 -374.742687


## C2 -----
ds <- local({
  ds <- ds_c2 %>%
    sample_n(200) %>% 
    mutate(
      Gs = as.numeric(as.factor(SID)),
      Ys = EQ5D,
      Ts = ti
    ) %>% 
    select(Gs, Ys, Ts) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})


post_0 <- sampling(model_norm, data = ds[c("Ys", "N", "Gs", "N_gp")], chains = 3, pars = c("b0"))
post_1 <- sampling(model_norm_t, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0", "bt"))
post_2 <- sampling(model_norm_tg, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0"))

tab_loo <- cbind(
  loo(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_loo

tab_waic <- cbind(
  waic(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_waic



## pz -----

ds <- local({
  ds <- ds_zero %>%
    sample_n(200) %>% 
    mutate(
      Gs = as.numeric(as.factor(SID)),
      Ys = cluster,
      Ts = ti,
      As = Age
    ) %>% 
    select(Gs, Ys, Ts, As) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})


post_0 <- sampling(model_logit, data = ds[c("Ys", "N", "Gs", "N_gp")], chains = 3, pars = c("b0"))
post_1 <- sampling(model_logit_d, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0", "bd15", "bd30"))
post_2 <- sampling(model_logit_ad, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts", "As")], chains = 3, pars = c("b0", "bd15", "bd30", "ba1", "ba2"))


tab_loo <- cbind(
  loo(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_loo # 156.741951 152.513784 152.408281

tab_waic <- cbind(
  waic(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_waic # 155.387890 150.159993 151.740055


## pc1 -----

ds <- local({
  ds <- ds_clu1 %>%
    sample_n(200) %>% 
    mutate(
      Gs = as.numeric(as.factor(SID)),
      Ys = cluster,
      Ts = ti,
      As = Age
    ) %>% 
    select(Gs, Ys, Ts, As) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})


post_0 <- sampling(model_logit, data = ds[c("Ys", "N", "Gs", "N_gp")], chains = 3, pars = c("b0"))
post_1 <- sampling(model_logit_d, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], chains = 3, pars = c("b0", "bd15", "bd30"))
post_2 <- sampling(model_logit_ad, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts", "As")], chains = 3, pars = c("b0", "bd15", "bd30", "ba1", "ba2"))


tab_loo <- cbind(
  loo(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  loo(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_loo # 183.421061 182.540720 174.541843

tab_waic <- cbind(
  waic(extract_log_lik(post_0, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_1, "lp__", F))$estimates[, 1],
  waic(extract_log_lik(post_2, "lp__", F))$estimates[, 1]
)
tab_waic # 181.48950 179.386443 173.136776



