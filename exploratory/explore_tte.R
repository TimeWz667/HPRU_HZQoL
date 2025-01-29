library(targets)
library(survival)
library(loo)
library(tidyverse)
library(rstan)




dat_tte <- tar_read(subdata_tte)

dat_tte$training


n_iter <- 10000
n_warmup <- n_iter - 1000

ds <- local({
  
  #dat_tte <- tar_read(data_tte) #%>% head(200)
  dat_tte <- dat_tte$training %>% filter(T_evt > 0)
  
  dat_int <- dat_tte %>% filter(Recovered)
  dat_cen <- dat_tte %>% filter(!Recovered) %>% filter(!is.na(Age))
  
  list(
    N = nrow(dat_int),
    Ts0 = dat_int$T_last,
    Ts1 = dat_int$T_end,
    As = dat_int$Age,
    N_Cen = nrow(dat_cen),
    Ts_Cen = dat_cen$T_end,
    As_Cen = dat_cen$Age
  )
})


training <- dat_tte$training %>% 
  mutate(
    time_f = (T_last + T_end) / 2,
    time_c = T_end,
    time = ifelse(Recovered, time_f, time_c),
    status = Recovered + 0,
    SID = as.character(as.numeric(factor(SID)))
  ) %>% 
  filter(time > 0) %>% 
  select(time, status, SID, Age)


fit <- coxph(Surv(time, status) ~ Age, training)


ds$ba1 <- coef(fit)[1]


model_e <- stan_model(here::here("models", "t2z_surv_exp_a.stan"))
samples_e <- sampling(model_e, ds, pars = c("r0"), iter = n_iter, warmup = n_warmup, cores = 4)

model_g <- stan_model(here::here("models", "t2z_surv_gamma_a.stan"))
samples_g <- sampling(model_g, ds, pars = c("alpha", "r0"), iter = n_iter, warmup = n_warmup, cores = 4)

model_l <- stan_model(here::here("models", "t2z_surv_lognorm_a.stan"))
samples_l <- sampling(model_l, ds, pars = c("mu", "sigma"), iter = n_iter, warmup = n_warmup, cores = 4)

model_w <- stan_model(here::here("models", "t2z_surv_weibull_a.stan"))
samples_w <- sampling(model_w, ds, pars = c("alpha", "sigma"), iter = n_iter, warmup = n_warmup, cores = 4)

l_e <- extract_log_lik(samples_e, "lp__", F)
l_g <- extract_log_lik(samples_g, "lp__", F)
l_l <- extract_log_lik(samples_l, "lp__", F)
l_w <- extract_log_lik(samples_w, "lp__", F)






# model_e <- stan_model(here::here("models", "t2z_surv_exp.stan"))
# samples_e <- sampling(model_e, ds, pars = c("r0", "ba1"), iter = n_iter, warmup = n_warmup, cores = 4)
# 
# model_g <- stan_model(here::here("models", "t2z_surv_gamma.stan"))
# samples_g <- sampling(model_g, ds, pars = c("alpha", "r0", "ba1"), iter = n_iter, warmup = n_warmup, cores = 4)
# 
# model_l <- stan_model(here::here("models", "t2z_surv_lognorm.stan"))
# samples_l <- sampling(model_l, ds, pars = c("mu", "sigma", "ba1"), iter = n_iter, warmup = n_warmup, cores = 4)
# 
# l_e <- extract_log_lik(samples_e, "lp__")
# l_g <- extract_log_lik(samples_g, "lp__")
# l_l <- extract_log_lik(samples_l, "lp__")


waic(l_e)
waic(l_g)
waic(l_l)
waic(l_w)
loo(l_e)
loo(l_g)
loo(l_l)
loo(l_w)

loo_compare(loo(l_e), loo(l_g), loo(l_l), loo(l_w))





plot(basehaz(fit))


bind_rows(
  extract(samples_g) %>% as_tibble() %>% 
    mutate(
      tte = rgamma(n(), alpha, r0),
      Dist = "Gamma"
    ) %>% 
    select(tte, Dist),
  extract(samples_e) %>% as_tibble() %>% 
    mutate(
      tte = rexp(n(), r0),
      Dist = "Exp"
    ) %>% 
    select(tte, Dist),
  extract(samples_l) %>% as_tibble() %>% 
    mutate(
      tte = rlnorm(n(), log(mu), (sigma)),
      Dist = "lnorm"
    ) %>% 
    select(tte, Dist),
  extract(samples_w) %>% as_tibble() %>% 
    mutate(
      tte = rweibull(n(), alpha, sigma),
      Dist = "Weibull"
    ) %>% 
    select(tte, Dist)
) %>% 
  group_by(Dist) %>% 
  summarise(
    M = mean(tte),
    L = quantile(tte, 0.025),
    U = quantile(tte, 0.975)
  )


