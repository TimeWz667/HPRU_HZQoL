library(targets)
#library(survival)
library(loo)
library(tidyverse)
library(rstan)
library(survstan)


dat_tte <- tar_read(subdata_tte)

## Frequentist approach

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



mean(resid(fit_cox) ^ 2)

fit_cox <- coxreg(Surv(time, status) ~ Age, training)
fit_cox
plot(basehaz(fit_cox))


for (key in c("exponential", "weibull", "lognormal", "loglogistic", "fatigue", "gamma", "gompertz")) {
  cat("Distribution: ", key)
  
  fit_phreg <- survstan::phreg(Surv(time, status) ~ Age, training, baseline = key)
  
  cat(", coef(age) = ", fit_phreg$estimates[1])
  cat(", mse = ", mean(log(fit_phreg$residuals) ^ 2))
  cat("\n")
}

# gamma and weibull


## Bayesian approach


rstan_options(auto_write = TRUE)
options(mc.cores = 6)

n_iter <- 5000
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


ds$ba1 <- coef(fit)[1]




model_e <- stan_model(here::here("models", "t2z_haz_baseline_exp.stan"))
model_g <- stan_model(here::here("models", "t2z_haz_baseline_gamma.stan"))
model_l <- stan_model(here::here("models", "t2z_haz_baseline_lognorm.stan"))
model_w <- stan_model(here::here("models", "t2z_haz_baseline_weibull.stan"))


samples_e <- sampling(model_e, ds, pars = c("r0"), iter = n_iter, warmup = n_warmup)
samples_g <- sampling(model_g, ds, pars = c("alpha", "r0"), iter = n_iter, warmup = n_warmup)
samples_l <- sampling(model_l, ds, pars = c("mu", "sigma"), iter = n_iter, warmup = n_warmup)
samples_w <- sampling(model_w, ds, pars = c("alpha", "sigma"), iter = n_iter, warmup = n_warmup)

l_e <- extract_log_lik(samples_e, "lp__", F)
l_g <- extract_log_lik(samples_g, "lp__", F)
l_l <- extract_log_lik(samples_l, "lp__", F)
l_w <- extract_log_lik(samples_w, "lp__", F)


library(survstan)
fit <- survstan::phreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, baseline = "weibull")
summary(fit)


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

cbind(
  waic(l_e)$estimates[, 1],
  waic(l_g)$estimates[, 1],
  waic(l_l)$estimates[, 1],
  waic(l_w)$estimates[, 1]
)


cbind(
  loo(l_e)$estimates[, 1],
  loo(l_g)$estimates[, 1],
  loo(l_l)$estimates[, 1],
  loo(l_w)$estimates[, 1]
)


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

