library(targets)
library(survival)
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

fit_cox <- coxph(Surv(time, status) ~ Age, training)
fit_cox
plot(basehaz(fit_cox))


for (key in c("weibull", "gamma", "lognormal", "gompertz")) {
  cat("Distribution: ", key)
  
  fit_phreg <- survstan::phreg(Surv(time, status) ~ Age, training, baseline = key)
  
  cat(", coef(age) = ", fit_phreg$estimates[1])
  cat(", mse = ", mean(log(fit_phreg$residuals) ^ 2))
  cat(", aic = ", AIC(fit_phreg))
  cat("\n")
}


fit_phreg <- survstan::phreg(Surv(time, status) ~ Age, training, baseline = "exponential")
est <- fit_phreg$estimates
est[2]
sim_e <- rexp(10000, est[2])

fit_phreg <- survstan::phreg(Surv(time, status) ~ Age, training, baseline = "gamma")
est <- fit_phreg$estimates
est[2] * est[3]
sim_g <- rgamma(1e4, est[2], est[3])

bind_rows(
  tibble(x = sim_e, d = "e"),
  tibble(x = sim_g, d = "g")
) %>% 
  ggplot() + 
  geom_density(aes(x = x, fill = d), alpha = 0.3)


## Bayesian approach

rstan_options(auto_write = TRUE)
options(mc.cores = 6)

n_iter <- 5000
n_warmup <- n_iter - 1000



ds <- local({
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


#ds$ba1 <- coef(fit)[1]


model_e <- stan_model(here::here("models", "t2z_haz_baseline_exp.stan"))
model_g <- stan_model(here::here("models", "t2z_haz_baseline_gamma.stan"))
model_l <- stan_model(here::here("models", "t2z_haz_baseline_lognorm.stan"))
model_w <- stan_model(here::here("models", "t2z_haz_baseline_weibull.stan"))


samples_e <- sampling(model_e, ds, pars = c("ba0", "ba1", "r0", "tte_sim"), iter = n_iter, warmup = n_warmup)
samples_g <- sampling(model_g, ds, pars = c("ba0", "ba1", "alpha", "r0", "tte_sim"), iter = n_iter, warmup = n_warmup)
#samples_l <- sampling(model_l, ds, pars = c("ba1", "mu", "sigma", "tte_sim"), iter = n_iter, warmup = n_warmup)
samples_w <- sampling(model_w, ds, pars = c("ba0", "ba1", "alpha", "sigma", "tte_sim"), iter = n_iter, warmup = n_warmup)

l_e <- extract_log_lik(samples_e, "lp__", F)
l_g <- extract_log_lik(samples_g, "lp__", F)
# l_l <- extract_log_lik(samples_l, "lp__", F)
l_w <- extract_log_lik(samples_w, "lp__", F)


tab_waic <- cbind(
  waic(l_e)$estimates[, 1],
  waic(l_g)$estimates[, 1],
  # waic(l_l)$estimates[, 1],
  waic(l_w)$estimates[, 1]
)

tab_loo <- cbind(
  loo(l_e)$estimates[, 1],
  loo(l_g)$estimates[, 1],
  #loo(l_l)$estimates[, 1],
  loo(l_w)$estimates[, 1]
)


tab_gof <- rbind(tab_waic, tab_loo)
tab_gof <- data.frame(tab_gof)
colnames(tab_gof) <- c("Exponential", "Gamma", "Weibull")

write_csv(tab_gof, here::here("docs", "tabs", "explore_tte_gof.csv"))

tab_gof

bind_rows(
  tibble(
      tte = extract(samples_e, pars = "tte_sim")[[1]],
      Dist = "Exp"
  ),
  tibble(
    tte = extract(samples_g, pars = "tte_sim")[[1]],
    Dist = "Gamma"
  ),
  tibble(
    tte = extract(samples_w, pars = "tte_sim")[[1]],
    Dist = "Weibull"
  )
) %>% 
  group_by(Dist) %>% 
  summarise(
    M = mean(tte),
    L = quantile(tte, 0.025),
    U = quantile(tte, 0.975)
  )

