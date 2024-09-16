library(rstan)


rstan_options(auto_write = TRUE)


model <- stan_model(here::here("models", "logit_cap_qol_at.stan"))
model <- stan_model(here::here("models", "logit_cap_qol_t.stan"))

model <- stan_model(here::here("models", "time2zero_age.stan"))
model <- stan_model(here::here("models", "time2zero.stan"))

model <- stan_model(here::here("models", "logit_qol.stan"))
model <- stan_model(here::here("models", "logit_qol_t.stan"))
model <- stan_model(here::here("models", "logit_qol_a.stan"))
model <- stan_model(here::here("models", "logit_qol_at.stan"))
model <- stan_model(here::here("models", "logit_qol_at_subject.stan"))
