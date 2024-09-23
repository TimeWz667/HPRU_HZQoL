library(tidyverse)
library(survival)
library(rstan)

theme_set(theme_bw())


## Setting
options(mc.cores = 6)
rstan_options(auto_write = TRUE)

source(here::here("scripts", "fn_stan.R"))

n_iter <- 20000
n_collect <- 500
n_warmup <- floor(n_iter - n_collect)
n_chains <- 4


## Load data
dat <- read_csv(here::here("data", "tte_uk.csv")) %>% 
  filter(T_evt2 > 0)

dat %>% 
  group_by(Agp) %>% 
  summarise(Dur = mean(T_evt2)) %>% 
  ggplot(aes(x = Agp, y = log(1 / Dur))) +
  geom_point() +
  expand_limits(y = 0)


dat %>% 
  ggplot(aes(x = Age, y = log(T_evt2))) +
  geom_point() +
  geom_smooth(method = "lm")


summary(lm(log(T_evt2) ~ Age, data = dat))


## Model fitting
model_src <- "time2zero_age"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))



ds <- local({
  d <- dat %>% filter(T_evt > 0)
  list(
    Ts = d$T_evt2,
    As = d$Age,
    N = nrow(d)
  )
})

post <- sampling(model, data = ds, pars = c("r0", "ba1"), init = \() { list(r0 = exp(-2.35), ba1 = 0.01)},
                 chains = n_chains, iter = n_iter, warmup = n_warmup)


res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))



res$Ext %>% 
  crossing(Age = 30:95) %>% 
  mutate(
    rate = r0 * exp(ba1 * Age),
    dur = 1 / rate
  ) %>% 
  group_by(Age) %>% 
  summarise(
    M = median(dur),
    L = quantile(dur, 0.025),
    U = quantile(dur, 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Age, ymin = L, ymax = U), alpha = 0.1) +
  geom_line(aes(x = Age, y = M)) +
  geom_point(data = dat, aes(x = Age, y = T_evt))











