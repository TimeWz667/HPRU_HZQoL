library(tidyverse)
library(rstan)

theme_set(theme_bw())

options(mc.cores = 6)
rstan_options(auto_write = TRUE)



proj_qol <- function(pars) {
  with(pars, {
    mu <- a0 + at * seq(0, 1, 0.01)
    qol <- 1 / (1 + exp(- mu))
    qol <- pmin(qol, PopNorm)
    
    data.frame(t = seq(0, 1, 0.01), QoL = qol)
  })
}

rand_qol <- function(pars, n = 100) {
  with(pars, {
    ti <- runif(n, 0, 1)
    ti <- round(ti * 365) / 365
    q0 <- rnorm(n, a0 + at * ti, sig)
    q1 <- 1 / (1 + exp(- q0))
    q1 <- pmin(q1, PopNorm)
    data.frame(ID = 1:n, t = ti, QoL = q1)
  })
}


p0 <- list(
  PopNorm = 0.8, 
  a0 = -2.5, 
  at = 7,
  sig = 1
)


actual <- proj_qol(p0)
sims <- rand_qol(p0)


ggplot(sims) +
  geom_point(aes(x = t, y = QoL)) +
  geom_line(data = actual, aes(x = t, y = QoL)) +
  scale_y_continuous("QoL, %", labels = scales::percent) +
  expand_limits(y = c(0, 1))


head(sims)

ds <- list(
  Ts = sims$t,
  Qs = sims$QoL,
  Zs = (sims$QoL == p0$PopNorm) + 0,
  N = nrow(sims),
  Cap = rep(log(p0$PopNorm / (1 - p0$PopNorm)), nrow(sims))
)

ds

model <- stan_model(here::here("models", "logit_cap_qol_t.stan"))


post <- sampling(model, data = ds, pars = c("b0", "b1", "p_z"), 
                 chains = 3, iter = 5000, warmup = 4500)


post


