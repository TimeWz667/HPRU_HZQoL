library(tidyverse)
library(rstan)


theme_set(theme_bw())

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## simulation study

n_sim <- 100


mu <- c(0.6, 0.9)
sig <- c(0.06, 0.05)

p0 <- 0.2


sims <- tibble(Key = 1:n_sim) %>% 
  mutate(
    x1 = rnorm(n(), mu[1], sig[1]),
    x2 = rnorm(n(), mu[2], sig[2]),
    x = ifelse(runif(n()) < p0, x1, x2)
  ) %>% 
  select(Key, x)


plot(density(sims$x))




sims_c <- sims %>% 
  mutate(
    cluster = kmeans(x, 2)$cluster,
    cluster = as.character(cluster)
  )

stats <- sims_c %>% 
  group_by(cluster) %>% 
  summarise(
    mu = mean(x),
    std = sd(x)
  )


sims_c %>% 
  ggplot() +
  geom_density(aes(x = x, fill = cluster), alpha = 2) +
  geom_vline(data = stats, aes(xintercept = mu))



ds <- list(
  N = nrow(sims),
  Ys = sims$x
)


model <- rstan::stan_model(here::here("models", "logit_qol_mixture.stan"))

post <- rstan::sampling(model, data = ds, pars = c("p0", "mu", "sigma"), 
                        init = \() { list(mu = stats$mu, sigma = stats$std) },
                        iter = 1e4, warmup = 9000)

post
