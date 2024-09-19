library(tidyverse)
library(rstan)

theme_set(theme_bw())

options(mc.cores = 6)
rstan_options(auto_write = TRUE)

source(here::here("scripts", "fn_stan.R"))

n_iter <- 10000
n_collect <- 500
n_warmup <- floor(n_iter - n_collect)
n_chains <- 2


## Load data
load(here::here("data", "qol_reformed.rdata"))

reformed <- reformed %>% filter(Q_rescaled > 0)

head(reformed)

min_qol <- min(reformed$EQ5D)


## Model fitting by age-groups
model_src <- "logit_qol_mixture"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))


agps <- set_names(levels(reformed$Agp), levels(reformed$Agp))


posts <- bind_rows(lapply(agps, function(agp) {
  ds <- local({
    sel <- reformed %>%
      filter(Agp == agp) %>%
      mutate(Key = 1:n()) %>% 
      filter(Key <= 300) %>%
      filter(Q_rescaled > 0) %>% 
      filter(Health == 0) %>% 
      select(Ys = Q_rescaled) %>% 
      as.list()
    
    sel$N <- length(sel$Ys)
    sel
  })
  
  post <- sampling(model, data = ds, pars = c("p0", "mu", "sigma"), 
                   init = \() list(mu = c(1.47, -0.23), sigma = c(0.1, 0.1)),
                   chains = 1, iter = 1500, warmup = 1000)
  
  save(post, ds, file = here::here("out", "post_" + model_src + "_" + agp + ".rdata"))
  
  su <- restructure_stan(post)$Summary %>% mutate(Agp = agp)
  su
})) %>% 
  relocate(Agp, Var)


rownames(posts) <- NULL

posts %>% arrange(Var, Agp)

posts %>% 
  filter(startsWith(Var, "mu"))

write_csv(posts, file = here::here("docs", "tabs", "fit_" + model_src + "_agp.csv"))

posts %>% 
  ggplot() +
  geom_pointrange(aes(x = Agp, y = mean, ymin = X25., ymax = X75.)) +
  facet_wrap(.~Var, scales = "free_y") +
  expand_limits(y = 0)


## Model fitting with age as a covariate

model_src <- "logit_cap_qol_at"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))


ds <- local({
  sel <- reformed %>%
    mutate(Key = 1:n(), Key = sample(Key, n())) %>% 
    filter(Healht == 0) %>% 
    filter(Q_rescaled > 0) %>% 
    filter(Key <= 1500) %>% 
    select(Ys = Q_rescaled, As = Age) %>% 
    as.list()
  
  sel$N <- length(sel$Qs)
  sel
})


post <- sampling(model, data = ds, pars = c("b0", "ba1", "ba2", "bt", "bta1", "bta2", "sigma"), 
                 init = \() list(b0 = 1, ba1 = 0, ba2 = 0, bt = 1, bta1 = 0, bta2 = 0, sigma = 0.5),
                 chains = n_chains, iter = n_iter, warmup = n_warmup)


post

save(post, ds, file = here::here("out", "post_" + model_src + ".rdata"))


res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))


