library(tidyverse)
library(rstan)

theme_set(theme_bw())

## Setting
options(mc.cores = 6)
rstan_options(auto_write = TRUE)

source(here::here("scripts", "fn_stan.R"))

n_iter <- 10000
n_collect <- 500
n_warmup <- floor(n_iter - n_collect)
n_chains <- 4


## Load data
load(here::here("data", "qol_reformed.rdata"))

reformed <- reformed %>% filter(Q_rescaled > 0)

head(reformed)

min_qol <- min(reformed$EQ5D)


set.seed(11667)

## QOL as a function of age
model_src <- "logit_qol_a"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))



ds <- local({
  sel <- reformed  %>%
    filter(Health == 0) %>%
    mutate(Key = 1:n(), Key = sample(Key, n())) %>%
    filter(Key <= 2000) %>%
    filter(ti > 0) %>% 
    select(Ts = ti, As = Age, Ys = Q_rescaled) %>% 
    as.list()
  
  sel$N <- length(sel$Ys)
  sel
})


post <- sampling(model, data = ds, pars = c("b0", "ba1", "ba2", "sigma"), 
                 chains = n_chains, iter = n_iter, warmup = n_warmup)


save(post, ds, file = here::here("out", "post_" + model_src + ".rdata"))

res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))


## QOL as a function of age and time
model_src <- "logit_qol_at"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))


ds <- local({
  sel <- reformed  %>%
    filter(Health == 0) %>% 
    mutate(Key = 1:n(), Key = sample(Key, n())) %>%
    filter(Key <= 1000) %>%
    filter(ti > 0) %>% 
    select(Ts = ti, As = Age, Ys = Q_rescaled) %>% 
    as.list()
  
  sel$N <- length(sel$Ys)
  sel
})


post <- sampling(model, data = ds, pars = c("b0", "ba1", "ba2", "bt", "sigma"), 
                 chains = n_chains, iter = n_iter, warmup = n_warmup)

save(post, ds, file = here::here("out", "post_" + model_src + ".rdata"))

res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))



## QOL as a function of age and time; random intercept for each subject
# model_src <- "logit_qol_at"
# model_src <- glue::as_glue(model_src)
# 
# model <- stan_model(here::here("models", model_src + ".stan"))
# 
# 
# model <-  stan_model(here::here("models", "logit_qol_at_subject.stan"))
# 
# ds <- dat_qol %>%
#   (\(df) {
#     ids <- as.numeric(as.factor(df$subject)) 
#     
#     list(
#       N = nrow(df),
#       M = length(unique(ids)),
#       As = df$age,
#       Ts = df$ti,
#       Ys = df$qol,
#       IDs = ids
#     )
#   })
# 
# post <- sampling(model, data = ds, pars = c("b0", "b1", "b2", "bt"), 
#                  chains = 3, iter = 15000, warmup = 14000)
# 
# ext <- data.frame(extract(post))
# write_csv(ext, file = here::here("results", "post_qol_(a,t|subject).csv"))



