library(tidyverse)
library(rstan)

theme_set(theme_bw())

options(mc.cores = 6)
rstan_options(auto_write = TRUE)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


max_qol <- 1
min_qol <- min(raw$EQ5D)

dat <- raw %>% 
  mutate(
    qol = (EQ5D - min_qol) / (max_qol - min_qol)
  ) %>% 
  rename(ti = time_points)


dat_qol <- dat %>% 
  mutate(ti = ti / 365) %>% 
  filter(qol > 0) %>% 
  filter(qol < 1) %>% 
  select(ti, qol, age, subject = Patient.ID)


## QOL as a function of age
model <-  stan_model(here::here("models", "logit_qol_a.stan"))

ds <- dat_qol %>%
  #head(1000) %>% 
  (\(df) {
    list(
      N = nrow(df),
      As = df$age,
      Ys = df$qol
    )
  })

post <- sampling(model, data = ds, pars = c("b0", "b1", "b2"), 
                 chains = 3, iter = 10000, warmup = 9000)

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("results", "post_qol_(a).csv"))


## QOL as a function of age and time
model <-  stan_model(here::here("models", "logit_qol_at.stan"))

ds <- dat_qol %>%
  (\(df) {
    list(
      N = nrow(df),
      As = df$age,
      Ts = df$ti,
      Ys = df$qol
    )
  })

post <- sampling(model, data = ds, pars = c("b0", "b1", "b2", "bt"), 
                 chains = 3, iter = 15000, warmup = 14000)

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("results", "post_qol_(a,t).csv"))



## QOL as a function of age and time; random intercept for each subject
model <-  stan_model(here::here("models", "logit_qol_at_subject.stan"))

ds <- dat_qol %>%
  head(200) %>% 
  (\(df) {
    ids <- as.numeric(as.factor(df$subject)) 
    
    list(
      N = nrow(df),
      M = length(unique(ids)),
      As = df$age,
      Ts = df$ti,
      Ys = df$qol,
      IDs = ids
    )
  })

post <- sampling(model, data = ds, pars = c("b0", "b1", "b2", "bt"), 
                 chains = 3, iter = 15000, warmup = 14000)

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("results", "post_qol_(a,t|subject).csv"))



