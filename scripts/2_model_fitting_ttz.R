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


## Time to recovery

dat_ttr <- dat %>% 
  mutate(Rec = (qol >= 1) + 0, ti = ti / 365) %>% 
  filter(ti > 0) %>% 
  filter(qol > 0) %>% 
  select(ti, Rec, age) %>% 
  mutate(
    Agp = cut(age, c(0, 30, seq(35, 90, 5), 100), right = F)
  )


model <-  stan_model(here::here("models", "time2zero_age.stan"))

ds <- dat_ttr %>%
  #head(2000) %>% 
  filter(ti > 0) %>%
  (\(df) {
    list(
      N = nrow(df),
      Ts = df$ti,
      Ys = df$Rec,
      As = df$age
    )
  })


post <- sampling(model, data = ds, pars = c("b0", "b1", "rate_age"), 
                 chains = 3, iter = 5000, warmup = floor(5000 - 1000))


ext <- data.frame(extract(post))

ext %>% 
  write_csv(file = here::here("out", "post_ttz_(a).csv"))


ext %>% 
  select(b0, b1) %>% mutate(Key = 1:n()) %>% 
  relocate(Key) %>% 
  write_csv(here::here("results", "post_ttz_(a).csv"))


samples <- data.frame(extract(post, pars = "rate_age")) %>% 
  mutate(Key = 1:n()) %>% 
  pivot_longer(-Key, values_to = "Rate") %>% 
  tidyr::extract(name, "Age", "rate_age.(\\d+)", convert = T) 

samples %>%
  filter(Key <= 1000) %>% 
  write_csv(here::here("docs", "tabs", "samples_ttz(t).csv"))


g_fit <- as_tibble(summary(post)$summary[-c(1:2), ]) %>% 
  mutate(a = 1:101) %>%
  filter(a <= 100) %>% 
  ggplot() +
  geom_ribbon(aes(x = a, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
  scale_y_continuous("Rate to recovery, per year", limits = c(0, 10)) +
  scale_x_continuous("Age", breaks = c(0, seq(40, 100, 10))) +
  geom_line(aes(x = a, y = mean))


g_fit

ggsave(g_fit, filename = here::here("docs", "figs", "g_fit_ttz(t).png"), width = 7.5, height = 4)


