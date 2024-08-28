library(tidyverse)
library(boot)

theme_set(theme_bw())



raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


max_qol <- 1
min_qol <- min(raw$EQ5D)

dat_qol <- raw %>% 
  mutate(
    qol = (EQ5D - min_qol) / (max_qol - min_qol)
  ) %>% 
  rename(ti = time_points) %>% 
  mutate(ti = ti / 365) %>% 
  filter(qol > 0) %>% 
  filter(qol < 1) %>% 
  select(ti, qol, age) 



fit <- lm(y ~ poly(age, 2), data = dat_qol %>% mutate(y = log(qol / (1 - qol))))


data.pred <- crossing(ID = 1:3000, age = 20:100)


pred0 <- predict(fit, data.pred, se = T)

pred <- data.pred %>% 
  mutate(
    qol = pred0$fit,
    se = pred0$se.fit,
    qol = rnorm(n(), qol, se),
    qol = 1 / (1 + exp(-qol)),
    qol = qol * (1 - min_qol) + min_qol
  ) %>% 
  select(-se)


pred %>% 
  group_by(age) %>% 
  summarise(
    M = mean(qol),
    L = quantile(qol, 0.025),
    U = quantile(qol, 0.975)
  ) %>% 
  ggplot(aes(x = age)) +
  geom_ribbon(aes(ymin = L, ymax = U), alpha = 0.2) +
  geom_line(aes(y = M))


write_csv(pred, file = here::here("out", "post_qol_(a)_lm.csv"))




