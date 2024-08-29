library(tidyverse)
library(lme4)
library(lmeresampler)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))

max_qol <- 1
min_qol <- min(raw$EQ5D) #- 0.001

dat <- raw %>% 
  mutate(
    qol = (EQ5D - min_qol) / (max_qol - min_qol),
    y0 = log(qol / (1 - qol)),
    time_points = time_points / 365
  ) %>% 
  rename(ti = time_points)

dat <- dat %>% filter(qol < 1) %>% filter(qol > 0)

# dat <- dat %>% filter(y0 != max(y0))


m_at <- lmer(y0 ~ ti + poly(age, 2) + (1 | Patient.ID), data = dat)
AIC(m_at)
summary(m_at)

m_t <- lmer(y0 ~ ti + (1 | Patient.ID)+ (1 | study), data = dat)
AIC(m_t)
summary(m_t)

m_a <- lmer(y0 ~ poly(age, 2) + (1 | Patient.ID)+ (1 | study), data = dat)
AIC(m_a)
summary(m_a)

m_0 <- lmer(y0 ~ (1 | Patient.ID)+ (1 | study), data = dat)
AIC(m_0)
summary(m_0)

cat(AIC(m_at), AIC(m_t), AIC(m_a), AIC(m_0))


samples <- bootstrap(m_at, .f = fixef, type = "parametric", B = 100)
samples <- samples$replicates

head(samples)

names(samples) <- c("k", "b_t", "b_a1", "b_a2")

head(samples)
write_csv(samples, here::here("results", "fit_qol_lmer(a,t).csv"))





samples %>% 
  mutate(
    Key = 1:n(),
    age0 = 70
  ) %>% 
  crossing(ti = seq(0, 1, 0.01)) %>% 
  mutate(
    qol = k + b_a1 * age0 + b_a2 * age0 ^ 2 + b_t * ti,
    qol = 1 / (1 + exp(-qol)) * (1 - min_qol) + min_qol
  ) %>% 
  ggplot() +
  stat_lineribbon(aes(x = ti, y = qol))


samples %>% 
  mutate(
    Key = 1:n(),
    age0 = 0
  ) %>% 
  crossing(ti = seq(0, 1, 0.01)) %>% 
  mutate(
    qol = k + b_a1 * age0 + b_a2 * age0 ^ 2 + b_t * ti,
    qol = 1 / (1 + exp(-qol)) * (1 - min_qol) + min_qol
  ) %>% 
  ggplot() +
  geom_line(aes(x = ti, y = qol, group = Key))


hist(fitted(m_at))
x = fitted(m_at)
x = 1 / (1 + exp(-x)) * (1 - min_qol) + min_qol
hist(x, breaks = 50)


