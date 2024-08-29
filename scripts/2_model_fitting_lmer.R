library(tidyverse)
library(lme4)
library(lmeresampler)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))

max_qol <- 1
min_qol <- min(raw$EQ5D)


dat <- raw %>% 
  mutate(
    qol = (EQ5D - min_qol) / (max_qol - min_qol), # * (0.9 - 0.1) + 0.1,
    y0 = log(qol / (1 - qol)),
    time_points = time_points / 365
  ) %>% 
  rename(ti = time_points)

dat <- dat %>% filter(qol < 1) %>% filter(qol > 0)

# dat <- dat %>% filter(y0 != max(y0))


m_at <- lmer(y0 ~ poly(ti, 2, raw = T) + poly(age, 2, raw = T) + (1 | Patient.ID:study) + (1 | study), data = dat)
AIC(m_at)
summary(m_at)

m_t <- lmer(y0 ~ poly(ti, 2, raw = T) + (1 | Patient.ID:study) + (1 | study), data = dat)
AIC(m_t)
summary(m_t)

m_a <- lmer(y0 ~ poly(age, 2, raw = T) + (1 | Patient.ID:study) + (1 | study), data = dat)
AIC(m_a)
summary(m_a)

m_0 <- lmer(y0 ~ (1 | Patient.ID:study) + (1 | study), data = dat)
AIC(m_0)
summary(m_0)


cat("Model(a, t)", AIC(m_at))
cat("Model(t)", AIC(m_t))
cat("Model(a)", AIC(m_a))
cat("Model(.)", AIC(m_0))

samples <- bootstrap(m_at, .f = fixef, type = "parametric", B = 3000)
samples <- samples$replicates

head(samples)

names(samples) <- c("k", "b_t1", "b_t2", "b_a1", "b_a2")
head(samples)

write_csv(samples, here::here("results", "fit_qol_lmer(a,t).csv"))

