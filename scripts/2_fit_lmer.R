library(tidyverse)
library(lme4)
library(lmeresampler)


# The model is based on the outdated version from AC's work
# There are potential issues behind the methods
# 1. The methods pre-processed the data into range of 0.3, 0.7
# 2. The model form is non-identifiable


## Load data
load(here::here("data", "qol_reformed.rdata"))
head(reformed)

max_qol <- 1
min_qol <- min(reformed$EQ5D)


dat <- reformed %>% 
  mutate(
    Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol) * (0.7 - 0.3) + 0.3,
    Y = log(Q_rescaled / (1 - Q_rescaled))
  )


# dat <- dat %>% filter(y0 != max(y0))


m_at <- lmer(Y ~ poly(ti, 2, raw = T) + poly(Age, 2, raw = T) + (1 | PID:SID) + (1 | SID), data = dat)
AIC(m_at)
summary(m_at)

m_t <- lmer(Y ~ poly(ti, 2, raw = T) + (1 | PID:SID) + (1 | SID), data = dat)
AIC(m_t)
summary(m_t)

m_a <- lmer(Y ~ poly(Age, 2, raw = T) + (1 | PID:SID) + (1 | SID), data = dat)
AIC(m_a)
summary(m_a)

m_0 <- lmer(Y ~ (1 | PID:SID) + (1 | SID), data = dat)
AIC(m_0)
summary(m_0)


cat("Model(a, t)", AIC(m_at))
cat("Model(t)", AIC(m_t))
cat("Model(a)", AIC(m_a))
cat("Model(.)", AIC(m_0))

samples <- bootstrap(m_at, .f = fixef, type = "parametric", B = 2000)
samples <- samples$replicates

head(samples)

names(samples) <- c("b0", "b_t1", "b_t2", "b_a1", "b_a2")
head(samples)

write_csv(samples, here::here("posteriors", "boot_lmer_at.csv"))

