library(tidyverse)
library(lme4)
library(splines)
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



# dat <- dat %>% filter(y0 != max(y0))


fn <- function(lower = 0.3, upper  = 0.7) {
  dat <- reformed %>% 
    mutate(
      Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol) * (upper - lower) + lower,
      Y = log(Q_rescaled / (1 - Q_rescaled))
    )
  
  m_at <- lmer(Y ~ Age * ns(ti, df = 4) + (1 + ti | PID:SID) + (1 + ti| SID), data = dat)
  
  newdata <- crossing(Age = 50:90, ti = seq(0, 1, 0.01))
  
  newdata %>% 
    mutate(
      pred = predict(m_at, newdata=., re.form=~0),
      pred = 1 / (1 + exp(-pred)),
      pred = pmax(pred, lower),
      pred = pmin(pred, upper),
      pred = (pred - lower) / (upper - lower) * (1 - min_qol) + min_qol
    ) %>% 
    group_by(Age) %>% 
    summarise(QL = mean(1 - pred)) %>% 
    mutate(lower = lower, upper = upper)
}

sims <- bind_rows(
  fn(0.01, 0.7),
  fn(0.1, 0.7),
  fn(0.2, 0.7),
  fn(0.3, 0.7),
  fn(0.01, 0.8),
  fn(0.1, 0.8),
  fn(0.2, 0.8),
  fn(0.3, 0.8),
  fn(0.01, 0.9),
  fn(0.1, 0.9),
  fn(0.2, 0.9),
  fn(0.3, 0.9),
  fn(0.01, 0.999),
  fn(0.1, 0.999),
  fn(0.2, 0.999),
  fn(0.3, 0.99),
) 


x <- crossing(
  lower = c(0.0001, 0.001, 0.01, 0.01, 0.1, 0.2, 0.3),
  upper = rev(1 - c(0.0001, 0.001, 0.01, 0.01, 0.1, 0.2, 0.3))
)

sims <- bind_rows(lapply(1:nrow(x), \(i) {
  fn(x[i, ]$lower, x[i, ]$upper)
}))


sims %>% 
  mutate(upper = as.character(upper)) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = QL, colour = upper)) +
  facet_wrap(lower~., labeller = label_both) +
  expand_limits(y = c(0, 0.2))




m_t <- lmer(Y ~ ti + (1 | PID:SID) + (1 | SID), data = dat)
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

names(samples) <- c("b0", "b_t", "b_a1", "b_a2", "b_at1", "b_at2")
head(samples)

write_csv(samples, here::here("posteriors", "boot_lmer_at.csv"))




reformed %>% 
  filter(ti < 30 / 365) %>% 
  group_by(Age) %>% 
  summarise(EQ5D = mean(EQ5D)) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = EQ5D))


