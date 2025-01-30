library(targets)
library(tidyverse)
library(rstan)
library(lme4)


dat_qol <- tar_read(subdata_qol, 1)

## Frequentist approach

training <- dat_qol$training 

ds_nz <- training %>% filter(EQ5D < 1)


hist(ds_nz$EQ5D)

clu <- kmeans(ds_nz$EQ5D, 2)

if (clu$centers[1] > clu$centers[2]) {
  ds_nz <- ds_nz %>% 
    mutate(
      cluster = clu$cluster
    )
} else {
  ds_nz <- ds_nz %>% 
    mutate(
      cluster = 3 - clu$cluster
    )
}


ds_c1 <- ds_nz %>% filter(cluster == 1)
ds_c2 <- ds_nz %>% filter(cluster == 2)


fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c1); AIC(fit)
fit <- lmer(EQ5D ~ ti + (ti | PID), data = ds_c1); AIC(fit)
fit <- lm(EQ5D ~ ti, data = ds_c1); AIC(fit)

fit <- lmer(EQ5D ~ Age + (1 | PID) + (1 | SID), data = ds_c1); AIC(fit)
fit <- lmer(EQ5D ~ Age + (1 | PID), data = ds_c1); AIC(fit)
fit <- lm(EQ5D ~ Age, data = ds_c1); AIC(fit)

fit <- lmer(EQ5D ~ (1 | PID) + (1 | SID), data = ds_c1); AIC(fit)
fit <- lmer(EQ5D ~ (1 | PID), data = ds_c1); AIC(fit)

fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c1); AIC(fit)
fit <- lmer(EQ5D ~ t30 + (1 | PID) + (1 | SID), data = ds_c1 %>% mutate(t30 = (ti > 30) + 0)); AIC(fit)
fit <- lmer(EQ5D ~ poly(ti, 2) + (ti | PID) + (ti | SID), data = ds_c1); AIC(fit)


fit <- lmer(EQ5D ~ ti + (1 | PID) + (1 | SID), data = ds_c1)
AIC(fit)
plot(resid(fit))

fit <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c1)
AIC(fit)
plot(resid(fit))


# Severely discomfort
fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ ti + (ti | PID), data = ds_c2); AIC(fit)
fit <- lm(EQ5D ~ ti, data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ Age + (1 | PID) + (1 | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ Age + (1 | PID), data = ds_c2); AIC(fit)
fit <- lm(EQ5D ~ Age, data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ (1 | PID) + (1 | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ (1 | PID), data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ t30 + (1 | PID) + (1 | SID), data = ds_c2 %>% mutate(t30 = (ti > 30) + 0)); AIC(fit)
fit <- lmer(EQ5D ~ poly(ti, 2) + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)


fit <- lmer(EQ5D ~ ti + (1 | PID) + (1 | SID), data = ds_c2)
AIC(fit)
plot(resid(fit))

fit <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c2)
AIC(fit)
plot(resid(fit))













