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



# prob(severe)

ds_clu <- ds_nz %>% mutate(
    cluster = cluster - 1,
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  )

glmer(cluster ~ ti + (1 | PID) + (1 | SID), data = ds_clu, family = binomial)

glmer(cluster ~ 1 + (ti | PID) + (ti | SID), data = ds_clu, family = binomial)

glmer(cluster ~ Age + (1 | PID) + (1 | SID), data = ds_clu, family = binomial)

fit <- glmer(cluster ~ 1 + (ti | PID) + (ti | SID), data = ds_clu, family = binomial)
summary(fit)

fit <- glmer(cluster ~ d15 + d30 + d60 + Age + (1 | SID), data = ds_clu, family = binomial)
summary(fit)



nd <- crossing(Age = c(50, 60, 70, 80), ti = seq(0, 1, 0.02)) %>% 
  mutate(
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  ) %>% 
  mutate(
    prob = predict(fit, re.form = NA, newdata = ., type = "response")
  )


nd %>% 
  ggplot() +
  geom_line(aes(y = prob, x = ti, colour = as.character(Age))) +
  expand_limits(y = 0:1)


plot(predict(fit, re.form = NA, newdata = nd, type = "response"), ylim = 0:1)
fit


ds_clu %>% 
  ggplot() +
  geom_density(aes(x = ti, fill = as.character(cluster)), alpha = 0.3)




# prob(zero)
ds_zero <- training %>% 
  mutate(
    cluster = (EQ5D < 1),
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  )

fit <- glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_zero, family = binomial)
summary(fit)


nd <- tibble(Age = 70, ti = seq(0, 1, 0.02)) %>% 
  mutate(
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  ) %>% 
  mutate(
    prob = predict(fit, re.form = NA, newdata = ., type = "response")
  )


nd %>% 
  ggplot() +
  geom_line(aes(y = prob, x = ti, colour = as.character(Age))) +
  expand_limits(y = 0:1)


