library(targets)
library(tidyverse)
library(rstan)
library(lme4)


dat_qol <- tar_read(subdata_qol, 1)
save(dat_qol, file = here::here("data", "sub_qol.rdata"))

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
ds_zero <- training %>% 
  mutate(
    cluster = (EQ5D >= 1) + 0,
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  )

ds_clu1 <- ds_nz %>% 
  mutate(
    cluster = (cluster == 1) + 0,
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  )


# mild discomfort
# fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c1); AIC(fit)
fit <- lmer(EQ5D ~ ti + (1 | PID) + (1 | SID), data = ds_c1); print(AIC(fit))
fit <- lmer(EQ5D ~      (1 | PID) + (1 | SID), data = ds_c1); print(AIC(fit))
fit <- lmer(EQ5D ~ ti             + (1 | SID), data = ds_c1); print(AIC(fit))
fit <- lm(  EQ5D ~ ti,                         data = ds_c1); print(AIC(fit))

fit <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c1); print(AIC(fit))
fit <- lmer(EQ5D ~ 1 +            + (ti | SID), data = ds_c1); print(AIC(fit))
fit <- lm(  EQ5D ~ 1,                         data = ds_c1); print(AIC(fit))

#### Adding Age
fit <- lmer(EQ5D ~            1 + (ti | PID) + (ti | SID), data = ds_c1); print(AIC(fit))
fit <- lmer(EQ5D ~          Age + (ti | PID) + (ti | SID), data = ds_c1); print(AIC(fit))
fit <- lmer(EQ5D ~ poly(Age, 2) + (ti | PID) + (ti | SID), data = ds_c1); print(AIC(fit))



## Final
fit_c1 <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c1); AIC(fit)
plot(resid(fit_c1))

ds_c1 %>% 
  mutate(
    pred = predict(fit_c1, .)
  ) %>% 
  ggplot()  +
  geom_histogram(aes(x = pred, fill = "Sim"), alpha = 0.6, bins = 30) +
  geom_histogram(aes(x = EQ5D, fill = "Data"), alpha = 0.6, bins = 30)


# Severely discomfort
fit <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ ti + (1 | PID) + (ti | SID), data = ds_c2); AIC(fit)
fit <- lm(EQ5D ~ ti, data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ Age + (1 | PID) + (1 | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ Age + (1 | PID), data = ds_c2); AIC(fit)
fit <- lm(EQ5D ~ Age, data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ (1 | PID) + (1 | SID), data = ds_c2); AIC(fit)
fit <- lmer(EQ5D ~ (1 | PID), data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ ti + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)

fit <- lmer(EQ5D ~ ti + (1 | PID) + (1 | SID), data = ds_c2)
AIC(fit)
plot(resid(fit))

fit <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c2)
AIC(fit)
plot(resid(fit))


## Final
fit_c2 <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c2); AIC(fit)
summary(fit_c2)

ds_c2 %>% 
  mutate(
    pred = predict(fit_c2, .)
  ) %>% 
  ggplot() +
  geom_density(aes(x = EQ5D, colour = "Data")) +
  geom_density(aes(x = pred, colour = "Sim"))


# prob(zero)

## unidentifiable
fit <- glmer(cluster ~ ti + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
## unidentifiable
fit <- glmer(cluster ~ 1 + (ti | PID) + (ti | SID), data = ds_zero, family = binomial); AIC(fit)


fit <- glmer(cluster ~ ti + (1 | SID), data = ds_zero, family = binomial); AIC(fit) #*
fit <- glmer(cluster ~ 1 + (ti | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glm(cluster ~ ti, data = ds_zero, family = binomial); AIC(fit)
fit <- glm(cluster ~ 1, data = ds_zero, family = binomial); AIC(fit)


fit <- glmer(cluster ~ ti + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_zero, family = binomial); AIC(fit) #*1858
fit <- glmer(cluster ~ d15 + d30 + d45 + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + d45 + d60 + (1 | SID), data = ds_zero, family = binomial); AIC(fit)

fit <- glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + Age + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + poly(Age, 2) + (1 | SID), data = ds_zero, family = binomial); AIC(fit) #*1856


## unidentifiable
# fit <- glmer(cluster ~ Age + (ti | PID) + (ti | SID), data = ds_zero, family = binomial); AIC(fit)
# 
# fit <- glmer(cluster ~ d15 + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# fit <- glmer(cluster ~ d15 + d30 + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# fit <- glmer(cluster ~ d15 + d30 + d45 + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# fit <- glmer(cluster ~ d15 + d30 + d45 + d60 + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# 
# fit <- glmer(cluster ~ d15 + d30 + d60 + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# 
# 
# fit <- glmer(cluster ~ Age + (ti | PID) + (ti | SID), data = ds_zero, family = binomial); AIC(fit)
# fit <- glmer(cluster ~ poly(Age, 2) + (ti | PID) + (ti | SID), data = ds_zero, family = binomial); AIC(fit)
# 
# fit <- glmer(cluster ~ poly(Age, 2) + (1 | PID) + (1 | SID), data = ds_zero, family = binomial); AIC(fit)
# 
# fit <- glmer(cluster ~ d15 + d30 + d60 + Age + (ti | PID) + (ti | SID), data = ds_zero, family = binomial); AIC(fit)

## Best
fit_zero <- glmer(cluster ~ d15 + d30 + Age + (1 | SID), data = ds_zero, family = binomial); AIC(fit_zero)
summary(fit_zero)





tibble(Age = 70, ti = seq(0, 1, 0.02)) %>% 
  mutate(
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25),
    d45 = (ti < 45 / 365.25),
    d60 = (ti < 60 / 365.25)
  ) %>% 
  mutate(
    prob = predict(fit_zero, re.form = NA, newdata = ., type = "response")
  ) %>% 
  ggplot() +
  geom_line(aes(y = prob, x = ti, colour = as.character(Age))) +
  geom_line(data = ds_zero %>% 
              mutate(
                tig = cut(ti * 365.25, seq(0, 365, 15), right = F)
              ) %>% 
              group_by(tig) %>% 
              summarise(prop = mean(cluster)) %>% 
              mutate(
                ti = seq(0, 300 - 15, 15) / 365.25
              ), 
            aes(x = ti, y = prop, colour = "Data")) +
  expand_limits(y = 0:1)



# prob(mild|non zero)
fit <- glmer(cluster ~ ti + (1 | PID) + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ 1 + (ti | PID) + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)

fit <- glmer(cluster ~ ti + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ 1 + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)

fit <- glm(cluster ~ ti, data = ds_clu1, family = binomial); AIC(fit)
fit <- glm(cluster ~ 1, data = ds_clu1, family = binomial); AIC(fit)

# unidentifiable
fit <- glmer(cluster ~ Age + (ti | PID) + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)


fit <- glmer(cluster ~ d15 + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + d45 + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ d15 + d30 + d45 + d60  + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)


fit <- glmer(cluster ~ Age + (ti | SID), data = ds_clu1, family = binomial); AIC(fit) #x
fit <- glmer(cluster ~ poly(Age, 2) + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)
fit <- glmer(cluster ~ poly(Age, 2) + (1 | SID), data = ds_clu1, family = binomial); AIC(fit)

fit <- glmer(cluster ~ d15 + d30 + d60 + Age + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)#x
fit <- glmer(cluster ~ d15 + d30 + poly(Age, 2) + (ti | SID), data = ds_clu1, family = binomial); AIC(fit)#x
summary(fit)

# best fit
fit_clu1 <- glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_clu1, family = binomial)
summary(fit_clu1)

tibble(Age = 70, ti = seq(0, 1, 0.02)) %>% 
  mutate(
    d15 = (ti < 15 / 365.25),
    d30 = (ti < 30 / 365.25)
  ) %>% 
  mutate(
    prob = predict(fit_pc1, re.form = NA, newdata = ., type = "response")
  ) %>% 
  ggplot() +
  geom_line(aes(y = prob, x = ti, colour = as.character(Age))) +
  geom_line(data = ds_clu1 %>% 
              mutate(
                tig = cut(ti * 365.25, seq(0, 365, 15), right = F)
              ) %>% 
              group_by(tig) %>% 
              summarise(prop = mean(cluster)) %>% 
              mutate(
                ti = seq(0, 300 - 30, 15) / 365.25
              ), 
            aes(x = ti, y = prop, colour = "Data")) +
  expand_limits(y = 0:1)






## final model
ms_fin <- list()
ms_fin$c1 <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c1)
ms_fin$c2 <- lmer(EQ5D ~ 1 + (ti | PID) + (ti | SID), data = ds_c2)
ms_fin$pz <- glmer(cluster ~ d15 + d30 + poly(Age, 2) + (1 | SID), data = ds_zero, family = binomial)
ms_fin$pc1 <-  glmer(cluster ~ d15 + d30 + poly(Age, 2) + (ti | SID), data = ds_clu1, family = binomial)
summary(fit)

## reduced model
ms_red <- ms_fin
ms_red$pz <- glm(cluster ~ 1, data = ds_zero, family = binomial)
ms_red$pc1 <-  glm(cluster ~ 1, data = ds_clu1, family = binomial)



ms_fin %>% lapply(AIC) %>% unlist() %>% sum
ms_red %>% lapply(AIC) %>% unlist() %>% sum


gen_pred <- \(ms) {
  crossing(Age = c(60, 70, 80, 90), ti = seq(0, 1, 1 / 100)) %>% 
    mutate(
      d15 = (ti < 15 / 365.25),
      d30 = (ti < 30 / 365.25)
    ) %>% 
    mutate(
      p_z = 1 - predict(ms$pz, ., re.form = NA, type = "response"),
      p_c1 = predict(ms$pc1, ., re.form = NA, type = "response") * (1 - p_z),
      p_c2 = 1 - p_z - p_c1,
      q_c1 = predict(ms$c1, ., re.form = NA),
      q_c2 = predict(ms$c2, ., re.form = NA),
      avg = p_c1 * q_c1 + p_c2 * q_c2
    )
}



pred_bind <- bind_rows(
  ms_fin %>% gen_pred %>% mutate(Model = "Final"),
  ms_red %>% gen_pred %>% mutate(Model = "Reduced"),
)

pred_bind %>% 
  filter(ti < 0.5) %>% 
  group_by(Age, Model) %>% 
  summarise(QALY = mean(avg))

pred_bind %>% 
  filter(ti < 0.5) %>% 
  group_by(Age, Model) %>% 
  summarise(QALY = mean(avg))


