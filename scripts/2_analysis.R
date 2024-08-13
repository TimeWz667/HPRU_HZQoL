library(tidyverse)
library(lme4)
library(splines)


scores <- 0.5 * (log(1 - y) - (log(1 - 0.6))) / time_points


tibble(time = 0:100) %>% 
  mutate(
    scores = 1 - (1 - 0.4) * exp(-0.02 * time)
  ) %>% 
  ggplot() +
  geom_line(aes(x = time, y = scores)) +
  expand_limits(y = 0:1)

raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


dat <- raw %>% 
  mutate(
    y0 =  (EQ5D - min(EQ5D)) / (max(EQ5D) - min(EQ5D)) * (0.7 - 0.3) + 0.3,
    y0 = log(y0 / (1 - y0))
  )


raw


m <- lm(y0 ~ time_points, data = dat)
AIC(m)


m <- lm(y0 ~ time_points + age, data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 2), data = dat)
AIC(m)


m <- lm(y0 ~ time_points + poly(age, 2), data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 2) + age, data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 3), data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 3) + age, data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 4), data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 4) + age, data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 5), data = dat)
AIC(m)


m <- lm(y0 ~ poly(time_points, 5) + age, data = dat)
AIC(m)


m <- lm(y0 ~ ns(time_points, df = 3), data = dat)
AIC(m)

m <- lm(y0 ~ ns(time_points, df = 3) + age, data = dat)
AIC(m)


m <- lm(y0 ~ ns(time_points, df = 4), data = dat)
AIC(m)

m <- lm(y0 ~ ns(time_points, df = 4) + age, data = dat)
AIC(m)


m <- lm(y0 ~ ns(time_points, df = 9), data = dat)
AIC(m)

m <- lm(y0 ~ ns(time_points, df = 9) + age, data = dat)
AIC(m)


m <- lmer(y0 ~ time_points + age + (1 | Patient.ID), data = dat)
AIC(m)


m <- lmer(y0 ~ ns(time_points, 4) * age + (time_points | Patient.ID) + (time_points | study), data = dat)
AIC(m); summary(m)$sigma


newdata <- dat %>% 
  select(study, Patient.ID) %>% 
  crossing(data.frame(time_points = seq(0, 120, 60), age = 60)) %>% 
  data.frame()



simulate(m, nsim = 5, re.form = NA)

newdata <- data.frame(time_points = seq(0, 120, 60), age = 60)

pfun <- function(fit) {
  predict(fit, newdata = newdata, re.form = NULL)
}


cc <- confint(m, method="boot", FUN=pfun, nsim = 100)

as_tibble(cc) %>% 
  rename(
    L = `2.5 %`,
    U = `97.5 %`
  ) %>% 
  mutate(
    Time = 1:n(),
    L = 1 / (1 + exp(-L)),
    U = 1 / (1 + exp(-U))
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Time, ymin = L, ymax = U))

lme4::bootMer(mem, FUN=predfn, nsim=250, re.form=NULL, type="parametric")


m <- lmer(y0 ~ age + (time_points | Patient.ID) + (time_points | study), data = dat)
AIC(m); summary(m)$sigma


m <- lmer(y0 ~ ns(time_points, 4) * age + (time_points | Patient.ID:study), data = dat, REML = F)
AIC(m); summary(m)$sigma


m <- lmer(y0 ~ ns(time_points, 4) * age + (1 + time_points | Patient.ID:study), data = dat, REML = F)
AIC(m); summary(m)$sigma






simulate(m, 4)

m <- lmer(y0 ~ age * ns(time_points, 4)  + (1 | time_points) + (1 | age), data = dat)
m
AIC(m)


