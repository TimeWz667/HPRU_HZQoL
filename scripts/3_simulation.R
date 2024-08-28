library(tidyverse)
library(tidybayes)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))

raw %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = study))


raw %>% 
  mutate(
    Agp = cut(age, c(0, 50, 60, 70, 80, 101)),
    y = EQ5D,
    y = (y - min(y)) / (max(y) - min(y)),
  ) %>% 
  filter(y < 1) %>% 
  mutate(
    y = (log(1 - y) - (log(1 - 0.6))) / time_points
  ) %>% 
  ggplot() +
  geom_point(aes(x = time_points, y = y, colour = study)) +
  facet_grid(Agp ~ .)

raw %>% filter(Patient.ID == "Scott 50")


plot(1:100, 0.4 + (0.6 - exp(-0.02 * 1:100)))


library(deSolve)


ys = ode(0.5, seq(0, 100), \(t, x, pars) list(0.1 * (1 - x)))

plot(ys)
points(0:100, 1 - (1 - 0.5) * exp(- 0.1 * 0:100))


raw %>% 
  mutate(
    y = EQ5D,
    y = (y - min(y)) / (max(y) - min(y)),
    tgp = cut(age, c(0:5 * 10, 200, 400))
  ) %>% 
  filter(y < 1) %>% 
  ggplot() +
  stat_halfeye(aes(x = tgp, y = y))
  

raw %>% 
  group_by(study) %>% 
  summarise(
    n = length(unique(Patient.ID)),
    Lower = min(EQ5D),
    Upper = max(EQ5D)
  )


raw %>% 
  mutate(
    y = EQ5D,
    y = (y - min(y)) / (max(y) - min(y)),
    tgp = cut(time_points, c(0:5 * 10, 200, 400))
  ) %>% 
  group_by(tgp, study) %>% 
  summarise(
    p = mean(y == 1)
  ) %>% 
  arrange(study) %>% 
  ggplot() +
  geom_point(aes(x = tgp, y = p, colour = study))


