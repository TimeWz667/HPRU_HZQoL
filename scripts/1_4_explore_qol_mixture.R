library(tidyverse)
library(rstan)

theme_set(theme_bw())


## Load data
load(here::here("data", "qol_reformed.rdata"))
head(reformed)

min_qol <- min(reformed$EQ5D)

d <- reformed %>% 
  filter(Health == 0) %>% 
  filter(Q_rescaled > 0)

qs <- d$Q_rescaled

km <- kmeans(log(qs / (1 - qs)), 2)

km$centers

clu <- reformed %>% 
  filter(Health == 0) %>% 
  filter(Q_rescaled > 0) %>% 
  mutate(
    Tgp = cut(ti, (0:12) / 12),
    Cluster = kmeans(log(Q_rescaled / (1 - Q_rescaled)), 2)$cluster
  )


clu %>% 
  ggplot() +
  geom_point(aes(x = ti, y = Q_rescaled, colour = Cluster)) 


clu %>% 
  group_by(Agp) %>% 
  summarise(Cluster = mean(Cluster) - 1) %>% 
  ggplot() +
  geom_point(aes(x = Agp, y = Cluster))


clu %>% 
  filter(Age >= 50) %>% 
  group_by(Age) %>% 
  summarise(Cluster = mean(Cluster) - 1) %>%
  mutate(
    Cluster = log(Cluster / (1 - Cluster))
  ) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = Cluster))


clu %>% 
  group_by(Tgp) %>% 
  summarise(Cluster = mean(Cluster) - 1) %>% 
  ggplot() +
  geom_point(aes(x = Tgp, y = Cluster))

