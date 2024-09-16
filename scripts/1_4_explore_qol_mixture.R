library(tidyverse)
library(rstan)

theme_set(theme_bw())


source(here::here("scripts", "fn_stan.R"))

n_iter <- 100
n_collect <- 50
n_warmup <- floor(n_iter - n_collect)
n_chains <- 4


## Load data
load(here::here("data", "qol_reformed.rdata"))
head(reformed)

min_qol <- min(reformed$EQ5D)


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
  group_by(Tgp) %>% 
  summarise(Cluster = mean(Cluster) - 1) %>% 
  ggplot() +
  geom_point(aes(x = Tgp, y = Cluster))

