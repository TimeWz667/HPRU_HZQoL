library(tidyverse)

load(here::here("data", "qol_reformed.rdata"))
head(reformed)



phn <- reformed %>% filter(EQ5D < 1) %>% 
  mutate(
    PHN = ti > 90 / 365
  ) 


phn %>% 
  ggplot() +
  geom_density(aes(x = EQ5D, fill = PHN), alpha = 0.2)


phn %>% 
  group_by(PHN) %>% 
  summarise(mean(EQ5D))


indicators <- reformed %>% 
  group_by(SID, PID) %>% 
  summarise(
    over90 = mean(ti > 90/ 365) > 0,
    pain90 = mean((ti > 90/ 365) * (EQ5D < 1)) > 0
  ) 


reformed %>% filter(EQ5D < 1) %>% 
  left_join(indicators) %>% 
  ggplot() +
  geom_density(aes(x = EQ5D, fill = pain90), alpha = 0.2) 



cluster <- kmeans(reformed %>% mutate(q = log(Q_rescaled / (1 - Q_rescaled))) %>% filter(EQ5D < 1 & Q_rescaled > 0) %>% pull(q), 2)

ref <- reformed %>% 
  filter(EQ5D < 1 & Q_rescaled > 0) %>% 
  mutate(
    q = log(Q_rescaled / (1 - Q_rescaled)),
    cluster = kmeans(Q_rescaled, 2)$cluster
  )



d <- reformed %>%
  left_join(ref) %>%
  group_by(PID) %>% 
  mutate(
    over90 = mean(ti > 90/ 365.25) > 0,
    pain90 = mean((ti > 90/ 365.25) * (EQ5D < 1)) > 0,
    cluster = ifelse(is.na(cluster), 0, cluster),
    cluster = as.character(cluster)
  ) %>% 
  ungroup()


d %>% 
  ggplot() +
  geom_point(aes(x = ti, y = EQ5D, colour = cluster)) +
  geom_vline(xintercept = 90 / 365.25) +
  facet_grid(pain90~.)


d %>% 
  group_by(pain90) %>% 
  summarise(
   c0 = mean(cluster == '0'),
   c1 = mean(cluster == '1'),
   c2 = mean(cluster == '2'),
   p1 = c1 / (1 - c0)
  )





