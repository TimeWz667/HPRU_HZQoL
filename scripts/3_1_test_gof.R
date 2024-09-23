library(tidyverse)

theme_set(theme_bw())




dat <- read_csv(here::here("data", "tte_uk.csv")) %>% 
  filter(T_evt2 > 0)


pars_tte <- read_csv(here::here("posteriors", "post_time2zero_age.csv")) %>% 
  mutate(Key = 1:n())


sims <- dat %>% 
  mutate(
    Key = sample(pars_tte$Key, n(), rep = T)
  ) %>% 
  left_join(pars_tte) %>% 
  mutate(
    rate = (r0 * exp(Age * ba1)),
    T_Sim = rexp(n(), rate)
  )


hist(sims$T_Sim)
hist(sims$T_evt2)

g_tte <- sims %>% 
  group_by(Age) %>% 
  summarise(
    Data = mean(T_evt2),
    Simulated = mean(T_Sim)
  ) %>% 
  pivot_longer(-Age, names_to = "Source") %>% 
  ggplot() +
  geom_point(aes(x = Age, y = value, colour = Source)) +
  scale_y_continuous("Time to recovery, year") +
  facet_grid(.~Source)


ggsave(g_tte, filename = here::here("docs", "figs", "g_sim_tte.png"), width = 7, height = 4)


