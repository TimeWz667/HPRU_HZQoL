library(tidyverse)



eq5d %>% 
  filter(!is_baseline) %>% 
  filter(!is.na(EqMo)) %>% 
  filter(t >= 0) %>% 
  mutate(
    t = cut(t, c(0, 30, 60, 90, 120, 150, 180, 210, 240), right = F)
  ) %>% 
  ggplot() +
  geom_bar(aes(x = t, fill = as.character(EqMo)), position = "fill")


eq5d %>% 
  filter(!is_baseline) %>% 
  filter(!is.na(EqMo)) %>% 
  filter(t >= 0) %>% 
  filter(t < 300) %>%
  filter(!is.na(t)) %>% 
  filter(!is_perfect) %>% 
  mutate(
    t = cut(t, seq(0, 420, 30), right = F)
  ) %>% 
  pivot_longer(starts_with("Eq")) %>% 
  ggplot() +
  geom_bar(aes(x = t, fill = as.character(value)), position = "fill") +
  scale_fill_discrete("level") +
  facet_wrap(.~name)

