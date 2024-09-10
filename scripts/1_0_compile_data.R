library(tidyverse)


norm <- local({
  load(here::here("data", "sup_demo.rdata"))
  
  sup_demo %>% 
    ungroup() %>% 
    filter(Year == 2023) %>% 
    select(Age, Norm = norm_leoss)
})


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))

max_qol <- max(raw$EQ5D)
min_qol <- min(raw$EQ5D)


reformed <- raw %>% 
  mutate(ti = time_points / 365) %>% 
  select(Age = age, ti = ti, EQ5D) %>% 
  left_join(norm) %>% 
  mutate(
    Agp = cut(Age, c(0, 30, seq(35, 90, 5), 100), right = F),
    Health = (EQ5D >= Norm) + 0,
    Q_rescaled = (EQ5D - min_qol) / (max_qol - min_qol),
    Norm_rescaled = (Norm - min_qol) / (max_qol - min_qol)
  )

reformed

write_csv(reformed, file = here::here("data", "qol_reformed.csv"))
save(reformed, file = here::here("data", "qol_reformed.rdata"))

