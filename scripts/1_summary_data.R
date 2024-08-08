library(tidyverse)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


raw %>% 
  pull(study) %>% 
  unique


raw %>% 
  group_by(study) %>% 
  summarise(
    n = length(unique(Patient.ID)) 
  )


