library(tidyverse)



# UK
# McNamara, S., Schneider, P. P., Love-Koh, J., Doran, T., & Gutacker, N. (2023). 
# Quality-adjusted life expectancy norms for the English population. Value in Health, 26(2), 163-169.

# Pre-processed with demographics
pn_uk <- local({
  load(here::here("data", "processed", "sup_demo.rdata"))
  
  sup_demo
}) %>% 
  filter(Year == 2025) %>% 
  ungroup() %>% 
  select(Age, norm) %>% 
  mutate(
    Country = "UK",
    Source = "McNamara et al."
  )



# Canada
# Poder TG, Carrier N, Kouakou CRC. 
# Quebec Health-Related Quality-of-Life Population Norms Using the EQ-5D-5L: 
# Decomposition by Sociodemographic Data and Health Problems. Value Health. 2020 Feb;23(2):251-259.

pn_ca <- read_csv(here::here("data", "external", "raw_pnorm_Poder2020.csv")) %>% 
  mutate(
    Age = ifelse(Age == "$75", "75-100", Age)
  ) %>% 
  separate(Age, c("age0", "age1"), "-") %>% 
  select(age0, age1, norm = Mean) %>% 
  mutate(
    age0 = as.numeric(age0),
    age1 = as.numeric(age1),
    n = age1 - age0 + 1
  ) %>% 
  uncount(n) %>% 
  group_by(age0, age1) %>% 
  mutate(key = 1:n()) %>% 
  ungroup() %>% 
  mutate(Age = age0 + key - 1) %>% 
  select(Age, norm) %>% 
  mutate(
    Country = "Canada",
    Source = "Poder et al."
  )


# Brazil
# Proxy for Mexico and Costa Rica
# Santos, M., Monteiro, A. L., & Santos, B. (2021). 
# EQ-5D Brazilian population norms. Health and quality of life outcomes, 19(1), 162.


pn_br <- read_csv(here::here("data", "external", "raw_pnorm_Santos2021.csv")) %>% 
  filter(Age != "Total") %>% 
  separate(Age, c("age0", "age1"), "–") %>% 
  select(age0, age1, norm = Mean) %>% 
  mutate(
    age0 = as.numeric(age0),
    age1 = as.numeric(age1),
    age1 = ifelse(age1 == max(age1), 100, age1),
    n = age1 - age0 + 1
  ) %>% 
  uncount(n) %>% 
  group_by(age0, age1) %>% 
  mutate(key = 1:n()) %>% 
  ungroup() %>% 
  mutate(Age = age0 + key - 1) %>% 
  select(Age, norm) %>% 
  mutate(
    Country = "Brazil",
    Source = "Santos et al."
  )


# The other countries

pn_20 <- read_csv(here::here("data", "external", "raw_pnorm_Janssen2019.csv")) %>% 
  filter(Country %in% c("Argentina", "Korea", "Netherlands", "Thailand")) %>% 
  pivot_longer(-Country, names_to = "Age", values_to = "norm") %>% 
  filter(Age != "Total" & !is.na(norm)) %>% 
  mutate(
    Age = ifelse(Age == "75+", "75–100", Age)
  ) %>% 
  separate(Age, c("age0", "age1"), "–") %>% 
  select(age0, age1, Country, norm) %>% 
  group_by(Country) %>% 
  mutate(
    age0 = as.numeric(age0),
    age1 = as.numeric(age1),
    age1 = ifelse(age1 == max(age1), 100, age1),
    n = age1 - age0 + 1
  ) %>% 
  uncount(n) %>% 
  group_by(age0, age1, Country) %>% 
  mutate(key = 1:n()) %>% 
  ungroup() %>% 
  mutate(Age = age0 + key - 1) %>% 
  select(Age, norm, Country) %>% 
  mutate(
    Source = "Janssen et al."
  )


# combining
pn <- bind_rows(pn_uk, pn_ca, pn_br, pn_20) %>% 
  group_by(Country, Source) %>% 
  mutate(
    norm_leoss = predict(loess(norm ~ Age))
  ) %>% 
  ungroup()

write_csv(pn, here::here("data", "processed", "population_norm.csv"))


mapping <- read_csv(here::here("data", "pn_mapping.csv"))

mapped <- mapping %>% 
  left_join(pn, relationship = "many-to-many") 

write_csv(mapped, here::here("data", "processed", "population_norm_mapped.csv"))



pn %>% 
  filter(Age >= 18 & Age <= 90) %>% 
  ggplot(aes(x = Age, y = norm, colour = Country)) +
  geom_line() +
  geom_smooth() +
  expand_limits(y = 0)


pn %>% 
  filter(Age >= 18 & Age <= 90) %>% 
  ggplot(aes(x = Age, y = norm, colour = Country)) +
  geom_line() +
  geom_line(aes(y = norm_leoss), linewidth = 2) +
  expand_limits(y = 0)
