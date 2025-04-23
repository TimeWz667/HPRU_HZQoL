library(tidyverse)
library(readxl)



demo_proj <- read_xls(here::here("data", "external", "uk_pop_proj_2020.xls"), sheet = "Population") %>% 
  pivot_longer(-c(Sex, Age), names_to = "Year", values_to = "N") %>% 
  mutate(
    Age = ifelse(Age %in% as.character(0:100), Age, "100"),
    Age = as.numeric(Age),
    Year = as.numeric(Year),
    Sex = ifelse(Sex == 1, "m", "f")
  ) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(N = sum(N)) %>% 
  ungroup()


demo_pre <- bind_rows(lapply(2012:2020, function(yr) {
  raw_pre <- read_xlsx(here::here("data", "external", "uk_pop_data.xlsx"), sheet = as.character(yr))
  raw_pre %>% 
    filter(geogcode %in% c("E92000001", "K02000001")) %>% 
    pivot_longer(starts_with(c("m_", "f_"))) %>% 
    separate(name, c("Sex", "Year", "Age")) %>% 
    filter(Age != "al") %>% 
    mutate(
      Age = as.numeric(Age),
      Year = 2000 + as.numeric(Year)
    ) %>% 
    select(Location = variable, Year, Age, Sex, N = value)
})) %>% 
  mutate(
    Location = ifelse(Location == "ENGLAND", "England", "UK")
  )


demo_pre_old <- demo_pre %>% 
  filter(Age == 90) %>% 
  select(- Age) %>% 
  full_join(crossing(Year = 2012:2020, Age = 90:100), relationship = "many-to-many") %>% 
  group_by(Location, Year, Sex) %>% 
  mutate(
    k = 100 - Age,
    k = k / sum(k),
    N = N * k
  ) %>% 
  ungroup() %>% 
  select(Location, Year, Sex, Age, N)


demo_pre <- bind_rows(
  demo_pre %>% filter(Age != 90),
  demo_pre_old
)



rat <- demo_pre %>% 
  group_by(Year, Location, Sex) %>%
  summarise(N = sum(N)) %>% 
  pivot_wider(names_from = Location, values_from = N) %>% 
  mutate(rat = England / UK) %>% 
  filter(Year > 2016) %>% 
  group_by(Sex) %>% 
  summarise(rat = mean(rat))



mor_proj <- bind_rows(
  read_xlsx(here::here("data", "external", "uk_mor.xlsx"), sheet = "males period qx", skip = 4) %>% 
    pivot_longer(-age, names_to = "Year", values_to = "mortality") %>% 
    rename(Age = age) %>% 
    mutate(Sex = "m"),
  read_xlsx(here::here("data", "external", "uk_mor.xlsx"), sheet = "females period qx", skip = 4) %>% 
    pivot_longer(-age, names_to = "Year", values_to = "mortality") %>% 
    rename(Age = age) %>% 
    mutate(Sex = "f")
) %>% 
  mutate(
    mortality = mortality * 1e-5,
    Year = as.numeric(Year)
  ) 


demo_ons <- mor_proj %>% 
  inner_join(
    bind_rows(
      demo_pre %>% filter(Location == "England") %>% select(-Location),
      demo_proj %>% filter(Year > 2020) %>% left_join(rat) %>% 
        mutate(N = round(N * rat)) %>% 
        select(-rat)
    )
  )


##
## Source: https://github.com/bitowaqr/shortfall
##
population_norm <- read_csv(here::here("data", "external", "qale_shortfall.csv")) %>% 
  mutate(Sex = ifelse(sex == "female", "f", "m")) %>% 
  select(Age = age, Sex, norm = co)


pn <- read_csv(here::here("data", "external", "qale_shortfall.csv")) %>% 
  mutate(Sex = ifelse(sex == "female", "f", "m")) %>% 
  select(Sex, cw, co, age5_str, age5_start) %>% 
  distinct() %>% 
  group_by(Sex) %>% 
  mutate(
    age5_end = c(age5_start[-1] - 1, 100),
    age5_mid = (age5_start + age5_end) / 2
  ) %>% 
  ungroup()


## Combine them and aggregate across sexes

sup_demo_s <- demo_ons  %>% 
  mutate(
    age5_str = floor(Age * 0.2) /0.2,
    age5_str = paste(age5_str, age5_str + 4, sep = "-"),
    age5_str = case_when(
      Age < 18 ~ "16-17",
      Age < 20 ~ "18-19",
      Age >= 90 ~ "90+",
      T ~ age5_str
    )
  ) %>% 
  left_join(pn) %>% 
  select(Age, Year, mortality, Sex, N, age5_str, norm = co)


sup_demo <- sup_demo_s %>% 
  group_by(Year, age5_str) %>% 
  mutate(
    norm = weighted.mean(norm, N)
  ) %>% 
  group_by(Year, Age) %>% 
  summarise(
    mortality = weighted.mean(mortality, wt = N),
    norm = weighted.mean(norm, wt = N),
    N = sum(N)
  ) %>% 
  group_by(Year) %>% 
  mutate(
    norm_leoss = predict(loess(norm ~ Age))
  )


save(sup_demo_s, sup_demo, file = here::here("data", "processed", "sup_demo.rdata"))

# UK
# McNamara, S., Schneider, P. P., Love-Koh, J., Doran, T., & Gutacker, N. (2023). 
# Quality-adjusted life expectancy norms for the English population. Value in Health, 26(2), 163-169.

# Pre-processed with demographics
pn_uk <- sup_demo %>% 
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
  left_join(pn %>% rename(proxy = Country), relationship = "many-to-many") 

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
