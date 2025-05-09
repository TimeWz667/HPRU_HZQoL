library(tidyverse)
library(readxl)



demo_proj <- read_xls(here::here("data", "uk_pop_proj_2020.xls"), sheet = "Population") %>% 
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
  raw_pre <- read_xlsx(here::here("data", "uk_pop_data.xlsx"), sheet = as.character(yr))
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
  read_xlsx(here::here("data", "uk_mor.xlsx"), sheet = "males period qx", skip = 4) %>% 
    pivot_longer(-age, names_to = "Year", values_to = "mortality") %>% 
    rename(Age = age) %>% 
    mutate(Sex = "m"),
  read_xlsx(here::here("data", "uk_mor.xlsx"), sheet = "females period qx", skip = 4) %>% 
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
population_norm <- read_csv(here::here("data", "qale_shortfall.csv")) %>% 
  mutate(Sex = ifelse(sex == "female", "f", "m")) %>% 
  select(Age = age, Sex, norm = co)


pn <- read_csv(here::here("data", "qale_shortfall.csv")) %>% 
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


save(sup_demo_s, sup_demo, file = here::here("data", "sup_demo.rdata"))
