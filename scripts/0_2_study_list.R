library(tidyverse)

ds <- dir("studies")
ds <- gsub(".pdf", "", ds)
ds


li <- tibble(Study = ds, N = 0) %>% 
  extract(Study, "Year", "(\\d{4})", remove = F, convert = T) %>% 
  mutate(
    SID = gsub("et al.", "", Study),
    SID = gsub("\\s+", "", SID)
  ) %>% 
  relocate(SID, Study)

write_csv(li, here::here("Studies", "list_of_studies.csv"))


