library(tidyverse)



q0 = read_csv(here::here("data", "processed", "eq5d_baseline_uk.csv"))
q1 = read_csv(here::here("data", "processed", "eq5d_uk.csv"))


bl <- q0 %>% 
  inner_join(
    q1 %>% 
      group_by(Patient.ID) %>% 
      summarise(
        first = EQ5D[1],
        max_1 = max(EQ5D[time_points <= 365 / 12], na.rm = T),
        max_3 = max(EQ5D[time_points <= 365 * 0.25], na.rm = T),
        max_6 = max(EQ5D[time_points <= 365 * 0.5], na.rm = T),
        max_9 = max(EQ5D[time_points <= 365 * 0.75], na.rm = T),
        max_all = max(EQ5D, na.rm = T)
      )
    
  )

bl


c1 <- table(bl$EQ5D == 1, bl$max_all == 1)

chisq.test(c1)

c1


t.test(bl$EQ5D > bl$max_all)

bl %>% 
  mutate(
    perf_baseline = (EQ5D == 1)
  ) %>% 
  ggplot() +
  geom_histogram(aes(x = max_all)) +
  facet_grid(.~perf_baseline)


c2 <- bl %>% 
  filter(EQ5D < 1)

bl %>% 
  filter(EQ5D < 1 & max_3 < 1) %>% 
  ggplot() +
  geom_point(aes(x = EQ5D, y = max_3)) +
  geom_abline(slope = 1) +
  expand_limits(x = 0:1, y = 0:1)

bl %>% 
  filter(EQ5D < 1 & max_1 < 1) %>% 
  ggplot() +
  geom_point(aes(x = EQ5D, y = max_1)) +
  geom_abline(slope = 1) +
  expand_limits(x = 0:1, y = 0:1)



bl %>% 
  group_by(study) %>% 
  summarise(
    n()
  )

hist(bl$first)


mean(bl$first > bl$EQ5D, na.rm = T)

bl %>% 
  filter(EQ5D < 1 & first < 1) %>% 
  ggplot() +
  geom_point(aes(x = EQ5D, y = first)) +
  geom_abline(slope = 1) +
  expand_limits(x = 0:1, y = 0:1)


mean(bl$EQ5D < bl$first)
mean(bl$EQ5D == bl$first)
mean(bl$EQ5D > bl$first)
