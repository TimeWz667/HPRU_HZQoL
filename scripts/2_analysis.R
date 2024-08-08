

scores <- 0.5 * (log(1 - y) - (log(1 - 0.6))) / time_points


tibble(time = 0:100) %>% 
  mutate(
    scores = 1 - (1 - 0.4) * exp(-0.02 * time)
  ) %>% 
  ggplot() +
  geom_line(aes(x = time, y = scores)) +
  expand_limits(y = 0:1)
