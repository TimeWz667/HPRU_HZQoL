library(targets)
library(tidyverse)


d_ph <- tar_read(data_raw, 1)
d_pn <- tar_read(data_raw_pn, 1)
d_0 <- tar_read(data_raw_0, 1)



step_ph <- bind_rows(lapply(c(seq(0, 0.2, 0.01), seq(0.2, 1, 0.05)[-1]), \(t1) {
  d_ph %>% 
    filter(ti <= t1) %>% 
    group_by(PID) %>% 
    summarise(q = max(EQ5D)) %>% 
    ungroup() %>% 
    summarise(k = mean(q >= 1)) %>% 
    mutate(Time = t1)
})) %>% mutate(Cap = "Perfect health")


step_pn <- bind_rows(lapply(c(seq(0, 0.2, 0.01), seq(0.2, 1, 0.05)[-1]), \(t1) {
  d_pn %>% 
    filter(ti <= t1) %>% 
    group_by(PID) %>% 
    summarise(q = max(EQ5D)) %>% 
    ungroup() %>% 
    summarise(k = mean(q >= 1)) %>% 
    mutate(Time = t1)
})) %>% mutate(Cap = "Population norm")


step_0 <- bind_rows(lapply(c(seq(0, 0.2, 0.01), seq(0.2, 1, 0.05)[-1]), \(t1) {
  d_0 %>% 
    filter(ti <= t1) %>% 
    group_by(PID) %>% 
    summarise(q = max(EQ5D)) %>% 
    ungroup() %>% 
    summarise(k = mean(q >= 1)) %>% 
    mutate(Time = t1)
})) %>% mutate(Cap = "Pre-HZ baseline")


step <- bind_rows(step_ph, step_pn, step_0)


cuv <- step %>% 
  group_by(Cap) %>% 
  summarise(
    Time0 = max(Time[k < 0.5]), 
    Time1 = min(Time[k > 0.5]),
    Time = (Time0 + Time1) / 2,
    rate = log(2) / Time
  ) %>% 
  select(Cap, rate) %>% 
  crossing(Time = seq(0, 1, 0.01)) %>% 
  mutate(
    f = pexp(Time, rate - 2.5)
  )


g <- step %>% 
  ggplot() +
  geom_line(aes(x = Time, y = k, colour = Cap)) +
  geom_line(data = cuv, aes(x = Time, y = f, colour = Cap), linetype = 2) + 
  scale_y_continuous("Met end-point at least once, %", labels = scales::percent) +
  scale_x_continuous("Month", labels = scales::number_format(scale = 12)) +
  expand_limits(y = 0:1)


ggsave(g, filename = "out/fig_prop(end).png", width = 6, height = 4)



tar_read(data_raw, 1) %>% 
  group_by(SID, PID) %>% 
  summarise(
    n_rec = n()
  ) %>% 
  group_by(SID) %>% 
  #ungroup() %>% 
  summarise(
    n = n(),
    n_exp = max(n_rec),
    m_rec = median(n_rec),
    l_rec = quantile(n_rec, 0.25),
    p_complete = mean(n_rec == n_exp),
    p_complete_m1 = mean(n_rec >= (n_exp - 1)),
    p_complete_m2 = mean(n_rec >= (n_exp - 2)),
    p_complete_m3 = mean(n_rec >= (n_exp - 3))
  )



tar_read(data_raw, 1) %>% 
  group_by(SID, PID) %>% 
  summarise(
    n_rec = n()
  ) %>% 
  group_by(SID) %>% 
  left_join(
    tar_read(data_qol, 1) %>% 
      group_by(SID, PID) %>% 
      summarise(
        n_rec1 = n()
      ) %>% 
      group_by(SID)
  ) %>% 
  ungroup() %>% 
  summarise(
    mean(n_rec > n_rec1, na.rm = T)
  )



tar_read(sim_qloss, 1)  %>% 
  filter(Age %in% c(50, 60, 70, 80)) %>% 
  group_by(Age) %>% 
  summarise(n(), mean(QL00))

tar_read(sim_qloss_pn, 1) %>% 
#  filter(Age %in% c(50, 60, 70, 80)) %>% 
  group_by(study, Age) %>% 
  summarise(n(), ql = mean(QL00)) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = ql, colour = study))

tar_read(sim_qloss_pn_pooled, 1)  %>% 
  filter(Age %in% c(50, 60, 70, 80)) %>% 
  group_by(Age) %>% 
  summarise(mean(QL00))

tar_read(sim_qloss_0, 1)  %>% 
  filter(Age %in% c(50, 60, 70, 80)) %>% 
  group_by(Age) %>% 
  summarise(mean(QL00))

tar_read(pars_qloss, 1) %>% 
  crossing(Age = 50:99) %>% 
  mutate(
    
  ) %>% 
  group_by(Age)

