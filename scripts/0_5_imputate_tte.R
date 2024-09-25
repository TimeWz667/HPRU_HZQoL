library(tidyverse)
library(survival)


theme_set(theme_bw())


## Functions
calc_timing <- \(df) {
  df %>% 
    group_by(PID) %>% 
    select(PID, Age, Agp, ti, EQ5D) %>% 
    mutate(
      loss = 1 - EQ5D,
      End = cumsum(loss) == sum(loss) & loss == loss[n()],
      EndZeros = End * (loss == 0)
    ) %>% 
    summarise(
      QL_act = mean(loss[!End]),
      QL_end = mean(loss[End]),
      T_last = max(ti[!End]),
      T_last = ifelse(is.infinite(T_last), 0, T_last),
      T_end = min(ti[End]),
      T_end = ifelse(T_last == 0, 0, T_end),
      T_obs = max(ti),
      T_evt = (T_last + T_end) / 2,
      Recovered = sum(EndZeros) > 0
    ) %>% 
    mutate(
      T_evt = ifelse(Recovered, T_evt, T_obs)
    )
}


calc_hazard <- \(df, t0 = 0, t1 = 2, dt= 1/12) {
  bind_rows(lapply(seq(t0, t1, dt), \(ti) {
    from <- ti
    to <- ti + dt
    
    df %>%
      filter(pmin(T_evt, T_obs) > from) %>% 
      summarise(
        ti = ti,
        AtRisk = n(),
        Evt = sum(T_evt <= to),
        Haz = Evt / AtRisk
      )
  }))
}


## Load data
load(here::here("data", "qol_uk_set.rdata"))



dat_tte <- dat_qol %>% calc_timing()


haz_full <- dat_tte %>% calc_hazard(dt = 1/12)

haz_season_full <- haz_full %>% 
  mutate(
    ti = floor(ti * 4) / 4
  ) %>% 
  group_by(ti) %>% 
  summarise(Haz = mean(Haz))

haz <- with(haz_season_full %>% filter(!is.na(Haz)), {
  as.data.frame(approx(x = ti, y = Haz, xout = seq(0, 5, 1 / 12), rule = 2)) %>% 
    rename(ti = x, Haz = y)
}) %>% 
  mutate(
    Haz = ifelse(ti == max(ti), 1, Haz),
    Surv = c(1, cumprod(1 - Haz)[-n()])
  )

hf <- approxfun(x = haz$ti, y = haz$Surv, rule = 2)
inv_hf <- approxfun(x = haz$Surv, y = haz$ti, rule = 2)

sim_tte <- \(t0) inv_hf(hf(t0) * runif(1))


## Test simulation

tte <- sapply(1:1000, \(i) sim_tte(0))
plot(Surv(tte))
points(haz$ti, haz$Surv, col = 2, pch = 20, cex = 2)


tte <- sapply(1:1000, \(i) sim_tte(0.5))
plot(Surv(tte))
haz_t <- haz %>% filter(ti >= 0.5)
points(haz_t$ti, haz_t$Surv / hf(0.5), col = 2, pch = 20, cex = 2)



haz_full %>% 
  ggplot(aes(x = ti, y = Haz)) +
  geom_line(aes(colour = "month")) +
  geom_line(data = haz_season_full, aes(colour = "season"), size = 3) +
  geom_line(data = haz, aes(colour = "int"))


haz %>% 
  ggplot() + 
  geom_line(aes(x = ti, y = Surv)) +
  scale_x_continuous("Event time") +
  scale_y_continuous("Survival")




## Insert event points to those censored
dat_filled <- dat_tte %>% 
  mutate(
    T_evt2 = ifelse(QL_end == 0, T_evt, sapply(T_obs, sim_tte))
  ) %>% 
  left_join(
    dat_qol %>% 
      select(PID, SID, Age, Agp) %>% 
      distinct()
  )


write_csv(dat_filled, here::here("data", "tte_uk.csv"))

g_tte_imputated <- dat_filled %>% 
  arrange(-T_evt2) %>% 
  mutate(pr = 1:n() / n(), Censored = !Recovered) %>% 
  ggplot() +
  geom_point(aes(T_evt2, pr, colour = Censored), alpha = 0.2) +
  geom_line(data = haz, aes(x = ti, y = Surv)) +
  scale_y_continuous("Survival", labels = scales::percent) +
  scale_x_continuous("Years since rash onset") +
  scale_colour_discrete("Censored") +
  theme(legend.position = "bottom")

g_tte_imputated

ggsave(g_tte_imputated, file = here::here("docs", "figs", "g_tte_imputated.png"), width = 6, height = 4)



