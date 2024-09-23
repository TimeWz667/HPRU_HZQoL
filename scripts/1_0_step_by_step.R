library(tidyverse)



theme_set(theme_bw())


## Area function

calc_area <- \(df) {
  df %>% 
  mutate(
    dt = ti - lag(ti),
    area = (loss + lag(loss)) * dt / 2
  ) %>% 
    summarise(loss = sum(area, na.rm = T))
}



## Load data
load(here::here("data", "qol_reformed.rdata"))
min_qol <- min(reformed$EQ5D)


full <- reformed %>% 
  arrange(PID, ti) %>% 
  mutate(loss = 1 - EQ5D)

sel <- full %>% 
  filter(PID %in% sample(unique(PID), 10)) %>% 
  arrange(PID, ti)


## raw data
raw_area_sel <- sel %>% 
  group_by(PID, Age) %>% 
  calc_area() %>% 
  ungroup()

raw_area_full <- full %>% 
  group_by(PID, Age) %>% 
  calc_area() %>% 
  ungroup()


sel %>% 
  ggplot(aes(x = ti, y = loss)) +
  geom_point() +
  geom_area() +
  facet_wrap(PID~., ncol = 1)



raw_area_full %>% summarise(loss = mean(loss))


raw_area_full %>% 
  group_by(Age) %>% 
  summarise(loss = mean(loss)) %>% 
  ggplot(aes(x = Age, y = loss)) +
  geom_point() +
  geom_smooth()



## End point
# - the last point 
# - extend endpoints to 2 years 

fn_approx <- \(df, yr = 2) {
  ids <- unique(df$PID)
  apx <- bind_rows(lapply(ids, \(pid) {
    ds <- df %>% filter(PID == pid)
    
    as_tibble(approx(ds$ti, ds$EQ5D, seq(0, yr, 0.01), rule = 2)) %>% 
      mutate(PID = pid)
    
  })) %>% rename(ti = x, EQ5D = y) %>% 
    mutate(loss = 1 - EQ5D)
  
  df %>% 
    select(SID, PID, Age, Agp, Norm) %>% 
    distinct() %>% 
    left_join(apx)
}



apx_sel <- sel %>% fn_approx()

apx_full <- full %>% fn_approx()


apx_area_sel <- apx_sel %>% 
  group_by(PID, Age) %>% 
  calc_area() %>% 
  ungroup()

apx_area_full <- apx_full %>% 
  group_by(PID, Age) %>% 
  calc_area() %>% 
  ungroup()


apx_sel %>% 
  ggplot(aes(x = ti, y = loss)) +
  geom_point() +
  geom_area() +
  scale_x_continuous("Year", limits = c(0, 2)) +
  facet_wrap(PID~., ncol = 1)


apx_area_full %>% summarise(loss = mean(loss))


apx_area_full %>% 
  group_by(Age) %>% 
  summarise(loss = mean(loss)) %>% 
  ggplot(aes(x = Age, y = loss)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = raw_area_full %>% 
              group_by(Age) %>% 
              summarise(loss = mean(loss)), aes(x = Age, y = loss, colour = "raw")) 




## Survival
## Sperate survival and qaly

calc_timing <- \(df) {
  df %>% 
    group_by(PID) %>% 
    select(PID, Age, Agp, ti, EQ5D, loss) %>% 
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


timing_sel <- sel %>% calc_timing()

timing_full <- full %>% calc_timing()


sel %>% 
  group_by(PID) %>%
  mutate(
    End = cumsum(loss) == sum(loss) & loss == loss[n()]
  ) %>% 
  ggplot(aes(x = ti, y = loss)) +
  geom_point(aes(colour = End)) +
  geom_area() +
  facet_wrap(.~PID)



timing_full


calc_hazard <- function(df, t0 = 0, t1 = 2, dt= 1/12) {
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


haz_full <- timing_full %>% calc_hazard()

haz_season_full <- haz_full %>% 
  mutate(
    ti = floor(ti * 4) / 4
  ) %>% 
  group_by(ti) %>% 
  summarise(Haz = mean(Haz))



haz_full


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

sim_tte <- \(t0) inv_hf(hf(t0) * runif(1)) + t0


## Test simulation
library(survival)

tte <- sapply(1:1000, \(i) sim_tte(0))
plot(Surv(tte))
points(haz$ti, haz$Surv, col = 2, pch = 20, cex = 2)


tte <- sapply(1:1000, \(i) sim_tte(0.5))
plot(Surv(tte))
points(haz$ti + 0.5, haz$Surv / hf(0.5), col = 2, pch = 20, cex = 2)


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


pred_timing_sel <- timing_sel %>% 
  mutate(
    T_evt2 = ifelse(QL_end == 0, T_evt, sapply(T_obs, sim_tte))
  )


pred_timing_full <- timing_full %>% 
  mutate(
    T_evt2 = ifelse(QL_end == 0, T_evt, sapply(T_obs, sim_tte))
  )


plot(Surv(pred_timing_full$T_last))


pred_timing_full %>% 
  filter(startsWith(PID, "Scott")) %>% 
  summarise(mean(T_end > 90 / 365.25))


sel
pred_timing_sel %>% 
  ggplot() +
  geom_rect(aes(xmin = 0, ymin = 0, ymax = QL_act, xmax = T_evt2)) +
  facet_wrap(.~PID, ncol = 1) +
  expand_limits(x = 2, y = 1)



imputation_area_full <- imputation_full %>% 
  mutate(
    loss = QL_act * T_evt2
  ) 



g_loss_imp <- imputation_area_full %>% 
  group_by(Age) %>% 
  summarise(loss = mean(loss, na.rm = T)) %>% 
  ggplot(aes(x = Age, y = loss)) +
  geom_point() +
  geom_smooth() + 
  scale_y_continuous("QALY loss", breaks = seq(0, 1, 0.1))

g_loss_imp

## Considering PHN

load(here::here("data", "Epi_HZ_NIC_CPRD.rdata"))
P_PHN <- P_PHN %>% 
  group_by(Age) %>% 
  summarise(p_phn = mean(p_phn))

P_PHN %>% 
  ggplot() +
  geom_line(aes(x = Age, y = p_phn))

ipw_phn <- imputation_area_full %>% 
  mutate(PHN = T_evt2 > 90 / 365.25) %>% 
  left_join(P_PHN) %>% 
  group_by(Agp) %>% 
  summarise(
    N = n(),
    p_phn1 = mean(p_phn),
    p_phn0 = sum(PHN) / N
  ) %>% 
  mutate(
    ipw_0 = (1 - p_phn1) / (1 - p_phn0), 
    ipw_1 = p_phn1 / p_phn0
  ) %>% 
  pivot_longer(starts_with("ipw"), values_to = "ipw") %>% 
  mutate(
    PHN = name == "ipw_1"
  ) %>% 
  select(Agp, PHN, ipw)




g_loss_phn <- imputation_area_full %>% 
  mutate(PHN = T_evt2 > 90 / 365.25) %>% 
  left_join(ipw_phn) %>% 
  group_by(Age) %>% 
  summarise(loss = weighted.mean(loss, wt = ipw, na.rm = T)) %>% 
  ggplot(aes(x = Age, y = loss)) +
  scale_y_continuous("QALY loss", breaks = seq(0, 1, 0.1)) + 
  geom_point()  +
  geom_smooth()

g_loss_phn
