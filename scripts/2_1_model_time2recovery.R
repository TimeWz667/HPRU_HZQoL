library(tidyverse)
library(survival)
library(rstan)

theme_set(theme_bw())


## Setting
options(mc.cores = 6)
rstan_options(auto_write = TRUE)

source(here::here("scripts", "fn_stan.R"))

n_iter <- 20000
n_collect <- 500
n_warmup <- floor(n_iter - n_collect)
n_chains <- 4


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
load(here::here("data", "qol_orig_set.rdata"))



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


dat_tte %>% 
  arrange(-T_evt) %>% 
  mutate(pr = 1:n() / n()) %>% 
  ggplot() +
  geom_line(aes(T_evt, pr)) +
  geom_point(data = haz, aes(x = ti, y = Surv))


## Insert event points to those censored
dat_filled <- dat_tte %>% 
  mutate(
    T_evt2 = ifelse(QL_end == 0, T_evt, sapply(T_obs, sim_tte))
  )


g_tte_imputated <- dat_filled %>% 
  arrange(-T_evt2) %>% 
  mutate(pr = 1:n() / n(), Censored = !Recovered) %>% 
  ggplot() +
  geom_point(aes(T_evt2, pr, colour = Censored), alpha = 0.2) +
  geom_line(data = haz, aes(x = ti, y = Surv)) +
  scale_y_continuous("Survival", labels = scales::percent) +
  scale_x_continuous("Years since rush onset") +
  scale_colour_discrete("Censored") +
  theme(legend.position = "bottom")

g_tte_imputated

ggsave(g_tte_imputated, file = here::here("docs", "figs", "g_tte_imputated.png"), width = 6, height = 4)



dat_filled


dat <- dat_qol %>% 
  select(PID, SID, Age, Agp) %>% 
  distinct() %>% 
  left_join(dat_filled %>% select(PID, T_evt = T_evt2)) %>% 
  filter(T_evt > 0)


dat %>% 
  group_by(Agp) %>% 
  summarise(Dur = mean(T_evt)) %>% 
  ggplot(aes(x = Agp, y = log(1 / Dur))) +
  geom_point() +
  expand_limits(y = 0)


dat %>% 
  filter(T_evt >0) %>% 
  ggplot(aes(x = Age, y = log(T_evt))) +
  geom_point() +
  geom_smooth(method = "lm")


summary(lm(log(T_evt) ~ Age, data = dat))


## Model fitting
model_src <- "time2zero_age"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))



ds <- local({
  d <- dat %>% filter(T_evt > 0)
  list(
    Ts = d$T_evt,
    As = d$Age,
    N = nrow(d)
  )
})

post <- sampling(model, data = ds, pars = c("r0", "ba1"), init = \() { list(r0 = exp(-2.35), ba1 = 0.01)},
                 chains = n_chains, iter = n_iter, warmup = n_warmup)


res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))



res$Ext %>% 
  crossing(Age = 30:95) %>% 
  mutate(
    rate = r0 * exp(ba1 * Age),
    dur = 1 / rate
  ) %>% 
  group_by(Age) %>% 
  summarise(
    M = median(dur),
    L = quantile(dur, 0.025),
    U = quantile(dur, 0.975)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Age, ymin = L, ymax = U), alpha = 0.1) +
  geom_line(aes(x = Age, y = M)) +
  geom_point(data = dat, aes(x = Age, y = T_evt))











