library(tidyverse)


theme_set(theme_bw())



## simulation study


rate_true <- 5


n_sim <- 100

n_reg <- 70



sims <- crossing(Key = 1:n_reg, Ti = c(7, 30, 90, 120, 180, 210, 240, 300)) %>% 
  bind_rows(lapply((700 + 1):1000, \(i) {
    tibble(Key = i, Ti =  c(7, sample(14:365, 5)))
  })) %>% 
  left_join(tibble(Key = 1:n_sim, TTE = rexp(n_sim, rate_true) * 365), by = "Key") %>% 
  mutate(
    End = (Ti > TTE) + 0
  ) %>% 
  select(-TTE) %>% 
  arrange(Key, Ti)



xs <- sims %>% 
  group_by(Key) %>% 
  mutate(
    SID = 1:n(),
    n_zero = sum(End == 0),
    n_one = sum(End == 1),
    i_one = cumsum(End == 1)
  ) %>% 
  filter(n_zero >= 1) %>% 
  filter(n_one >= 1) %>% 
  filter(n_zero == SID | i_one == 1) %>% 
  select(Key, Ti, End) %>% 
  ungroup() %>% 
  pivot_wider(names_from = End, values_from = Ti, names_prefix = "Evt")


lis <- lapply(seq(0.01, 10, 0.01), \(r) {
  list(
    rate = r,
    li = sum(log(pexp(xs$Evt1 / 365, r) - pexp(xs$Evt0 / 365, r)))
  )
}) %>% 
  bind_rows()


plot(lis$rate, lis$li)
abline(v = rate_true, col = 2)
abline(v = lis %>% filter(li == max(li)) %>% pull(rate), col = 3)


ds <- local({
  xs <- sims %>% 
    group_by(Key) %>% 
    mutate(
      SID = 1:n(),
      n_zero = sum(End == 0),
      n_one = sum(End == 1),
      i_one = cumsum(End == 1)
    ) %>% 
    filter(n_zero >= 1)

  xs_va <- xs %>% 
    filter(n_one >= 1) %>% 
    filter(n_zero == SID | i_one == 1) %>% 
    select(Key, Ti, End) %>% 
    ungroup() %>% 
    pivot_wider(names_from = End, values_from = Ti, names_prefix = "Evt")
    
  
  xs_cen <- xs %>% filter(n_one < 1) %>% filter(Ti == max(Ti))
  
  xs_cen 
  
  
  list(
    N = nrow(xs_va),
    Ts0 = xs_va$Evt0 / 365,
    Ts1 = xs_va$Evt1 / 365,
    N_Cen = nrow(xs_cen),
    Ts_Cen = xs_cen$Ti / 365
  )
  
})

library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



model <- rstan::stan_model(here::here("models", "time2zero_surv.stan"))


post <- rstan::sampling(model, data = ds, pars = "rate")

post


ds$As <- sample(50:90, ds$N, rep = T)
ds$As_Cen <- sample(50:90, ds$N_Cen, rep = T)

model <- rstan::stan_model(here::here("models", "time2zero_surv_age.stan"))

post <- rstan::sampling(model, data = ds, pars = c("r0", "ba1"))

post


