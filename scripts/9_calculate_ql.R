library(tidyverse)
library(tidybayes)

## age mapping

idx_agp <- cut(0:99, c(0, 30, seq(35, 90, 5), 100), right = F)


## load data
load(here::here("data", "sup_demo.rdata"))
pars_demo <- sup_demo %>% 
  filter(Year == 2023) %>% 
  select(age = Age, mr = mortality, norm = norm_leoss) %>% 
  ungroup()



#pars_qol0 <- lapply(levels(idx_agp), \(agp) read_csv(here::here("out", "post_qol_" + glue::as_glue(agp) + ".csv")) %>% pull(qol0))
# names(pars_qol0) <- levels(idx_agp)

pars_qol0 <- read_csv(file = here::here("results", "post_qol_(a)_lm.csv"))

pars_qol0 %>% 
  ggplot(aes(x = age)) +
  stat_lineribbon(aes(y = qol)) +
  scale_fill_brewer() 
  


ttz <- read_csv( here::here("results", "post_ttz_(a).csv"))


## simulate

simulate_qale <- function(age, n_sim = 1000) {
  ### Pick parameters
  # qds <- pars_qol0[[idx_agp[age]]]
  
  qds <- pars_qol0 %>% filter(age == age) %>% pull(qol)
  rates <- unname(unlist(ttz[, paste0("rate_age.", age)]))
  qds <- sample(qds, n_sim)
  rates <- sample(rates, n_sim)
  
  
  tibble(Key = 1:n_sim, qd = qds, rate = rates) %>% 
    crossing(age = age:100) %>% 
    left_join(pars_demo, by = "age") %>% 
    group_by(Key) %>% 
    mutate(
      ti = 1:n(),
      pd = diff(c(0, pexp(ti, rate)))
    ) %>% 
    mutate(
      qdz = pmin(norm, qd),
      qol = pd * qdz + (1 - pd) * norm,
      surv = c(1, cumprod(1 - mr)[-n()]),
      d15 = (1 + 0.015) ^ -(ti - 1),
      d35 = (1 + 0.035) ^ -(ti - 1)
    ) %>% 
    group_by(Key, Qol0 = qd, RRec = rate) %>% 
    summarise(
      LE = sum(surv),
      QALE1 = sum(qol * surv),
      QALE0 = sum(norm * surv),
      QL00 = sum((norm - qol) * surv),
      QL15 = sum((norm - qol) * surv * d15),
      QL35 = sum((norm - qol) * surv * d35)
    ) %>% 
    mutate(Age = age)
}



sims <- bind_rows(lapply(20:100, \(age) {
  print(age)
  simulate_qale(age, 3000)
})) %>% 
  ungroup()


ggplot(sims) + 
  stat_lineribbon(aes(x = Age, y = QL00), alpha = 0.2) +
  scale_fill_brewer() 


g_ql <- ggplot(sims) + 
  stat_lineribbon(aes(x = Age, y = QL35)) +
  scale_y_continuous("QALY loss at 3.5% discounting rate") +
  scale_x_continuous("Age of HZ onset") +
  scale_fill_brewer() 
  
g_ql

ggsave(g_ql, filename = here::here("docs", "figs", "proj_qol.png"), width = 6, height = 4.5)


ggplot(sims) + 
  stat_lineribbon(aes(x = Age, y = 1 / RRec), alpha = 0.2) +
  scale_fill_brewer() 


res <- sims %>% 
  select(- Key)  %>% 
  group_by(Age) %>% 
  summarise(
    across(everything(), list(
      Avg = mean,
      Q025 = \(x) quantile(x, 0.025),
      Q250 = \(x) quantile(x, 0.25),
      Q500 = \(x) quantile(x, 0.5),
      Q750 = \(x) quantile(x, 0.75),
      Q975 = \(x) quantile(x, 0.975)
    ))
  )


write_csv(res, here::here("docs", "tabs", "projection.csv"))


res %>% 
  pivot_longer(-Age) %>% 
  separate(name, c("Index", "Stat"), "_") %>% 
  pivot_wider(names_from = "Stat") %>% 
  filter(startsWith(Index, "QL")) %>% 
  data.frame()


# With the model fits, the trajectory of qALY associated with HZ were simulated with a two step process. 
# - Simulate a time to recovery from an exponential distribution
# - Simulate a
