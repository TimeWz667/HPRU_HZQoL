library(tidyverse)
library(rstan)

theme_set(theme_bw())

options(mc.cores = 6)
rstan_options(auto_write = TRUE)


raw <- read_csv(here::here("data", "27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv"))


max_qol <- 1
min_qol <- min(raw$EQ5D)

dat <- raw %>% 
  mutate(
    qol = (EQ5D - min_qol) / (max_qol - min_qol)
  ) %>% 
  rename(ti = time_points)

dat_qol <- dat %>% 
  mutate(ti = ti / 365) %>% 
  filter(qol > 0) %>% 
  filter(qol < 1) %>% 
  select(ti, qol, age) %>% 
  mutate(
    Agp = cut(age, c(0, 30, seq(35, 90, 5), 100), right = F),
    Tgp = cut(ti, c(seq(0, 1, 0.2), 1.5), right = F)
  )


dat_qol <- dat_qol[sample.int(nrow(dat_qol)), ]

dat_qol %>% 
  group_by(Agp) %>% 
  summarise(n())

dat_qol %>% 
  group_by(Tgp) %>% 
  summarise(n())

model <-  stan_model(here::here("models", "logit_qol.stan"))

ds <- dat_qol %>%
  head(500) %>% 
  (\(df) {
    list(
      N = nrow(df),
      Ys = df$qol,
      min_qol = min_qol
    )
  })

post <- sampling(model, data = ds, pars = c("qol0", "b0"), chains = 3, iter = 2000, warmup = floor(2000 - 1000))


res_all <- list(
  data.frame(summary(post)$summary)[1:2, ] %>% 
    mutate(Group = "All", Index = c("qol0", "b0"))
)


res_agp <- lapply(levels(dat_qol$Agp), \(agp) {
  ds <- dat_qol %>% 
    filter(Agp == agp) %>% 
    (\(df) {
      list(
        N = nrow(df),
        Ys = df$qol,
        min_qol = min_qol
      )
    })
  
  post <- sampling(model, data = ds, pars = c("qol0", "b0"), 
                   chains = 3, iter = 2000, warmup = floor(2000 - 1000))
  
  data.frame(summary(post)$summary)[1:2, ] %>% 
    mutate(Group = agp, Index = c("qol0", "b0"))
  
})

res_tgp <- bind_rows(res_agp)


res_tgp <- lapply(levels(dat_qol$Tgp), \(tgp) {
  ds <- dat_qol %>% 
    filter(Tgp == tgp) %>% 
    (\(df) {
      list(
        N = nrow(df),
        Ys = df$qol,
        min_qol = min_qol
      )
    })
  
  post <- sampling(model, data = ds, pars = c("qol0", "b0"), 
                   chains = 3, iter = 2000, warmup = floor(2000 - 1000))
  
  data.frame(summary(post)$summary)[1:2, ] %>% 
    mutate(Group = tgp, Index = c("qol0", "b0"))
  
})

res_tgp <- bind_rows(res_tgp)


res <- bind_rows(c(res_all, res_agp, res_tgp)) %>% 
  relocate(Group, mean, starts_with("X"))

write_csv(res, here::here("docs", "tabs", "summary_exploratory_qol.csv" ))



res_tgp %>% 
  filter(Index == "qol0") %>% 
  ggplot() +
  geom_point(aes(x = Group, y= mean))


