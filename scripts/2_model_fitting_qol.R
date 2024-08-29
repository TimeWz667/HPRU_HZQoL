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
    Agp = cut(age, c(0, 30, seq(35, 90, 5), 100), right = F)
  )


dat_qol <- dat_qol[sample.int(nrow(dat_qol)), ]

dat_qol %>% 
  group_by(Agp) %>% 
  summarise(n())

model <-  stan_model(here::here("models", "logit_qol.stan"))

ds <- dat_qol %>%
  head(500) %>% 
  (\(df) {
    list(
      N = nrow(df),
      Ts = df$ti,
      Ys = df$qol,
      min_qol = min_qol
    )
  })

post <- sampling(model, data = ds, pars = c("qol0", "b0", "b1"), chains = 3, iter = 2000, warmup = floor(2000 - 1000))

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("out", "post_qol_all.csv"))



res_all <- list(
  data.frame(summary(post)$summary)[1:3, ] %>% 
    mutate(Group = "All", Index = c("qol0", "b0", "b1"))
)


res_agp <- lapply(levels(dat_qol$Agp), \(agp) {
  ds <- dat_qol %>% 
    filter(Agp == agp) %>% 
    (\(df) {
      list(
        N = nrow(df),
        Ts = df$ti,
        Ys = df$qol,
        min_qol = min_qol
      )
    })
  
  post <- sampling(model, data = ds, pars = c("qol0", "b0", "b1"), 
                   chains = 3, iter = 2000, warmup = floor(2000 - 1000))
  
  ext <- data.frame(extract(post))
  write_csv(ext, file = here::here("out", "post_qol_" + glue::as_glue(agp) + ".csv"))
  
  
  data.frame(summary(post)$summary)[1:3, ] %>% 
    mutate(Group = "All", Index = c("qol0", "b0", "b1"))
  
})


res <- bind_rows(c(res_all, res_agp)) %>% 
  relocate(Group, mean, starts_with("X"))

write_csv(res, here::here("docs", "tabs", "summary_post_qol.csv" ))


bind_rows(res_agp) %>% 
  filter(Index == "qol0") %>% 
  select(Group, mean) %>% 
  mutate(a = seq(25, 90, 5)) %>% 
  ggplot() +
  geom_point(aes(x = a, y = mean)) +
  geom_smooth(aes(x = a, y = mean), method = "loess")



## QOL as a function of age

model <-  stan_model(here::here("models", "logit_qol_a3.stan"))

ds <- dat_qol %>%
  #head(1000) %>% 
  (\(df) {
    list(
      N = nrow(df),
      As = df$age,
      Ys = df$qol
    )
  })

post <- sampling(model, data = ds, pars = c("b0", "b1", "b2"), 
                 chains = 3, iter = 10000, warmup = 9000)

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("results", "post_qol_(a).csv"))




