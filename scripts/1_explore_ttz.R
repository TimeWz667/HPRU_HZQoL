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


## Time to recovery
dat_ttr <- dat %>% 
  mutate(Rec = (qol >= 1) + 0, ti = ti / 365) %>% 
  filter(ti > 0) %>% 
  filter(qol > 0) %>% 
  select(ti, Rec, age) %>% 
  mutate(
    Agp = cut(age, c(0, 30, seq(35, 90, 5), 100), right = F)
  )


dat_ttr %>% 
  group_by(Agp) %>% 
  summarise(n())

model <-  stan_model(here::here("models", "time2zero.stan"))

ds <- dat_ttr %>%
  head(500) %>% 
  (\(df) {
    list(
      N = nrow(df),
      Ts = df$ti,
      Ys = df$Rec
    )
  })

post <- sampling(model, data = ds, pars = "rate", chains = 3, iter = 2000, warmup = floor(2000 - 1000))

ext <- data.frame(extract(post))
write_csv(ext, file = here::here("out", "post_ttz_all.csv"))


res_all <- data.frame(summary(post)$summary)[1, ] %>% mutate(Group = "All")




res_agp <- lapply(levels(dat_ttr$Agp), \(agp) {
  ds <- dat_ttr %>% 
    filter(Agp == agp) %>% 
    (\(df) {
      list(
        N = nrow(df),
        Ts = df$ti,
        Ys = df$Rec
      )
    })
  
  post <- sampling(model, data = ds, pars = "rate", 
                   chains = 3, iter = 2000, warmup = floor(2000 - 1000))
  
  ext <- data.frame(extract(post))
  write_csv(ext, file = here::here("out", "post_ttz_" + glue::as_glue(agp) + ".csv"))
  
  
  data.frame(summary(post)$summary)[1, ] %>% mutate(Group = agp)
  
})

res_agp <- bind_rows(res_agp)


res <- bind_rows(res_all, res_agp) %>% 
  relocate(Group, mean, starts_with("X"))

res

write_csv(res, here::here("docs", "tabs", "summary_post_ttz.csv" ))
