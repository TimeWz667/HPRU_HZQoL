library(tidyverse)
library(rstan)

theme_set(theme_bw())

options(mc.cores = 6)
rstan_options(auto_write = TRUE)


load(here::here("data", "qol_reformed.rdata"))

reformed <- reformed %>% filter(Q_rescaled > 0)

head(reformed)

min_qol <- min(reformed$EQ5D)



## Time to recovery
model_src <- "time2zero_age"
model_src <- glue::as_glue(model_src)

model <- stan_model(here::here("models", model_src + ".stan"))


ds <- local({
  sel <- reformed  %>%
    # mutate(Key = 1:n(), Key = sample(Key, n())) %>% 
    # filter(Key <= 300) %>% 
    filter(ti > 0) %>% 
    select(Ts = ti, As = Age, Ys = Health) %>% 
    as.list()
    
  sel$N <- length(sel$Ys)
  sel
})


post <- sampling(model, data = ds, pars = c("b0", "ba1"), 
                 chains = 4, iter = 5000, warmup = floor(5000 - 500))



save(post, ds, file = here::here("out", "post_" + model_src + ".rdata"))

su <- summary(post)$summary
write_csv(su, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))


ext <- data.frame(extract(post))
head(ext)

write_csv(ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))


# 
# samples <- data.frame(extract(post, pars = "rate_age")) %>% 
#   mutate(Key = 1:n()) %>% 
#   pivot_longer(-Key, values_to = "Rate") %>% 
#   tidyr::extract(name, "Age", "rate_age.(\\d+)", convert = T) 
# 
# samples %>%
#   filter(Key <= 1000) %>% 
#   write_csv(here::here("docs", "tabs", "samples_ttz(t).csv"))
# 
# 
# g_fit <- as_tibble(summary(post)$summary[-c(1:2), ]) %>% 
#   mutate(a = 1:101) %>%
#   filter(a <= 100) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = a, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
#   scale_y_continuous("Rate to recovery, per year", limits = c(0, 10)) +
#   scale_x_continuous("Age", breaks = c(0, seq(40, 100, 10))) +
#   geom_line(aes(x = a, y = mean))
# 
# 
# g_fit
# 
# ggsave(g_fit, filename = here::here("docs", "figs", "g_fit_ttz(t).png"), width = 7.5, height = 4)
# 

