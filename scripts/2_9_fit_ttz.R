library(tidyverse)
library(rstan)

theme_set(theme_bw())

## Setting
options(mc.cores = 6)
rstan_options(auto_write = TRUE)

source(here::here("scripts", "fn_stan.R"))

n_iter <- 10000
n_collect <- 500
n_warmup <- floor(n_iter - n_collect)
n_chains <- 4


## Load data
load(here::here("data", "qol_reformed.rdata"))
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
                 chains = n_chains, iter = n_iter, warmup = n_warmup)

post

save(post, ds, file = here::here("out", "post_" + model_src + ".rdata"))


res <- restructure_stan(post)

write_csv(res$Summary, file = here::here("docs", "tabs", "fit_" + model_src + ".csv"))
write_csv(res$Ext, file = here::here("posteriors", "post_"+ model_src + ".csv"))



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

