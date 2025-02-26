library(tidyverse)
library(targets)
library(rstan)

dat_qol <- tar_read(data_qol, 1)
n_iter = 5e3
n_collect = 500
n_chains = 4

n_warmup <- floor(n_iter - n_collect)

# dat_qol <- tar_read(data_qol, 1)

ds_nz <- dat_qol %>% mutate(
    Gs = as.numeric(as.factor(SID))
  ) %>% filter(!is.na(Age)) %>% 
  filter(EQ5D < 1)


map_gp <- ds_nz %>% select(SID, Gs) %>% distinct() %>% 
  mutate(
    Gname = case_when(
      startsWith(SID, "Drolet") ~ glue::as_glue(SID) + " Canada",
      startsWith(SID, "Scott") ~ glue::as_glue(SID) + " England",
      startsWith(SID, "van") ~ glue::as_glue(SID) + "van Wijck et al. 2016 Netherlands",
      T ~ SID
    )
  ) %>% 
  tidyr::extract(Gname, c("Author", "Year", "Country"), "(\\S+) et al. (\\d+) (\\S+)") %>% 
  mutate(
    Gname = sprintf("%s %s %s", Author, Country, Year)
  ) %>%
  arrange(Country) %>% 
  bind_rows(
    list(SID = "Pooled", Gs = 0, Gname = "Pooled")
  )
  

clu <- kmeans(ds_nz$EQ5D, 2)


if (clu$centers[1] > clu$centers[2]) {
  ds_nz <- ds_nz %>% mutate(cluster = clu$cluster)
} else {
  ds_nz <- ds_nz %>% mutate(cluster = 3 - clu$cluster)
}

# model_norm <- stan_model(here::here("models", "reff_norm_0.stan"))
# model_norm_t <- stan_model(here::here("models", "reff_norm_t.stan"))
model_norm_tg <- stan_model(here::here("models", "reff_norm_tg.stan"))



# Q, cluster 1
ds <- local({
  ds <- ds_nz %>% 
    filter(cluster == 1) %>%
    # sample_n(500) %>%
    select(Gs, Ys = EQ5D, Ts = ti) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})

post_c1 <- sampling(model_norm_tg, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], 
                    pars = c("b0", "bg"), chains = n_chains, iter = n_iter, warmup = n_warmup)


# Q, cluster 2
ds <- local({
  ds <- ds_nz %>% 
    filter(cluster == 2) %>%
    # sample_n(500) %>%
    select(Gs, Ys = EQ5D, Ts = ti) %>% as.list()
  
  ds$N <- length(ds$Ys)
  ds$N_gp <- length(unique(ds$Gs))
  ds
})
post_c2 <- sampling(model_norm_tg, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], 
                    pars = c("b0", "bg"), chains = n_chains, iter = n_iter, warmup = n_warmup)


res <- bind_rows(
  rstan::extract(post_c1, pars = c("b0", "bg")) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  mutate(bg.0 = 0) %>% 
  pivot_longer(-b0) %>% 
  mutate(
    pred = value + b0,
    sub = "C1"
  ),
  rstan::extract(post_c2, pars = c("b0", "bg")) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  mutate(bg.0 = 0) %>% 
  pivot_longer(-b0) %>% 
  mutate(
    pred = value + b0,
    sub = "C2"
  )
) %>% 
  group_by(name, sub)%>% 
  summarise(
    M = median(pred),
    L = quantile(pred, 0.05),
    U = quantile(pred, 0.95)
  ) %>% 
  tidyr::extract(name, "Gs", "bg.(\\d+)", convert = T) %>% 
  left_join(map_gp) %>% 
  arrange(sub, Gs)

write_csv(res, here::here("docs", "tabs", "tab_hetero.csv"))




dat <- read_csv(here::here("docs", "tabs", "tab_hetero.csv")) %>% 
  mutate(
    SID = sprintf("%s %s\n%s", Author, Year, Country),
    SID = ifelse(is.na(Year), "Pooled", SID), 
    SID = factor(SID)
  )


dat

theme_set(theme_bw())

g_heter <- dat %>% 
  filter(SID != "Pooled") %>% 
  ggplot(aes(y = SID)) +
  geom_point(aes(x = M)) +
  geom_errorbar(aes(xmin = L, xmax = U), width = 0.3) +
  geom_pointrange(data = dat %>% filter(SID == "Pooled"), aes(y = SID, x = M, xmin = L, xmax = U)) +
  facet_grid(.~sub, labeller = labeller(sub = c(C1 = "mild discomfort", C2 = "severe discomfort"))) +
  scale_x_continuous("EQ-5D score") +
  scale_y_discrete("", limits = rev) +
  expand_limits(x = 1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ggsave(g_heter, filename = here::here("docs", "figs", paste0("g_qol_heter.png")), width = 7, height = 4)

