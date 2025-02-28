fit_qol_bayes <- function(dat_qol, n_iter = 5e3, n_collect = 500, n_chains = 4) {
  require(rstan)
  require(tidyverse)
  require(lme4)
  
  n_warmup <- floor(n_iter - n_collect)
  
  # dat_qol <- tar_read(data_qol, 1)
  
  dat_qol <- dat_qol %>% mutate(Gs = as.numeric(as.factor(SID))) %>% filter(!is.na(Age))
  ds_nz <- dat_qol %>% filter(EQ5D < 1)
  clu <- kmeans(ds_nz$EQ5D, 2)
  
  
  
  if (clu$centers[1] > clu$centers[2]) {
    ds_nz <- ds_nz %>% mutate(cluster = clu$cluster)
  } else {
    ds_nz <- ds_nz %>% mutate(cluster = 3 - clu$cluster)
  }

  # model_norm <- stan_model(here::here("models", "reff_norm_0.stan"))
  # model_norm_t <- stan_model(here::here("models", "reff_norm_t.stan"))
  model_norm_tg <- stan_model(here::here("models", "reff_norm_tg.stan"))
  
#  model_logit <- stan_model(here::here("models", "reff_logistic_0.stan"))
  model_logit_d <- stan_model(here::here("models", "reff_logistic_d.stan"))
  # model_logit_ad <- stan_model(here::here("models", "reff_logistic_ad.stan"))
  
  # m_c1 <- lmer(EQ5D ~ 1 + (ti | SID), data = ds_nz %>% filter(cluster == 1))
  # ms$c2 <- lmer(EQ5D ~ 1 + (ti | SID), data = ds_c2)
  # ms$pz <- glmer(cluster ~ 1 + (1 | SID), data = ds_zero, family = binomial)
  # m_pc1 <-  glmer(cluster ~ 1 + (1 | SID), data = ds_nz %>% mutate(cluster = (cluster == 1) + 0), family = binomial)
  
  
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
                      pars = c("b0"), chains = n_chains, iter = n_iter, warmup = n_warmup)
  
  
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
                      pars = c("b0"), chains = n_chains, iter = n_iter, warmup = n_warmup)
  
  
  # P(zero)
  ds <- local({
    ds <- dat_qol %>% mutate(cluster = (EQ5D >= 1) + 0) %>%
      sample_n(500) %>%
      select(Gs, Ys = cluster, Ts = ti, As = Age) %>% as.list()
    
    ds$N <- length(ds$Ys)
    ds$N_gp <- length(unique(ds$Gs))
    ds
  })
  post_pz <- sampling(model_logit_d, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")], 
                      pars = c("b0", "bd15", "bd30"), chains = n_chains, iter = n_iter, warmup = n_warmup)
  #post_pz <- sampling(model_logit_ad, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts", "As")], chains = 3, pars = c("b0", "bd15", "bd30", "ba1", "ba2"))
  
  
  # P(c1|~zero)
  ds <- local({
    ds <- ds_nz %>% mutate(cluster = (cluster == 1) + 0) %>%
      # sample_n(500) %>%
      select(Gs, Ys = cluster, Ts = ti, As = Age) %>% as.list()
    
    ds$N <- length(ds$Ys)
    ds$N_gp <- length(unique(ds$Gs))
    ds
  })
  post_pc1 <- sampling(model_logit_d, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts")],
                       pars = c("b0", "bd15", "bd30"), chains = n_chains, iter = n_iter, warmup = n_warmup)
  # post_pc1 <- sampling(model_logit_ad, data = ds[c("Ys", "N", "Gs", "N_gp", "Ts", "As")],
  #                      pars = c("b0", "bd15", "bd30", "ba1", "ba2"), chains = n_chains, iter = n_iter, warmup = n_warmup)


  res_c1 <- restructure_stan(post_c1)
  res_c2 <- restructure_stan(post_c2)
  res_pz <- restructure_stan(post_pz)
  res_pc1 <- restructure_stan(post_pc1)
  
  res <- list(
    Ext = bind_rows(
      C1 = res_c1$Ext %>% mutate(Model = "C1"),
      C2 = res_c2$Ext %>% mutate(Model = "C2"),
      PZ = res_pz$Ext %>% mutate(Model = "PZ"),
      PC1 = res_pc1$Ext %>% mutate(Model = "PC1")
    ), 
    Summary = bind_rows(
      res_c1$Summary %>% mutate(Model = "C1"),
      res_c2$Summary %>% mutate(Model = "C2"),
      res_pz$Summary %>% mutate(Model = "PZ"),
      res_pc1$Summary %>% mutate(Model = "PC1")
    ) %>% relocate(Model, Var)
  )
  
  return(res)  
}


summarise_qol_bayes <- function(fit, vset) {
  #fit <- tar_read(pars_qol_b, 1)
  write_csv(fit$Summary, file = here::here("docs", "tabs", paste0("stats_qol_b_", vset, ".csv")))
  write_csv(fit$Ext, file = here::here("posteriors", paste0("post_qol_b_", vset, ".csv")))
  return(here::here("posteriors", paste0("post_qol_b_", vset, ".csv")))
}
