fit_qol_freq <- function(dat_qol) {
  require(dplyr)
  require(lme4)

  #dat_qol <- tar_read(data_qol, 2)
  
  dat_qol <- dat_qol %>% 
    filter(!is.na(Age))
  
  ds_nz <- dat_qol %>% filter(EQ5D < 1)
  
  
  clu <- kmeans(ds_nz$EQ5D, 2)
  
  if (clu$centers[1] > clu$centers[2]) {
    ds_nz <- ds_nz %>% 
      mutate(
        cluster = clu$cluster
      )
  } else {
    ds_nz <- ds_nz %>% 
      mutate(
        cluster = 3 - clu$cluster
      )
  }
  
  
  ds_c1 <- ds_nz %>% filter(cluster == 1)
  ds_c2 <- ds_nz %>% filter(cluster == 2)
  
  ds_zero <- dat_qol %>% 
    mutate(
      cluster = (EQ5D >= 1) + 0,
      d15 = (ti < 15 / 365.25),
      d30 = (ti < 30 / 365.25)
    )
  
  ds_clu1 <- ds_nz %>% 
    mutate(
      cluster = (cluster == 1) + 0,
      d15 = (ti < 15 / 365.25),
      d30 = (ti < 30 / 365.25)
    )
  
  ms <- list()
  ms$c1 <- lmer(EQ5D ~ 1 + (ti | SID), data = ds_c1)
  ms$c2 <- lmer(EQ5D ~ 1 + (ti | SID), data = ds_c2)
  ms$pz <- glmer(cluster ~ d15 + d30 + poly(Age, 2) + (1 | SID), data = ds_zero, family = binomial)
  ms$pc1 <-  glmer(cluster ~ d15 + d30 + (1 | SID), data = ds_clu1, family = binomial)
  
  dat_labelled <- dat_qol %>% 
    left_join(ds_nz %>% select(Key, cluster)) %>% 
    mutate(
      cluster = ifelse(is.na(cluster), 0, cluster)
    )

  
  res <- list(
    Models = ms,
    Labeled = dat_labelled
  )
  
  return(res)  
}


summarise_qol_freq <- function(fit, vset) {
  #fit <- tar_read(pars_qol_f, 1)
  
  stats <- fit[[2]] %>% 
    group_by(Agp, cluster) %>% 
    summarise(
      mu = mean(EQ5D),
      std = sd(EQ5D),
      N = n()
    ) %>% 
    group_by(Agp) %>% 
    mutate(
      Prop = N / sum(N)
    ) 
  
  ms <- fit[[1]]
  
  post <- bind_rows(
    summary(ms$pz)$coefficients %>% 
      data.frame() %>% as_tibble() %>% 
      select(Estimate, Std = `Std..Error`) %>% 
      mutate(Parameter = names(fixef(ms$pz)), Model = "Pr(C0)"),
    summary(ms$pc1)$coefficients %>% 
      data.frame() %>% as_tibble() %>% 
      select(Estimate, Std = `Std..Error`) %>% 
      mutate(Parameter = names(fixef(ms$pc1)), Model = "Pr(C1|~C0)"),
    summary(ms$c1)$coefficients %>% 
      data.frame() %>% as_tibble() %>% 
      select(Estimate, Std = `Std..Error`) %>% 
      mutate(Parameter = names(fixef(ms$c1)), Model = "C1"),
    summary(ms$c2)$coefficients %>%
      data.frame() %>% as_tibble() %>%
      select(Estimate, Std = `Std..Error`) %>% 
      mutate(Parameter = names(fixef(ms$c2)), Model = "C2")
  )
  
  write_csv(stats, file = here::here("docs", "tabs", paste0("stats_qol_f_", vset, ".csv")))
  write_csv(post, file = here::here("posteriors", paste0("post_qol_f_", vset, ".csv")))
  return(here::here("posteriors", paste0("post_qol_f_", vset, ".csv")))
}
