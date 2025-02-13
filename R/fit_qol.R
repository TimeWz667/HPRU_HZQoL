fit_qol_freq <- function(dat_qol) {
  require(dplyr)
  require(lme4)

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
  
  return(ms)  
}


simulate_shortfall_alt <- function() {
  
}
