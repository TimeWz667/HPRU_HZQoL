library(tidyverse)
library(survival)

theme_set(theme_bw())



## Load data
for (vs in c("orig", "uk")) {
  vs <- glue::as_glue(vs)
  
  
  load(here::here("data", "qol_" + vs + "_set.rdata"))
  
  
  dat_cluster <- local({
    dat <- dat_qol %>% 
      arrange(PID, ti) %>% 
      group_by(PID) %>% 
      mutate(
        loss = 1 - EQ5D,
        End = cumsum(loss) == sum(loss) & loss == loss[n()],
        EndZeros = End * (loss == 0)
      ) %>% 
      filter(EndZeros == 0) %>% 
      ungroup() %>% 
      select(PID, Age, Agp, EQ5D, loss) %>% 
      mutate(
        Key = 1:n()
      )
    
    dat %>% 
      left_join(
        dat %>% 
          filter(EQ5D != 1) %>% 
          mutate(
            Cluster = kmeans(EQ5D, 2)$cluster
          )
      ) %>% 
      mutate(
        Cluster = ifelse(is.na(Cluster), 0, Cluster),
        Cluster = as.character(Cluster)
      )
    
  })
  
  
  dat_cluster %>% 
    ggplot() +
    geom_histogram(aes(x = EQ5D, fill = Cluster), alpha = 0.8)
  
  
  
  stats_cluster_Agp <- dat_cluster %>% 
    group_by(Agp, Cluster) %>% 
    summarise(
      mu = mean(EQ5D),
      std = sd(EQ5D),
      N = n()
    ) %>% 
    group_by(Agp) %>% 
    mutate(
      Prop = N / sum(N)
    ) 
  
  stats_cluster_Agp %>% 
    ggplot() +
    geom_bar(aes(x = Agp, y = Prop, fill = Cluster), stat = "identity", position = position_stack())
  
  
  stats_cluster_All <- dat_cluster %>% 
    group_by(Cluster) %>% 
    summarise(
      mu = mean(EQ5D),
      std = sd(EQ5D),
      N = n()
    ) %>% 
    mutate(
      Prop = N / sum(N)
    ) 
  
  
  stats_cluster <- bind_rows(
    stats_cluster_Agp,
    stats_cluster_All %>% 
      mutate(Agp = "All")
    
  )
  
  
  write_csv(stats_cluster, here::here("posteriors", "stats_cluster_" + vs + ".csv"))
  
  
  
  
  boot_cluster <- stats_cluster %>%
    filter(Agp == "All") %>% 
    crossing(Key = 1:2000) %>% 
    mutate(
      Q = rnorm(n(), mu, std),
      Q = pmin(Q, 1)
    ) %>% 
    relocate(Key) %>% 
    select(Key, Q, Prop, Cluster)
  
  write_csv(boot_cluster, here::here("posteriors", "boot_cluster_" + vs + ".csv"))
  
}
