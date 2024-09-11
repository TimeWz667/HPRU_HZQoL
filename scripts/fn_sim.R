
sim_t2z <- \(df, pars) {
  n_df <- length(unique(df$Key))
  n_pars <- nrow(pars)
  
  pars <- pars %>% 
    mutate(Key = sample.int(n())) %>% 
    arrange(Key) %>% 
    filter(Key <= n_df) 
  
  sim <- df %>% 
    left_join(pars, by = "Key") %>% 
    mutate(
      r2z = exp(b0 + ba1 * Age),
      t2z = 1 / r2z,
      p_z = pexp(Ti, r2z)
    ) %>% 
    select(Key, Age, Ti, r2z, t2z, p_z)
  
  return(df %>% left_join(sim, by = c("Key", "Age", "Ti")))
}


sim_qol_a <- \(df, pars, q0) {
  n_df <- length(unique(df$Key))
  n_pars <- nrow(pars)
  
  pars <- pars %>% 
    mutate(Key = sample.int(n())) %>% 
    arrange(Key) %>% 
    filter(Key <= n_df) 
  
  sim <- df %>% 
    left_join(pars, by = "Key") %>% 
    mutate(
      logit_q = b0 + ba1 * Age + ba2 * Age ^ 2,
      qol = 1 / (1 + exp(- logit_q)),
      qol = qol * (1 - min_qol) + min_qol
    ) %>% 
    select(Key, Age, Ti, qol)
  
  return(df %>% left_join(sim, by = c("Key", "Age", "Ti")))
}


sim_qol_at <- \(df, pars, q0) {
  n_df <- length(unique(df$Key))
  n_pars <- nrow(pars)
  
  pars <- pars %>% 
    mutate(Key = sample.int(n())) %>% 
    arrange(Key) %>% 
    filter(Key <= n_df) 
  
  sim <- df %>% 
    left_join(pars, by = "Key") %>% 
    mutate(
      logit_q = b0 + ba1 * Age + ba2 * Age ^ 2 + bt * Ti,
      qol = 1 / (1 + exp(- logit_q)),
      qol = qol * (1 - min_qol) + min_qol
    ) %>% 
    select(Key, Age, Ti, qol)
  
  return(df %>% left_join(sim, by = c("Key", "Age", "Ti")))
}


sim_cap_qol_at <- \(df, pars, q0, norm) {
  n_df <- length(unique(df$Key))
  n_pars <- nrow(pars)
  
  pars <- pars %>% 
    mutate(Key = sample.int(n())) %>% 
    arrange(Key) %>% 
    filter(Key <= n_df) 
  
  sim <- df %>% 
    left_join(pars, by = "Key") %>% 
    left_join(norm, by = "Age") %>% 
    mutate(
      logit_q = b0 + ba1 * Age + ba2 * Age ^ 2 + (bt + bta1 * Age + bta2 * Age ^ 2) * Ti,
      qol = 1 / (1 + exp(- logit_q)),
      qol = qol * (1 - min_qol) + min_qol,
      p_z = pnorm(logit_q, log(Norm_rescaled / (1 - Norm_rescaled)), sigma)
    ) %>% 
    select(Key, Age, Ti, qol, p_z)
  
  return(df %>% left_join(sim, by = c("Key", "Age", "Ti")))
}



calc_shortfall <- \(proj, pars_demo) {
  n_sim <- length(unique(proj$Key))
  
  sim <- proj %>% 
    mutate(
      Age0 = Age,
      Age = Age0 + floor(Ti)
    ) %>% 
    left_join(pars_demo, by = "Age") %>% 
    mutate(
      ql = (1 - p_z) * pmax(norm - qol, 0),
      qlh = (1 - p_z) * pmax(1 - qol, 0)
    ) %>% 
    group_by(Key, Age, Age0) %>% 
    summarise(ql = mean(ql), qlh = mean(qlh)) %>%
    ungroup() %>% 
    mutate(Ti = Age - Age0, Age = Age0) %>% 
    select(Key, Age, Ti, ql, qlh) %>% 
    arrange(Key, Age, Ti)
  
  
  crossing(Key = 1:n_sim, Age = 50:99, AgeT = 50:99) %>% 
    mutate(
      Ti = AgeT - Age
    ) %>% 
    filter(Ti >= 0) %>% 
    left_join(sim, c("Key", "Age", "Ti")) %>% 
    mutate(ql = ifelse(is.na(ql), 0, ql), qlh = ifelse(is.na(qlh), 0, qlh)) %>% 
    left_join(pars_demo %>% rename(AgeT = Age), by = "AgeT") %>% 
    group_by(Key, Age) %>% 
    mutate(
      qol = norm - ql,
      qolh = 1 - qlh,
      surv = c(1, cumprod(1 - mr)[-n()]),
      d15 = (1 + 0.015) ^ -Ti,
      d35 = (1 + 0.035) ^ -Ti
    ) %>% 
    summarise(
      LE = sum(surv),
      QALE1 = sum(qol * surv),
      QALE0 = sum(norm * surv),
      QL00 = sum((norm - qol) * surv),
      QL15 = sum((norm - qol) * surv * d15),
      QL35 = sum((norm - qol) * surv * d35),
      QLH00 = sum((1 - qolh) * surv),
      QLH15 = sum((1 - qolh) * surv * d15),
      QLH35 = sum((1 - qolh) * surv * d35)
    ) %>% 
    ungroup()
}
