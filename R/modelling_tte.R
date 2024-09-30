fit_tte <- function(model, dat_tte) {
  n_iter <- 20000
  n_collect <- 500
  n_warmup <- floor(n_iter - n_collect)
  n_chains <- 4
  
  ds <- local({
    d <- dat_tte %>% filter(T_evt > 0)
    list(
      Ts = d$T_evt2,
      As = d$Age,
      N = nrow(d)
    )
  })
  
  post <- sampling(model, data = ds, pars = c("r0", "ba1"), init = \() { list(r0 = exp(-2.35), ba1 = 0.01)},
                   chains = n_chains, iter = n_iter, warmup = n_warmup)
  
  res <- restructure_stan(post)
  return(res)
  
}


summarise_tte <- function(fit, file) {
  write_csv(fit$Summary, file = here::here("docs", "tabs", "fit_time2zero_age.csv"))
  write_csv(fit$Ext, file = here::here("posteriors", "post_time2zero_age.csv"))
  return(fit$Summary)
}
