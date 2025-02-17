
restructure_stan <- function(post) {
  su <- summary(post)$summary
  su <- data.frame(su) %>% 
    mutate(Var = row.names(su)) %>% 
    relocate(Var)
  rownames(su) <- NULL
  
  ext <- data.frame(rstan::extract(post))
  
  list(
    Ext = ext,
    Summary = su
  )
}
