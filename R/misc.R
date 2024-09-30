amlu <- list(
  A = mean,
  M = median,
  L = \(x) quantile(x, 0.025),
  U = \(x) quantile(x, 0.975)
)
