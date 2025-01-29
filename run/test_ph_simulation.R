library(survival)


## Inverse Survival function
gen_fn_p <- function(kappa = 0, p_dens_fn) {
  ps <- seq(0, 1, length.out = 1e4)
  ps <- ps[-length(ps)]
  pow <- exp(kappa)
  ys <- sapply(ps, \(p) {
    nlminb(1, \(x) (1 - (1 - p_dens_fn(x)) ^ pow - p) ^ 2, lower = 0)$par
  }) 
  
  approxfun(ps, ys)
}

## generate pdf with different offsets
fn_p0 <- gen_fn_p(0, \(x) pgamma(x, 2, 0.5))
fn_p1 <- gen_fn_p(1, \(x) pgamma(x, 2, 0.5))
fn_p2 <- gen_fn_p(-1, \(x) pgamma(x, 2, 0.5))


## test simulation
xs <- rgamma(1000, 2, 0.5)
ps <- rev(1:1000)/ 1000
plot(sort(xs), ps, col = scales::alpha(2, 0.03))
lines(fn_p0(ps), rev(ps), col = 3, lwd  = 5)
lines(qgamma(ps, 2, 0.5), rev(ps))

lines(fn_p1(ps), rev(ps), col = 4, lwd  = 5)
lines(fn_p2(ps), rev(ps), col = 5, lwd  = 5)


## test model fitting
sims <- bind_rows(
  tibble(time = fn_p0(runif(300)), x = 0),
  tibble(time = fn_p1(runif(300)), x = 5),
  tibble(time = fn_p2(runif(300)), x = -5)
)

fit1 <- coxph(Surv(time) ~ x, data = sims)

### baseline hazards
plot(basehaz(fit1, data.frame(x = 0)), type = "l")
lines(basehaz(fit1, data.frame(x = 5)), col = 2)
lines(basehaz(fit1, data.frame(x = -5)), col = 3)

### baseline hazards centered
plot(basehaz(fit1, data.frame(x = 0)), type = "l")
lines(basehaz(fit1, data.frame(x = 5)) %>% mutate(hazard = hazard * exp(-5 * coef(fit1))), col = 2)
lines(basehaz(fit1, data.frame(x = -5)) %>% mutate(hazard = hazard * exp(5 * coef(fit1))), col = 3)



