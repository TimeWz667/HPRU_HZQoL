data {
  int<lower=0> N;
  real Ys[N];
  real Ts[N];
  
  real min_qol;
}

parameters {
  real b0;
  real b1;
  real<lower=0> sigma;
}

model {
  // b0 ~ normal(0, 10);
  // b1 ~ normal(0, 10);
  // sigma ~ gamma(1, 1);
  
  for (i in 1:N) {
    target += normal_lpdf(logit(Ys[i])| b0 + 0 * Ts[i], sigma);
  }
}

generated quantities {
  real qol0;
  
  qol0 = inv_logit(b0) * (1 - min_qol) + min_qol;
  
}
