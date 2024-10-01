data {
  int<lower=0> N;
  real Ys[N];
  real Ts[N];
}

parameters {
  real b0;
  real bt;
  real<lower=0> sigma;
}

model {
  // b0 ~ normal(0, 10);
  // b1 ~ normal(0, 10);
  // sigma ~ gamma(1, 1);
  
  for (i in 1:N) {
    target += normal_lpdf(logit(Ys[i])| b0 + bt * Ts[i], sigma);
  }
}
