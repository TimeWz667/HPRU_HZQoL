data {
  int<lower=0> N;
  real Ys[N];
  real As[N];
}

parameters {
  real b0;
  real ba1;
  real ba2;
  real<lower=0> sigma;
}

model {
  b0 ~ normal(0, 1);
  ba1 ~ normal(0, 1);
  ba2 ~ normal(0, 1);
  sigma ~ gamma(1, 1);
  
  for (i in 1:N) {
    target += normal_lpdf(logit(Ys[i])| b0 + ba1 * As[i] + ba2 * pow(As[i], 2), sigma);
  }
}

