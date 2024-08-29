data {
  int<lower=0> N;
  int<lower=1> M;
  real Ys[N];
  real As[N];
  real Ts[N];
  int IDs[N];
}

parameters {
  real b0;
  real b1;
  real b2;
  real rd[M];
  real<lower = 0> bt;
  real<lower=0> sigma;
}

model {
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  rd ~ normal(0, 10);
  bt ~ exponential(1);
  sigma ~ gamma(1, 1);
  
  for (i in 1:N) {
    target += normal_lpdf(logit(Ys[i])| b0 + b1 * As[i] + b2 * pow(As[i], 2) + bt * Ts[i] + rd[IDs[i]], sigma);
  }
}

