data {
  int<lower=0> N;
  int Ys[N];
  real Ts[N];
  int As[N];
}

parameters {
  real b0;
  real ba1;
}

transformed parameters {
  real<lower=0, upper=1> prob[N];
  
  for (i in 1:N) {
    prob[i] = exponential_cdf(Ts[i], exp(b0 + ba1 * As[i]));
  }
}

model {
  b0 ~ normal(0, 1);
  ba1 ~ normal(0, 1);
  
  for (i in 1:N) {
    target += binomial_lpmf(Ys[i] | 1, prob[i]);
  }
}
