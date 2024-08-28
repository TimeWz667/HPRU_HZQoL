data {
  int<lower=0> N;
  int Ys[N];
  real Ts[N];
  int As[N];
}

parameters {
  real b0;
  real b1;
}

transformed parameters {
  real<lower=0, upper=1> prob[N];
  
  for (i in 1:N) {
    prob[i] = exponential_cdf(Ts[i], exp(b0 + b1 * As[i]));
  }
}

model {
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  
  for (i in 1:N) {
    target += binomial_lpmf(Ys[i] | 1, prob[i] * 0.9999);
  }
}

generated quantities {
  real rate_age[100];
  
  for (age in 1:100) {
    rate_age[age] = exp(b0 + b1 * age);
  }
  
}


