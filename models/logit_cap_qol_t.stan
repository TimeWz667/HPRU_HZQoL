data {
  int<lower=0> N;
  real Qs[N];
  int Zs[N];
  real Ts[N];
  real Cs[N]; 
}

transformed data {
  real logit_Cs[N] = logit(Cs);
}

parameters {
  real b0;
  real<lower=0> bt;
  real<lower=0> sigma;
}

transformed parameters {
  real mu[N];
  
  for (i in 1:N) {
   mu[i] = b0 + bt * Ts[i];
  }
}

model {
  for (i in 1:N) {
    target += binomial_lpmf(Zs[i] | 1, 1 - normal_cdf(logit_Cs[i], mu[i], sigma));
    if (Zs[i] == 0) {
      target += normal_lpdf(logit(Qs[i])| mu[i], sigma);
    }
  }
}

