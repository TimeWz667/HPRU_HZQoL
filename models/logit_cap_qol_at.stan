data {
  int<lower=0> N;
  real Qs[N];
  int Zs[N];
  real As[N];
  real Ts[N];
  real Cs[N]; 
}

transformed data {
  real logit_Cs[N] = logit(Cs);
}

parameters {
  real b0;
  real ba1;
  real ba2;
  real bta1;
  real bta2;
  real<lower=0> bt;
  real<lower=0> sigma;
}

transformed parameters {
  real mu[N];
  
  for (i in 1:N) {
   mu[i] = b0 + ba1 * As[i] + ba2 * pow(As[i], 2) +  (bt + bta1 * As[i] + bta2 * pow(As[i], 2)) * Ts[i];
  }
}

model {
  ba1 ~ normal(0, 1);
  ba2 ~ normal(0, 1);
  // b0 ~ normal(0, 10);
  // b1 ~ normal(0, 10);
  // sigma ~ gamma(1, 1);
  
  for (i in 1:N) {
    target += binomial_lpmf(Zs[i] | 1, 1 - normal_cdf(logit_Cs[i], mu[i], sigma));
    if (Zs[i] == 0) {
      target += normal_lpdf(logit(Qs[i])| mu[i], sigma);
    }
  }
}

