data {
  int<lower=0> N;
  real Ts0[N];
  real Ts1[N];
  
  int<lower=0> N_Cen;
  real Ts_Cen[N_Cen];
}

parameters {
  real<lower=0> rate;
}

transformed parameters {
  real<lower=0, upper=1> prob[N];
  
  for (i in 1:N) {
    prob[i] = exponential_cdf(Ts1[i], rate) - exponential_cdf(Ts0[i], rate);
  }
  
  
  
}

model {
  rate ~ exponential(1);
  
  for (i in 1:N) {
    target += log(prob);
  }
  for (i in 1:N_Cen) {
    target += exponential_lccdf(Ts_Cen[i] | rate);
  }
}
