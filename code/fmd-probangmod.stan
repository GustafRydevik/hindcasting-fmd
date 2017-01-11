data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> N;
  // Number of parameters
  int<lower=0> p;
  // Variables
  int<lower=0,upper=1> probang[N];
  real<lower=0>  monlast[N];
  int<lower=0,upper=1>  vnt[N];
  real<lower=0> age[N];
}

parameters {
  // Define parameters to estimate
  real beta[p];
}

transformed parameters  {
  // Probability trasformation from linear predictor
  real<lower=0> odds[N];
  real<lower=0, upper=1> prob[N];

  for (i in 1:N) {
    odds[i] = exp(beta[1] +beta[2]*monlast[i] +beta[3]*age[i] + beta[4]*age[i]*age[i] +beta[5]*age[i]*monlast[i] + beta[6]*vnt[i]);
    prob[i] = odds[i] / (odds[i] + 1);
  }
}

model {
  // Prior part of Bayesian inference (flat if unspecified)

  // Likelihood part of Bayesian inference
   probang ~ bernoulli(prob);
}
