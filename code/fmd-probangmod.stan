data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;
  // Number of parameters
  int<lower=0> p;
  // Variables
  int<lower=0,upper=1> probang[N];
  int<lower=0,upper=1> hcode[N];
  int<lower=0,upper=1>  vnt[N];
  
  
  real<lower=0> age[N];
  
  //herd related
  int<lower=0> H;
  int<lower=0> herd[H];
  real<lower=0>  monlast[H];
  
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
    odds[i] = exp(beta[1] +beta[2]*monlast[HerdID[i]] +beta[3]*age[i] + beta[4]*age[i]*age[i] +beta[5]*age[i]*monlast[HerdID[i]] + beta[6]*vnt[i]);
    prob[i] = odds[i] / (odds[i] + 1);
  }
}

model {
  // Prior part of Bayesian inference (flat if unspecified)
   
   
   //probability of positive probang related to monlast 
   //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference
   probang ~ bernoulli(prob);
}
