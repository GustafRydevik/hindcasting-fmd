data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;
  int<lower=0>p;
  
  
  // covariates - estimation
  int<lower=0> hcode_est[Ne]; //herd id
  real age_est[Ne]; //age
  int<lower=0,upper=1> probang_est[Ne]; //elisa 
  
  // covariates - prediction
  int<lower=0> hcode_pred[Np];
  real age_pred[Np];
  int<lower=0,upper=1> probang_pred[Np];
  
  //herd related
  int<lower=0> He;
  int<lower=0> Hp;
  int<lower=0> herd_est[He]; //herd ids from the estimated data
  real<lower=0>  monlast_est[He]; //herd ids from the data to be predicted
  
  int<lower=0> herd_pred[Hp];
  //real<lower=0>  monlast_pred[Hp];
  
}


parameters {
  // Define parameters to estimate
  real beta[p];
  real<lower=0> monlast_pred[Hp];
}

transformed parameters  {
  // Probability trasformation from linear predictor
  real<lower=0> odds_est[Ne];
  real<lower=0, upper=1> prob_est[Ne];
   real<lower=0> odds_pred[Np];
   real<lower=0> prob_pred[Np];
   
  for (i in 1:Ne) {
    odds_est[i] = exp(beta[1] +beta[2]*monlast_est[hcode_est[i]] +beta[3]*age_est[i] + beta[4]*age_est[i]*age_est[i] +beta[5]*age_est[i]*monlast_est[hcode_est[i]]);
    prob_est[i] = odds_est[i] / (odds_est[i] + 1);
  }
  
    for (i in 1:Np) {
    odds_pred[i] = exp(beta[1] +beta[2]*monlast_pred[hcode_pred[i]] +beta[3]*age_pred[i] + beta[4]*age_pred[i]*age_pred[i] +beta[5]*age_pred[i]*monlast_pred[hcode_pred[i]]);
    prob_pred[i] = odds_pred[i] / (odds_pred[i] + 1);
  }
}

model {
    monlast_pred ~ exponential(0.1);
  // Prior part of Bayesian inference (flat if unspecified)
   beta ~ normal(0,log(10));
   //probability of positive probang related to monlast 
   //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference
   probang_est ~ bernoulli(prob_est);
   probang_pred ~ bernoulli(prob_pred);
}
