data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;

  // Variables
  int<lower=0> hcode_est[Ne];
  int<lower=0> hcode_pred[Np];
  
  int K; //the number of columns in the model matrix
  real y_est[Ne]; //the response
  real y_pred[Np]; //the response
  matrix[Ne,K] design_matrix_est; //the model matrix
  matrix[Np,K] design_matrix_pred; //the model matrix
  
  
  //herd related
  int<lower=0> He;
  int<lower=0> Hp;
  int<lower=0> herd_est[He];
  real<lower=0>  monlast_est[He];
  
  int<lower=0> herd_pred[Hp];
  //real<lower=0>  monlast_pred[Hp];
  
}

parameters {
  // Define parameters to estimate
  real beta[2];

  real<lower=0> monlast_pred[Hp];
}

transformed parameters  {
  // Probability trasformation from linear predictor
  real<lower=0> odds_est[Ne];
  real<lower=0, upper=1> prob_est[Ne];
  real<lower=0> odds_pred[Np];
  real<lower=0> prob_pred[Np];

    odds_est = exp(design_matrix_est*beta);
    prob_est = odds_est / (odds_est + 1);
  
      odds_pred[i] = exp(design_matrix_pred*beta);
    prob_pred[i] = odds_pred[i] / (odds_pred[i] + 1);

}

model {
  // Prior part of Bayesian inference (flat if unspecified)
   monlast_pred ~ uniform(0,120);
  
  beta[1]~cauchy(0,10);
   for(i in 2:p){
     beta[i] ~ cauchy(0,2.5);
   }
   //probability of positive probang related to monlast 
   //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference
   y_est ~ bernoulli(prob_est);
   y_pred ~ bernoulli(prob_pred);
}
