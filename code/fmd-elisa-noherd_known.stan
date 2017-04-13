data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Np;
  
  


  real<lower=0> elisa_obs_pred[Np];
  
  
  //elisa growth parameters - should probably also include covariates somehow. 
  real<lower=0> elisa_lambda_one;
  real<lower=0> elisa_lambda_two;
  real<lower=0>  elisa_lambda_three;
  //real<lower=0> tau; 
  real<lower=0> sigma; 
}

parameters {
  //  logistic regression pars
 real<lower=0>  monlast_pred[Np]; 
 
  
  
}

transformed parameters  {
  // Probability trasformation from linear predictor
 
  real elisa_latent_pred[Np];
  
  //sigma <- 1 / sqrt(tau); 
  

  for (i in 1:Np) {
    
    //ELISA growth curve - shamelessly stolen from http://www.magesblog.com/2015/10/non-linear-growth-curves-with-stan.html
    elisa_latent_pred[i] <-  elisa_lambda_one/(1 + exp((elisa_lambda_two - monlast_pred[i])/elisa_lambda_three));
    
  }
  
  
}



model {
  // Prior part of Bayesian inference (flat if unspecified)
  monlast_pred ~ exponential(0.01);
  
  //probability of positive probang related to monlast 
  //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference

  elisa_obs_pred ~ normal(elisa_latent_pred, sigma); 
 // elisa_lambda_one ~ normal(100, 10); 
 // elisa_lambda_two ~ normal(5, 1); 
 // elisa_lambda_three ~ normal(5, 0.5); 
 // sigma~exponential(0.1);
  
  
}
