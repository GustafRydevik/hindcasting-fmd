data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;
  
  
  
  // covariates - estimation
  int<lower=0> hcode_est[Ne]; //herd id
  real<lower=0> age_est[Ne]; //age
  real<lower=0> elisa_obs_est[Ne]; //elisa
  
  // covariates - prediction
  int<lower=0> hcode_pred[Np];
  real<lower=0> age_pred[Np];
  real<lower=0> elisa_obs_pred[Np];
  
  //herd related
  int<lower=0> He;
  int<lower=0> Hp;
  int<lower=0> herd_est[He]; //herd ids from the estimated data
  real<lower=0>  monlast_est[He]; //herd ids from the data to be predicted
  
  int<lower=0> herd_pred[Hp];
  //real<lower=0>  monlast_pred[Hp];
  
}

parameters {
  //  logistic regression pars
  real<lower=0> monlast_pred[Hp];
  real<lower=0> ind_monlast_est[Ne];
  real<lower=0> ind_monlast_pred[Np];
  
  
  //elisa growth parameters - should probably also include covariates somehow. 
  real<lower=0> elisa_lambda_one;
  real<lower=0> elisa_lambda_two;
  real<lower=0>  elisa_lambda_three;
  //real<lower=0> tau; 
  real<lower=0> sigma; 
  
}

transformed parameters  {
  // Probability trasformation from linear predictor
 
  real elisa_latent_est[Ne];
  real elisa_latent_pred[Np];
  
  //sigma <- 1 / sqrt(tau); 
  
  for (i in 1:Ne) {
    
       
    //elisa 
    
    elisa_latent_est[i] <- elisa_lambda_one/(1+exp((elisa_lambda_two-ind_monlast_est[i])/elisa_lambda_three));
    monlast_est[hcode_est[i]]
    
  }
  
  for (i in 1:Np) {
    
    //ELISA growth curve - shamelessly stolen from http://www.magesblog.com/2015/10/non-linear-growth-curves-with-stan.html
    elisa_latent_pred[i] <- elisa_lambda_one - elisa_lambda_two * pow(elisa_lambda_three, monlast_pred[hcode_pred[i]]);
    
  }
  
  
}



model {
  // Prior part of Bayesian inference (flat if unspecified)
  monlast_pred ~ exponential(0.01);
  
  //probability of positive probang related to monlast 
  //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference

  monlast_est[i]
  infected
  
  elisa_obs_est ~ normal(elisa_latent_est, sigma); 
  elisa_obs_pred ~ normal(elisa_latent_pred, sigma); 
  elisa_lambda_one ~ normal(100, 10); 
  elisa_lambda_two ~ normal(5, 1); 
  elisa_lambda_three ~ normal(5, 0.5); 
  sigma~exponential(0.1);
  
  
}
