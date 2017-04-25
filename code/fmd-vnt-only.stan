data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;
  
  
  // covariates - estimation
  int<lower=0> hcode_est[Ne]; //herd id
  real<lower=0> age_est[Ne]; //age
  real<lower=0> vnt_obs_est[Ne]; //elisa 
  
  // covariates - prediction
  int<lower=0> hcode_pred[Np];
  real<lower=0> age_pred[Np];
  real<lower=0> vnt_obs_pred[Np];
  
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
  
  
  //elisa growth parameters - should probably also include covariates somehow. 
  real<lower=0> decay_start;
  real<lower=0> decay_scale;
  real<lower=0> decay_asym;
  
  //real<lower=0> tau; 
  //real<lower=0> gamma_shape;
  //real<lower=0> gamma_scale;
  real<lower=0> sigma;
  
}

transformed parameters  {
  // Probability trasformation from linear predictor
 
  real vnt_latent_est[Ne];
  real vnt_latent_pred[Np];
  
  real<lower=0> monlast_censored_est[Ne];
  //sigma <- 1 / sqrt(tau); 
  
  for (i in 1:Ne) {
    
        if(monlast_est[hcode_est[i]]<age_est[i]*12){
      monlast_censored_est[i]= monlast_est[hcode_est[i]];
        }else{
          monlast_censored_est[i]=120;
       }   
    //elisa 
    
    vnt_latent_est[i] = decay_start*(exp(-monlast_censored_est[i]*decay_scale)+decay_asym);
    
  }
  
  for (i in 1:Np) {
    
    //ELISA growth curve - shamelessly stolen from http://www.magesblog.com/2015/10/non-linear-growth-curves-with-stan.html    
    
    vnt_latent_pred[i] = decay_start*(exp(-monlast_pred[hcode_est[i]]*decay_scale)+decay_asym);
 }
  
  
}



model {
  // Prior part of Bayesian inference (flat if unspecified)
  monlast_pred ~ exponential(0.01);
  
  //probability of positive probang related to monlast 
  //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference

  for(i in 1:Np){
  vnt_obs_est[i] ~ lognormal(log(vnt_latent_est[i]),sigma); 
  vnt_obs_pred[i] ~ lognormal(log(vnt_latent_pred[i]),sigma); 
  
  //vnt_obs_est[i] ~ gamma(gamma_shape,gamma_scale*vnt_latent_est[i]); 
  //vnt_obs_pred[i] ~ gamma(gamma_shape,gamma_scale*vnt_latent_pred[i]); 
  }
  //gamma_shape ~ cauchy(0,2.5);
  //gamma_scale ~ cauchy(0,2.5);
  sigma ~ cauchy(0,2.5);
  decay_start ~ cauchy(0,2.5);
  #decay_scale ~ cauchy(0,2.5);
  decay_scale ~ exponential(100);
  
  decay_asym ~ cauchy(0,2.5);
}
