data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;



  // Variables - estimation
  int<lower=0,upper=1> probang_est[Ne];
  int<lower=0> hcode_est[Ne];
  int<lower=0,upper=1>  vnt_est[Ne];
  real<lower=0> age_est[Ne];
  real<lower=0> elisa_obs_est[Ne];
  
  //Variables - prediction
  int<lower=0,upper=1> probang_pred[Np];
  int<lower=0> hcode_pred[Np];
  int<lower=0,upper=1>  vnt_pred[Np];
  real<lower=0> age_pred[Np];
  real<lower=0> elisa_obs_pred[Np];
  
  //herd related
  int<lower=0> He;
  int<lower=0> Hp;
  int<lower=0> herd_est[He];
  real<lower=0>  monlast_est[He];
  
  int<lower=0> herd_pred[Hp];
  //real<lower=0>  monlast_pred[Hp];
  
}

parameters {
  // probang logistic regression pars
  real probang_lambda[5];
  real vnt_lambda[5];
  real<lower=0> monlast_pred[Hp];
  
  
  //growth parameters - should probably also include covariates somehow. 
  real elisa_lambda_one;
  real elisa_lambda_two;
  real<lower=.5,upper= 1>  elisa_lambda_three;
  real<lower=0> tau; 
  
}

transformed parameters  {
  // Probability trasformation from linear predictor
  real<lower=0> odds_est[Ne];
  real<lower=0, upper=1> prob_est[Ne];
   real<lower=0> odds_pred[Np];
   real<lower=0> prob_pred[Np];
  real sigma; 
  real<lower=0> elisa_latent_est[Ne];
  real<lower=0> elisa_latent_pred[Np];
  
  sigma <- 1 / sqrt(tau); 
  
  for (i in 1:Ne) {
    
    //probang
    probang_odds_est[i] = exp(probang_lambda[1] +probang_lambda[2]*monlast_est[hcode_est[i]] +probang_lambda[3]*age_est[i] + probang_lambda[4]*age_est[i]*age_est[i] +probang_lambda[5]*age_est[i]*monlast_est[hcode_est[i]] );
    probang_prob_est[i] = probang_odds_est[i] / (probang_odds_est[i] + 1);
    
    
    
    //vnt
        vnt_odds_est[i] = exp(vnt_lambda[1] +vnt_lambda[2]*monlast_est[hcode_est[i]] +vnt_lambda[3]*age_est[i] + vnt_lambda[4]*age_est[i]*age_est[i] +vnt_lambda[5]*age_est[i]*monlast_est[hcode_est[i]] );
    vnt_prob_est[i] = vnt_odds_est[i] / (vnt_odds_est[i] + 1);
    
    //elisa 
    
     elisa_latent_est[i] <- elisa_lambda_one - elisa_lambda_two* pow(elisa_lambda_three, monlast_est[hcode_est[i]]);
 
  }
  
    for (i in 1:Np) {
      
      //probang
    odds_pred[i] = exp(probang_lambda[1] +probang_lambda[2]*monlast_pred[hcode_pred[i]] +probang_lambda[3]*age_pred[i] + probang_lambda[4]*age_pred[i]*age_pred[i] +probang_lambda[5]*age_pred[i]*monlast_pred[hcode_pred[i]] + probang_lambda[6]*vnt_pred[i]);
    prob_pred[i] = odds_pred[i] / (odds_pred[i] + 1);
    
    
    //vnt
        odds_pred[i] = exp(probang_lambda[1] +probang_lambda[2]*monlast_pred[hcode_pred[i]] +probang_lambda[3]*age_pred[i] + probang_lambda[4]*age_pred[i]*age_pred[i] +probang_lambda[5]*age_pred[i]*monlast_pred[hcode_pred[i]] + probang_lambda[6]*vnt_pred[i]);
    prob_pred[i] = odds_pred[i] / (odds_pred[i] + 1);
    
    
   //ELISA growth curve - shamelessly stolen from http://www.magesblog.com/2015/10/non-linear-growth-curves-with-stan.html
    for (i in 1:Ne){
     elisa_latent_pred[i] <- elisa_lambda_one - elisa_lambda_two * pow(elisa_lambda_three, monlast_pred[hcode_pred[i]]);
 
  }
  
  
}



model {
  // Prior part of Bayesian inference (flat if unspecified)
   monlast_pred ~ uniform(0,120);
   
   //probability of positive probang related to monlast 
   //what we want is something like P(probang)~monlast; P(monlast)~Poisson(lambda), or other prior. 
  // Likelihood part of Bayesian inference
   probang_est ~ bernoulli(probang_prob_est);
   probang_pred ~ bernoulli(probang_prob_pred);
   
  vnt_est ~ bernoulli(vnt_prob_est);
  vnt_pred ~ bernoulli(vnt_prob_pred);
   
    
  elisa_obs_est ~ normal(elisa_latent_est, sigma); 
  elisa_obs_pred ~ normal(elisa_latent_pred, sigma); 
  elisa_lambda_one ~ normal(0.0, 1000); 
  elisa_lambda_two ~ normal(0.0, 1000); 
  elisa_lambda_three ~ uniform(.5, 1); 
  tau ~ gamma(.0001, .0001); 
   
   
}
