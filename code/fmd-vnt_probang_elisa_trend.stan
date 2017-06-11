data {
  // Define variables in data
  // Number of tested cattle (an integer)
  int<lower=0> Ne;
  int<lower=0> Np;
  int<lower=0>p;
  
  
  // covariates - estimation
  int<lower=0> hcode_est[Ne]; //herd id
  real age_est[Ne]; //age
  int<lower=0,upper=1> vnt_binary_est[Ne]; //elisa 
  int<lower=0,upper=1> probang_est[Ne]; //elisa 
  real<lower=0> elisa_obs_est[Ne]; //elisa 

  
  // covariates - prediction
  int<lower=0> hcode_pred[Np];
  real age_pred[Np];
  int<lower=0,upper=1> vnt_binary_pred[Np];
  int<lower=0,upper=1> probang_pred[Np];
  real<lower=0> elisa_obs_pred[Np]; //elisa 

  
  //herd related
  int<lower=0> He;
  int<lower=0> Hp;
  int<lower=0> herd_est[He]; //herd ids from the estimated data
  real<lower=0>  monlast_est[He]; //herd ids from the data to be predicted
  int<lower=0> herd_pred[Hp];
  
  //trend related
  real<lower=0> censorLimit;
  
}


parameters {
  // Define parameters to estimate
  real beta[2,p];
  real<lower=0,upper=censorLimit> monlast_pred[Hp];
  
   //elisa growth parameters - should probably also include covariates somehow. 
  real<lower=0> elisa_lambda_one;
  real<lower=0> elisa_lambda_two;
  real<lower=0>  elisa_lambda_three;
  real<lower=0> sigma; 
  
  //trend related
   real<lower=0,upper=1> a; //intercept
   real<lower=0> b_tmp; 

  
}

transformed parameters  {
  // Probability trasformation from linear predictor
  real<lower=0> odds_est[2,Ne];
  real<lower=0, upper=1> prob_est[2,Ne];
   real<lower=0> odds_pred[2,Np];
   real<lower=0> prob_pred[2,Np];
  
  real elisa_latent_est[Ne];
  real elisa_latent_pred[Np];
   real<lower=0> L[Np+Ne];
   real b; // slope
  
  //vnt binary model
  for (i in 1:Ne) {
    odds_est[1,i] = exp(beta[1,1] +beta[1,2]*monlast_est[hcode_est[i]] +beta[1,3]*age_est[i] + beta[1,4]*age_est[i]*age_est[i] +beta[1,5]*age_est[i]*monlast_est[hcode_est[i]]);
    
    prob_est[1,i] = odds_est[1,i] / (odds_est[1,i] + 1);
  }
  
    for (i in 1:Np) {
    odds_pred[1,i] = exp(beta[1,1] +beta[1,2]*monlast_pred[hcode_pred[i]] +beta[1,3]*age_pred[i] + beta[1,4]*age_pred[i]*age_pred[i] +beta[1,5]*age_pred[i]*monlast_pred[hcode_pred[i]]);
    
    prob_pred[1,i] = odds_pred[1,i] / (odds_pred[1,i] + 1);
  }
  
  //probang
    for (i in 1:Ne) {
    odds_est[2,i] = exp(beta[2,1] +beta[2,2]*monlast_est[hcode_est[i]] +beta[2,3]*age_est[i] + beta[2,4]*age_est[i]*age_est[i] +beta[2,5]*age_est[i]*monlast_est[hcode_est[i]]);
    
    prob_est[2,i] = odds_est[2,i] / (odds_est[2,i] + 1);
  }
  
    for (i in 1:Np) {
    odds_pred[2,i] = exp(beta[2,1] +beta[2,2]*monlast_pred[hcode_pred[i]] +beta[2,3]*age_pred[i] + beta[2,4]*age_pred[i]*age_pred[i] +beta[2,5]*age_pred[i]*monlast_pred[hcode_pred[i]]);
    
    prob_pred[2,i] = odds_pred[2,i] / (odds_pred[2,i] + 1);
  }


//elisa

  
  for (i in 1:Ne) {
    elisa_latent_est[i] <- elisa_lambda_one/(1 + exp((elisa_lambda_two - monlast_est[hcode_est[i]])/elisa_lambda_three));
    
  }
  
  for (i in 1:Np) {
    elisa_latent_pred[i] <-  elisa_lambda_one/(1 + exp((elisa_lambda_two - monlast_pred[hcode_pred[i]])/elisa_lambda_three));
  }
  
// trend related 

   b<-b_tmp-a/censorLimit;
  // print(a);
  // print(b);
  // print(b_tmp);
   
    for (i in 1:Np){
//     if(monlast_pred[hcode_pred[i]]<censorLimit){
 L[i]<-(a+b*monlast_pred[hcode_pred[i]])*exp(-(a+b*(monlast_pred[hcode_pred[i]])/2)*monlast_pred[hcode_pred[i]])/(1-exp(-(a*censorLimit+b*censorLimit^2/2)));
// }else{ 
//   L[i]<-1/10^6;
// }
//}
}
    for (i in 1:Ne){
//     if(monlast_pred[hcode_pred[i]]<censorLimit){
 L[i+Np]<-(a+b*monlast_est[hcode_est[i]])*exp(-(a+b*(monlast_est[hcode_est[i]])/2)*monlast_est[hcode_est[i]])/(1-exp(-(a*censorLimit+b*censorLimit^2/2)));
// }else{ 
//   L[i]<-1/10^6;
// }
//}
}
//print(sum(L[i]));
}


model {
    monlast_pred ~ exponential(0.1);
  // Prior part of Bayesian inference (flat if unspecified)
   
   ##Vnt binary
   beta[1,1]~cauchy(0,10);
   for(i in 2:p){
     beta[1,i] ~ cauchy(0,2.5);
   }
   vnt_binary_est ~ bernoulli(prob_est[1,1:Ne]);
   vnt_binary_pred ~ bernoulli(prob_pred[1,1:Np]);
   
   ##Probang
   beta[2,1]~cauchy(0,10);
   for(i in 2:p){
     beta[2,i] ~ cauchy(0,2.5);
   }
   probang_est ~ bernoulli(prob_est[2,1:Ne]);
   probang_pred ~ bernoulli(prob_pred[2,1:Np]);
   
   
  elisa_obs_est ~ normal(elisa_latent_est, sigma); 
  elisa_obs_pred ~ normal(elisa_latent_pred, sigma); 
  elisa_lambda_one ~ normal(100, 10); 
  elisa_lambda_two ~ normal(5, 1); 
  elisa_lambda_three ~ normal(5, 0.5); 
  sigma~exponential(0.1);  //probably want to change this prior...
  
  //trend 
    for(i in 1:(Np+Ne)){
   target += log(L[i]);
  // print(L[i]);
  }
 // print(sum(log(L)));
  
//print(b_tmp);
//print(b);
//print(a);

 b_tmp~normal(0, 0.1);
 a~beta(1,3);
  
}
