##This jags file assumes a constant incidence of cases

data {
  int N;
  real<lower=0> monlast[N];
  real<lower=0> censorLimit;
}

parameters{
  real<lower=0,upper=1> a; //intercept
 real<lower=0> b_tmp; 
  
}

transformed parameters{
  real L[N];
   real b; // slope
   b<-b_tmp-a/censorLimit;
   
  for (i in 1:N){
    if(monlast[i]<censorLimit){
L[i]<-(a+b*monlast[i])*exp(-(a+b*(monlast[i])/2)*monlast[i])/(1-exp(-(a*censorLimit+b*censorLimit^2/2)));
}else{ 
  L[i]<-0;
}

//print(L[i]);
//- removing normalisation for now... Likely to not be too important /(1-exp(-(incidence*censorLimit+trend*censorLimit^2/2)))*step(censorLimit-InfTime[i]
}

}

model {
  
  for(i in 1:N){
    
    target += log(L[i]);
  }
  #print(sum(log(L)));
  
//print(b_tmp);
//print(b);
//print(a);

 b_tmp~normal(0, 0.1);
 a~uniform(0,1);
  
  
}




