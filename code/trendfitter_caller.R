library(tidyr)
monlast_density<-data.frame(density(bronsvoort_training_data_clean$monlast)[c("x","y")])

optim(par=c(a=1/17,b=0),fn=function(pars){
  a<-pars[1]
  b<-pars[2]
  t<-monlast_density$x
  y_est<-(a+b*t)*exp(-(a+b*t/2)*t)
  y_obs<-monlast_density$y
  resid<-sqrt(sum((y_obs-y_est^2)))
})
init.mean<-list(a=0.16,b_tmp=0.08/200)

nchains<-4
init.list<-vector(mode="list",length=4)

for(i in 1:nchains){
init.list[[i]]<-list(a=rnorm(1,init.mean[[1]],sd=init.mean[[1]]/10),
                b_tmp=rnorm(1,init.mean[[2]],sd=init.mean[[2]]/10))  
  
}
library(rstan)
resStan<-stan(file.path(code.path,"crossectional_trend.stan"),
     data=list(N=length(bronsvoort_training_data_clean$monlast),
               monlast=bronsvoort_training_data_clean$monlast),
     init = init.list,chains = nchains
    )

pairs(resStan,pars=c("a","b_tmp"))
summary(resStan,pars=c("a","b_tmp"))
slope_samples<-data.frame(rstan::extract(resStan,pars=c("a","b_tmp")))
slope_estimates<-apply(slope_samples,1,function(tmp)tmp[1]+1:150*(tmp[2]-tmp[1]/200))
slope_est_df<-gather(data.frame(
                                as.data.frame(slope_estimates)),
                     )
slope_est_df$month<-rep(1:150,4000)

slope_mean<-as.data.frame(summary(resStan,pars=c("a","b_tmp"))$summary)
ggplot(slope_est_df,aes(x=month,y=value,group=key))+
  geom_line(alpha=0.01)+
  geom_abline(intercept=slope_mean[1,1],slope=slope_mean[2,1]-slope_mean[1,1]/200,col="red")


plot(1:100,tmp[1]+1:150*(tmp[2]-tmp[1]/200))
plot(1:150,tmp[1]+1:150*(tmp[2]-tmp[1]/200))


##Validation data below
inf.times<-na.omit(EndemicLinear(n.infections = 1000,start.time=0,end.time=100,trend = 0.001/100,incidence=.01))
resStan<-stan(file.path(code.path,"crossectional_trend.stan"),
              data=list(N=1000,
                        monlast=inf.times),
              init = init.list,chains = nchains
)


summary(resStan,pars=c("a","b"))$summary[1:2,1]