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
init.mean<-list(a=0.07,b_tmp=0.004/200)

nchains<-5
init.list<-vector(mode="list",length=nchains)

for(i in 1:nchains){
init.list[[i]]<-list(a=rnorm(1,init.mean[[1]],sd=init.mean[[1]]/10),
                b_tmp=rnorm(1,init.mean[[2]],sd=init.mean[[2]]/10))  
  
}
library(rstan)
resStan<-stan(file.path(code.path,"crossectional_trend.stan"),
     data=list(N=length(herd_monlast$monlast),
               monlast=herd_monlast$monlast,censorLimit=121),
     init = init.list,chains = nchains
    )

pairs(resStan,pars=c("a","b_tmp"))
summary(resStan,pars=c("a","b"))
slope_samples<-data.frame(rstan::extract(resStan,pars=c("a","b_tmp")))
slope_samples$b<-slope_samples$b_tmp-slope_samples$a/120
slope_estimates<-apply(slope_samples,1,function(tmp)tmp[1]+1:120*(tmp[2]-tmp[1]/120))
slope_est_df<-gather(data.frame(
                                as.data.frame(slope_estimates)),
                     )
slope_est_df$month<-rep(1:120,nchains*1000)
slope_est_df%<>%group_by(key)%>%mutate(sign=sign(mean(diff(value))))

slope_mean<-as.data.frame(summary(resStan,pars=c("a","b_tmp"))$summary)
ggplot(slope_est_df,aes(x=month,y=value,group=key,col=factor(sign)))+
  geom_line(alpha=0.01)+
  geom_abline(intercept=slope_mean[1,1],slope=-slope_mean[2,1]+slope_mean[1,1]/150,col="red")+
  scale_x_reverse()+ggtitle("Estimated incidence trend of FMD incursions")+
  theme_grey(base_size=24)+scale_color_discrete()
  xlab("Months since incursion")+ylab("incidence per month")
  


plot(1:100,tmp[1]+1:150*(tmp[2]-tmp[1]/200))
plot(1:150,tmp[1]+1:150*(tmp[2]-tmp[1]/200))


##Validation data below
inf.times<-na.omit(EndemicLinear(n.infections = 1000,start.time=0,end.time=100,trend = 0.001/100,incidence=.01))
inf.times<-na.omit(EndemicLinear(n.infections = 1000,start.time=0,end.time=150,trend = colMeans(slope_samples)[3],incidence=colMeans(slope_samples)[1]))
inf.times.constant<-na.omit(EndemicLinear(n.infections = 1000,start.time=0,end.time=150,trend = 0,incidence=1/mean(bronsvoort_training_data_clean$monlast)))

resStan<-stan(file.path(code.path,"crossectional_trend.stan"),
              data=list(N=100,
                        monlast=inf.times,censorLimit=150),
              init = init.list,chains = nchains
)


summary(resStan,pars=c("a","b"))$summary[1:2,1]
