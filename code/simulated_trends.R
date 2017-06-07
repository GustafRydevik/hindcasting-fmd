npop<-1000
slope<-(slope_mean[2,1]-slope_mean[1,1]/200)
pop_df<-expand.grid(id=1:npop,time=1:120)
pop_df$incidence_constant<-0.05
pop_df$incidence_increase<-0.07+pop_df$time*(slope_mean[2,1]-slope_mean[1,1]/120)
pop_df$incidence_decrease<-0.07-pop_df$time*(slope_mean[2,1]-slope_mean[1,1]/120)

pop_df$infected_constant<-rbinom(nrow(pop_df),1,pop_df$incidence_constant)
pop_df$infected_increase<-rbinom(nrow(pop_df),1,pop_df$incidence_increase)
pop_df$infected_decrease<-rbinom(nrow(pop_df),1,pop_df$incidence_decrease)

pop_df%>%group_by(id)%>%filter(infected_constant==1)%>%
  summarise(earliest_inf=min(time))->pop_constant


pop_df%>%group_by(id)%>%filter(infected_increase==1)%>%
  summarise(earliest_inf=min(time))->pop_increase

pop_df%>%group_by(id)%>%filter(infected_decrease==1)%>%
  summarise(earliest_inf=min(time))->pop_decrease



for(i in 1:nchains){
  init.list[[i]]<-list(a=rnorm(1,init.mean[[1]],sd=init.mean[[1]]/10),
                       b_tmp=rnorm(1,init.mean[[2]]/200*120,sd=init.mean[[2]]/10))  
  
}



resStan_constant<-stan(file.path(code.path,"crossectional_trend.stan"),
              data=list(N=length(pop_constant$earliest_inf),
                        monlast=pop_constant$earliest_inf,censorLimit=120),
              init = init.list,chains = nchains,cores = 4
)
est_constant<-as.data.frame(summary(resStan_constant,pars=c("a","b"))$summary)

plot(0:150,seq(0,0.2,length.out = 151),col="white")
abline(est_constant[1,1],est_constant[2,1],col="red")
abline(est_constant[1,4],est_constant[2,4],col="red")
abline(est_constant[1,8],est_constant[2,8],col="red")

resStan_increase<-stan(file.path(code.path,"crossectional_trend.stan"),
              data=list(N=length(pop_increase$earliest_inf),
                        monlast=pop_increase$earliest_inf,censorLimit=120),
              init = init.list,chains = nchains,cores = 4
)


resStan_decrease<-stan(file.path(code.path,"crossectional_trend.stan"),
                       data=list(N=length(pop_decrease$earliest_inf),
                                 monlast=pop_decrease$earliest_inf,censorLimit=120),
                       init = init.list,chains = nchains,cores = 4
)


est_constant<-as.data.frame(summary(resStan_constant,pars=c("a","b"))$summary)

plot(0:150,seq(0,0.2,length.out = 151),col="white")
abline(est_constant[1,1],est_constant[2,1],col="red")
abline(est_constant[1,4],est_constant[2,4],col="red")
abline(est_constant[1,8],est_constant[2,8],col="red")

est_increase<-as.data.frame(summary(resStan_increase,pars=c("a","b"))$summary)
est_decrease<-as.data.frame(summary(resStan_decrease,pars=c("a","b"))$summary)

abline(est_increase[1,1],est_increase[2,1],col="blue")
abline(est_increase[1,4],est_increase[2,4],col="blue")
abline(est_increase[1,8],est_increase[2,8],col="blue")



abline(est_decrease[1,1],est_decrease[2,1],col="purple")
abline(est_decrease[1,4],est_decrease[2,4],col="purple")
abline(est_decrease[1,8],est_decrease[2,8],col="purple")


## censoring visualisation
library(ggExtra)

p_Infections<-ggplot(subset(pop_df,id<201&time<50&infected_constant==1),aes(x=time,y=id,group=id))+geom_point()
ggMarginal(p_Infections,type="histogram",margins="x")

p_connectedInfections<-ggplot(subset(pop_df,id<201&time<50&infected_constant==1),aes(x=time,y=id,group=id))+geom_line()+geom_point()
ggMarginal(p_connectedInfections,type="histogram",margins="x")

p_censoredInfections<-ggplot(subset(pop_df,id<201&time<50&infected_constant==1),aes(x=time,y=id,group=id))+geom_line(alpha=0.5)+geom_point(alpha=0.5)+
  geom_point(data=
               (subset(pop_df,id<201&time<50&infected_constant==1)%>%group_by(id)%>%filter(time==max(time))),
             col="red")

ggMarginal(p_censoredInfections,data=(subset(pop_df,id<201&time<50&infected_constant==1)%>%group_by(id)%>%filter(time==max(time))),
           type="histogram",margins="x")


ggplot(subset(pop_df,id<21&time<50&infected_constant==1),aes(x=time,y=id,group=id))+geom_line(alpha=0.5)+geom_point(alpha=0.5)+
  geom_vline(xintercept = seq(0,50,by=5),col="red")


