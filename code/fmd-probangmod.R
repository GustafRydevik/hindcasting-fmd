library(rstan)
library(tidyverse)

bronsvoort_training_data$Probang<-with(bronsvoort_training_data,(PbP.A+PbP.O+PbP.SAT2)>0)
bronsvoort_training_data$VNTAny<-with(bronsvoort_training_data,(FMDS_A+FMDS_O+FMDS_SAT2)>0)
with(bronsvoort_training_data,table(Probang,monlast))

summary(glm(Probang~monlast*age+I(age^2)+VNTAny,data=bronsvoort_training_data,family=binomial(link="logit")))


bronsvoort_training_data_clean<-bronsvoort_training_data[
  complete.cases(bronsvoort_training_data[c("Probang","monlast","age","VNTAny","FMD_cELISA")]),]


herd_monlast<-unique(bronsvoort_training_data_clean[c("hcode","monlast")])


###Splitting up the data into prediction and estimation
set.seed(1337)
herd_pred<-sort(sample(herd_monlast$hcode,15))
herd_est<-sort(setdiff(herd_monlast$hcode,herd_pred))

            
probang_df_est<-subset(bronsvoort_training_data_clean,hcode%in%herd_est)
herd_df_est<-subset(herd_monlast,hcode%in%herd_est)
herd_df_est$hcode_stan_est<-1:nrow(herd_df_est)
probang_df_est<-left_join(probang_df_est,herd_df_est,by="hcode")


probang_df_pred<-subset(bronsvoort_training_data_clean,hcode%in%herd_pred)
herd_df_pred<-subset(herd_monlast,hcode%in%herd_pred)
herd_df_pred$hcode_stan_pred<-1:nrow(herd_df_pred)
probang_df_pred<-left_join(probang_df_pred,herd_df_pred,by="hcode")



###Creating predictive and estimating data lists
with(probang_df_est,
     dat_est <<- list(
       probang = Probang,
       age      = age,
       vnt     = VNTAny,
       elisa_obs=FMD_cELISA,
       hcode = hcode_stan_est
     ))

names(dat_est)<-paste(names(dat_est),"_est",sep="")


herd_list_est<-list(
  He=nrow(herd_df_est),
  herd_est=herd_df_est$hcode_stan_est,
  monlast_est     = herd_df_est$monlast
)



dat_est<-c(dat_est,
           herd_list_est,
           list(Ne=nrow(probang_df_est)))

with(
  probang_df_pred,
  dat_pred <<- list(
    probang    = Probang,
    age      = age,
    vnt     = VNTAny,
    elisa_obs=FMD_cELISA,
    hcode = hcode_stan_pred
  )
)

names(dat_pred)<-paste(names(dat_pred),"_pred",sep="")

herd_list_pred<-list(
  Hp=nrow(herd_df_pred),
  herd_pred=herd_df_pred$hcode_stan_pred
)


dat_pred<-c(dat_pred,
           herd_list_pred,
           list(Np=nrow(probang_df_pred)))

##Creating separate 
dat_model<-c(dat_est,dat_pred,p=6)

fileName <- file.path(code.path,"fmd-probang-ELISA-mod.stan")
resStan <- stan(fileName, data = dat_model,
                chains =5, iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.99))

traceplot(resStan, pars = c("monlast_pred"), inc_warmup = TRUE)
pairs(resStan,pars=c("beta","elisa_lambda_one","elisa_lambda_two","elisa_lambda_three"))
monlast_pred_stan<-summary(resStan,pars="monlast_pred")$summary
ggplot(data.frame(monlast_pred_stan,monlast=herd_df_pred$monlast),aes(x=monlast,y=mean))+
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.))+geom_point(col="red")+geom_abline(slope=1,intercept=0,col="blue")



plot(herd_df_pred$monlast,monlast_pred_stan[,"mean"])
points(herd_df_pred$monlast,monlast_pred_stan[,"2.5%"],col="red")
points(herd_df_pred$monlast,monlast_pred_stan[,"97.5%"],col="red")
