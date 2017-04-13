
output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

#bronsvoort_training_data



bronsvoort_training_data_clean<-bronsvoort_training_data_clean[bronsvoort_training_data_clean$FMD_cELISA!=0,]

bronsvoort_training_data_clean<-bronsvoort_training_data_clean[!is.na(bronsvoort_training_data_clean$FMD_VNT_O),]

###Splitting up the data into prediction and estimation
set.seed(1337)
herd_pred<-sort(sample(herd_monlast$hcode,60))
herd_est<-sort(setdiff(herd_monlast$hcode,herd_pred))


ind_df_est<-subset(bronsvoort_training_data_clean,hcode%in%herd_est)
herd_df_est<-subset(herd_monlast,hcode%in%herd_est)
herd_df_est$hcode_stan_est<-1:nrow(herd_df_est)
ind_df_est<-left_join(ind_df_est,herd_df_est,by="hcode")


ind_df_pred<-subset(bronsvoort_training_data_clean,hcode%in%herd_pred)
herd_df_pred<-subset(herd_monlast,hcode%in%herd_pred)
herd_df_pred$hcode_stan_pred<-1:nrow(herd_df_pred)
ind_df_pred<-left_join(ind_df_pred,herd_df_pred,by="hcode")



###Creating predictive and estimating data lists
with(ind_df_est,
     dat_est <<- list(
       probang = Probang,
       age      = c(scale(age)),
       vnt_binary    = VNTAny,
       elisa_obs=FMD_cELISA,
       hcode = hcode_stan_est,
       vnt_obs=FMD_VNT_SAT2
     ))

names(dat_est)<-paste(names(dat_est),"_est",sep="")


herd_list_est<-list(
  He=nrow(herd_df_est),
  herd_est=herd_df_est$hcode_stan_est,
  monlast_est     = herd_df_est$monlast/12
)



dat_est<-c(dat_est,
           herd_list_est,
           list(Ne=nrow(ind_df_est)))

with(
  ind_df_pred,
  dat_pred <<- list(
    probang    = Probang,
    age      = c(scale(age)),
    vnt_binary = VNTAny,
    elisa_obs=FMD_cELISA,
    hcode = hcode_stan_pred,
    vnt_obs=scale(FMD_VNT_SAT2)
  )
)

names(dat_pred)<-paste(names(dat_pred),"_pred",sep="")

herd_list_pred<-list(
  Hp=nrow(herd_df_pred),
  herd_pred=herd_df_pred$hcode_stan_pred
)


dat_pred<-c(dat_pred,
            herd_list_pred,
            list(Np=nrow(ind_df_pred)))

##Creating separate 
dat_model<-c(dat_est,dat_pred,p=5)

fileName <- file.path(code.path,"fmd-vnt_probang_elisa.stan")
resStan_3indicators <- stan(fileName, data = dat_model,
                chains =5,cores=5 ,iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.8))

fileName <- file.path(code.path,"fmd-vnt_binary-only.stan")
resStan_vnt <- stan(fileName, data = dat_model,
                            chains =5,cores=5 ,iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.8))
fileName <- file.path(code.path,"fmd-probang-only.stan")
resStan_probang <- stan(fileName, data = dat_model,
                            chains =5,cores=5 ,iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.8))
fileName <- file.path(code.path,"fmd-elisa-only.stan")
resStan_elisa <- stan(fileName, data = dat_model,
                            chains =5,cores=5 ,iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.8))



monlast_3indicators<-data.frame(summary(resStan_3indicators,pars="monlast_pred")$summary,model="3indicators")
monlast_vnt<-data.frame(summary(resStan_vnt,pars="monlast_pred")$summary,model="vnt")
monlast_probang<-data.frame(summary(resStan_probang,pars="monlast_pred")$summary,model="probang")
monlast_elisa<-data.frame(summary(resStan_elisa,pars="monlast_pred")$summary,model="elisa")
monlast_allmods<-bind_rows(monlast_3indicators,monlast_elisa,monlast_probang,monlast_vnt)

monlast_allmods$hcode<-rep(herd_df_pred$hcode,4)

monlast_allmods$true_monlast<-rep(herd_df_pred$monlast,4)


ggplot(monlast_allmods,aes(x=true_monlast/12,y=mean))+
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.))+geom_point(col="red")+geom_abline(slope=1,intercept=0,col="blue")+facet_wrap(~model)
