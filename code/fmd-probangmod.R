bronsvoort_training_data$ProbangAny<-with(bronsvoort_training_data,(PbP.A+PbP.O+PbP.SAT2)>0)
bronsvoort_training_data$VNTAny<-with(bronsvoort_training_data,(FMDS_A+FMDS_O+FMDS_SAT2)>0)
with(bronsvoort_training_data,table(Probang,monlast))

summary(glm(Probang~monlast*age+I(age^2)+VNTAny,data=bronsvoort_training_data,family=binomial(link="logit")))





bronsvoort_training_data_clean<-bronsvoort_training_data[complete.cases(bronsvoort_training_data[c("Probang","monlast","age","VNTAny")]),]
dat <- list(N        = nrow(bronsvoort_training_data_clean),
            p        = 6,
            probang    = bronsvoort_training_data_clean$Probang,
            monlast     = bronsvoort_training_data_clean$monlast,
            age      = bronsvoort_training_data_clean$age,
            vnt     = bronsvoort_training_data_clean$VNTAny)

## Load Stan file
fileName <- file.path(code.path,"fmd-probangmod.stan")
resStan <- stan(fileName, data = dat,
                chains = 3, iter = 3000, warmup = 500, thin = 10)

## Show traceplot
traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)


herd_monlast<-unique(bronsvoort_training_data_clean[c("hcode","monlast")])


###Splitting up the data into prediction and estimation
set.seed(1337)
herd_pred<-sort(sample(herd_monlast$hcode,15))
herd_est<-sort(setdiff(herd_monlast$hcode,pred.herd))

            
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

fileName <- file.path(code.path,"fmd-probangmod.stan")
resStan <- stan(fileName, data = dat_model,
                chains =5, iter = 10000, warmup = 5000, thin = 10)

traceplot(resStan, pars = c("monlast_pred"), inc_warmup = TRUE)
