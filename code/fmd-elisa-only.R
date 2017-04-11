
output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

#bronsvoort_training_data


#bronsvoort_training_data

SSlogis <- selfStart(~ Asym/(1 + exp((xmid - x)/scal)),
                     function(mCall, data, LHS)
                     {
                       xy <- sortedXyData(mCall[["x"]], LHS, data)
                       if(nrow(xy) < 4) {
                         stop("Too few distinct x values to fit a logistic")
                       }
                       z <- xy[["y"]]
                       if (min(z) <= 0) { z <- z + 0.05 * max(z) } # avoid zeroes
                       z <- z/(1.05 * max(z))              # scale to within unit height
                       xy[["z"]] <- log(z/(1 - z))         # logit transformation
                       aux <- coef(lm(x ~ z, xy))
                       parameters(xy) <- list(xmid = aux[1], scal = aux[2])
                       pars <- as.vector(coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)),
                                                  data = xy, algorithm = "plinear")))
                       setNames(c(pars[3], pars[1], pars[2]),
                                mCall[c("Asym", "xmid", "scal")])
                     }, c("Asym", "xmid", "scal"))

nls(FMD_cELISA~SSlogis(monlast,Asym,xmid,scal),data=bronsvoort_training_data)

growth_curve<-function(monlast,Asym=64.1,xmid=2.4681,scal=5.079){
  Asym/(1 + exp((xmid - monlast)/scal))}


bronsvoort_training_data_clean<-bronsvoort_training_data_clean[bronsvoort_training_data_clean$FMD_cELISA!=0,]

###Splitting up the data into prediction and estimation
set.seed(1337)
herd_pred<-sort(sample(herd_monlast$hcode,15))
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
           list(Ne=nrow(ind_df_est)))

with(
  ind_df_pred,
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
            list(Np=nrow(ind_df_pred)))

##Creating separate 
dat_model<-c(dat_est,dat_pred,p=6)

fileName <- file.path(code.path,"fmd-elisa-only.stan")
resStan <- stan(fileName, data = dat_model,
                chains =5,cores=5 ,iter = 10000, warmup = 5000, thin = 10,control = list(adapt_delta = 0.8))

pairs(resStan,pars=c("elisa_lambda_one","elisa_lambda_two","elisa_lambda_three","sigma"))

##There are two solutions to the growth curve: one with a scale of 1, an asymptote of 57, and a mid of 2
##A second solution with a scale of ~4.5, an asymptoote of 67, and a mid point of 3.5.
##A sigma of ~28 means that we're getting absolutely no information from 

growth_pars<-summary(resStan,pars=c("elisa_lambda_one","elisa_lambda_two","elisa_lambda_three"))$summary[,"mean"]
plot(growth_curve(1:150,growth_pars[1],growth_pars[2],growth_pars[3]))


traceplot(resStan, pars = c("monlast_pred"), inc_warmup = TRUE)


ggplot(ind_df_est,aes(y=FMD_cELISA,x=monlast.x))+geom_point()+geom_smooth()


monlast_pred_stan<-as.data.frame(summary(resStan,pars="monlast_pred")$summary)
elisa_latent_pred<-as.data.frame(summary(resStan,pars=c("elisa_latent_pred"))$summary)
monlast_pred_stan$hcode<-herd_df_pred$hcode
ind_pred_stan<-left_join(ind_df_pred,monlast_pred_stan,by="hcode")

monlast_pred_stan<-left_join(monlast_pred_stan,
                             (ind_pred_stan%>%group_by(hcode)%>%summarise(mean_elisa=mean(FMD_cELISA)))[,c("hcode","mean_elisa")],by="hcode")


ggplot(data.frame(monlast_pred_stan,monlast=herd_df_pred$monlast),aes(x=monlast,y=mean))+
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.))+geom_point(col="red")+geom_abline(slope=1,intercept=0,col="blue")



plot(herd_df_pred$monlast,monlast_pred_stan[,"mean"])
points(herd_df_pred$monlast,monlast_pred_stan[,"2.5%"],col="red")
points(herd_df_pred$monlast,monlast_pred_stan[,"97.5%"],col="red")

