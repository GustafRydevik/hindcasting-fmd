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

dat <- list(N        = nrow(bronsvoort_training_data_clean),
            p        = 6,
            probang    = bronsvoort_training_data_clean$Probang,
            age      = bronsvoort_training_data_clean$age,
            vnt     = bronsvoort_training_data_clean$VNTAny,
            hcode=bronsvoort_training_data_clean$hcode,
            H=nrow(herd_monlast),
            herd=herd_monlast$hcode,
            monlast     = herd_monlast$monlast
            )
