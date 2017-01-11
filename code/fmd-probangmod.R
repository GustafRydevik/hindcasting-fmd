bronsvoort_training_data$ProbangAny<-with(bronsvoort_training_data,(PbP.A+PbP.O+PbP.SAT2))
bronsvoort_training_data$VNTAny<-with(bronsvoort_training_data,(FMDS_A+FMDS_O+FMDS_SAT2))
with(bronsvoort_training_data,table(Probang,monlast))

summary(glm(Probang~monlast*age+age^2+VNTAny,data=bronsvoort_training_data,family=binomial(link="logit")))






dat <- list(N        = nrow(bronsvoort_training_data),
            p        = 5,
            probang    = bronsvoort_training_data$Probang,
            monlast     = bronsvoort_training_data$monlast,
            age      = bronsvoort_training_data$age,
            vnt     = bronsvoort_training_data$VNTAny)

## Load Stan file
fileName <- file.path(code.path,"fmd-probangmod.stan")
stan_code <- readChar(fileName, file.info(fileName)$size)
cat(stan_code)
