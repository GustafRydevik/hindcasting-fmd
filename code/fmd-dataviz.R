
output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

bronsvoort_training_data<-read.csv(file.path(textdata.path,"FMD-Gustaf-training.csv")) 
bronsvoort_training_data$Probang<-with(bronsvoort_training_data,(PbP.A+PbP.O+PbP.SAT2)>0)
bronsvoort_training_data$VNTAny<-with(bronsvoort_training_data,(FMDS_A+FMDS_O+FMDS_SAT2)>0)

bronsvoort_training_data%>%group_by(hcode)%>%summarise(prop_probang=mean(Probang,na.rm=T),monlast=mean(monlast))%>%
  ggplot(aes(x=monlast,y=prop_probang))+geom_point()+geom_smooth()->p_probang

bronsvoort_training_data%>%group_by(hcode)%>%summarise(prop_vnt=mean(VNTAny,na.rm=T),monlast=mean(monlast))%>%
  ggplot(aes(x=monlast,y=prop_vnt))+geom_point()+geom_smooth()->p_vnt

bronsvoort_training_data%>%ggplot(aes(x=monlast))+geom_histogram()->p_month_hist

bronsvoort_training_data%>%
  ggplot(aes(x=factor(monlast),y=FMD_cELISA))+geom_boxplot()->p_elisa
