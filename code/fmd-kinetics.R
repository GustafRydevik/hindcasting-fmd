output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

tmp<-read.table(file.path(textdata.path,"FMD-Gustaf.csv"),sep=",",header=T,row.names=1)

library(GGally)

###This should be replaced by a library call later on
lapply(dir(function.path,full.names=T),source)

#FMD_Danish C-ELISA

#South American I-ELISA

#CHEKIT kit

#FMD_EITB - enzyme linked immunoelectrotransfer blot

# 
# FMD_VNT_A 

# FMD_VNT_O 

# FMD_VNT_SAT2 

#Integrate info from
#Comparison of Two 3ABC Enzyme-Linked Immunosorbent Assays for 
#Diagnosis of Multiple-Serotype Foot-and-Mouth Disease in a Cattle Population in an Area of Endemicity


# Using the below publication as a source of FMD test kinetics. 
# Alexandressen (2003)
# Studies of quantitative parameters of virus excretion and 
# transmission in pigs and cattle experimentally infected with foot-and-mouth disease virus
##
mouthswab_inoculated<-read.table(file.path(textdata.path,"Alexandersen2003_inoculated_mouthswabs.csv"),sep=",",header=F)
nasalswab_inoculated<-read.table(file.path(textdata.path,"Alexandersen2003_inoculated_nasalswabs.csv"),sep=",",header=F)
viraemia_inoculated<-read.table(file.path(textdata.path,"Alexandersen2003_inoculated_viraemia.csv"),sep=",",header=F)
clinicalsigns_inoculated<-read.table(file.path(textdata.path,"Alexandersen2003_inoculated_clinicalsigns.csv"),sep=",",header=F)
antibodies_inoculated<-read.table(file.path(textdata.path,"Alexandersen2003_inoculated_antibodies.csv"),sep=",",header=F)

mouthswab_contact<-read.table(file.path(textdata.path,"Alexandersen2003_contact_mouthswabs.csv"),sep=",",header=F)
nasalswab_contact<-read.table(file.path(textdata.path,"Alexandersen2003_contact_nasalswabs.csv"),sep=",",header=F)
viraemia_contact<-read.table(file.path(textdata.path,"Alexandersen2003_contact_viraemia.csv"),sep=",",header=F)
clinicalsigns_contact<-read.table(file.path(textdata.path,"Alexandersen2003_contact_clinicalsigns.csv"),sep=",",header=F)
antibodies_contact<-read.table(file.path(textdata.path,"Alexandersen2003_contact_antibodies.csv"),sep=",",header=F)

interpolate_range<-1:360
fmd_diagnostics_df<-cbind(data.frame(time=interpolate_range),
                          InterpolateDiagnostics(mouthswab_inoculated,interpolate_range)[2],
                          InterpolateDiagnostics(nasalswab_inoculated,interpolate_range)[2],
                          InterpolateDiagnostics(viraemia_inoculated,interpolate_range)[2],
                          InterpolateDiagnostics(clinicalsigns_inoculated,interpolate_range)[2],
                          InterpolateDiagnostics(antibodies_inoculated,interpolate_range)[2],
                          InterpolateDiagnostics(mouthswab_contact,interpolate_range)[2],
                          InterpolateDiagnostics(nasalswab_contact,interpolate_range)[2],
                          InterpolateDiagnostics(viraemia_contact,interpolate_range)[2],
                          InterpolateDiagnostics(clinicalsigns_contact,interpolate_range)[2],
                          InterpolateDiagnostics(antibodies_contact,interpolate_range)[2]
)
names(fmd_diagnostics_df)[-1]<-c("mouthswab_inoculat",
                                 "nasalswab_inoculated",
                                 "viraemia_inoculated",
                                 "clinicalsigns_inoculated",
                                 "antibodies_inoculated",
                                 "mouthswab_contact",
                                 "nasalswab_contact",
                                 "viraemia_contact",
                                 "clinicalsigns_contact",
                                 "antibodies_contact")

plot(fmd_diagnostics_df[1:6])
plot(fmd_diagnostics_df[c(1,7:11)])
ggpairs(fmd_diagnostics_df[c(1,7:11)])
#Reading in digitized epicurve of the UK 2001 FMD outbreak 
fmd_2001outbreak_df<-round(read.table(file.path(textdata.path,"FMD2001_report.csv"),sep=","))
names(fmd_2001outbreak_df)<-c("week","casecount")
fmd_outbreak_daily<-data.frame(day=1:(32*7),interpolated_casecount=approx(x=c(1,(fmd_2001outbreak_df$week)*7),y=c(1/7,fmd_2001outbreak_df$casecount/7),xout=1:(32*7))$y)
fmd_outbreak_simulated<-data.frame(day=1:(32*7),sim_casecount=c(table(sample(factor(1:(32*7)),size=sum(fmd_2001outbreak_df$casecount),
                                                                   prob =fmd_outbreak_daily$interpolated_casecount,replace=T))))


fmd_diagnostics_df_plot<-fmd_diagnostics_df
names(fmd_diagnostics_df_plot)[c(1,7:11)]<-c("Time","Mouthswab","Nasalswab","Viraemia","Clinical_signs","Antibodies")
ggpairs_col<-wrap(ggpairs,color="#aa6985")
ggpairs(fmd_diagnostics_df_plot[c(1,7:11)],title="FMD diagnostics phaseplot")+theme_light(base_size=18)
