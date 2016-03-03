output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

cameroon_fmd_df<-read.table(file.path(textdata.path,"FMD-Gustaf.csv"),sep=",",header=T,row.names=1)



###This should be replaced by a library call later on
lapply(dir(function.path,full.names=T),source)

#Danish C-ELISA

#South American I-ELISA

#CHEKIT kit

#FMD_EITB - enzyme linked immunoelectrotransfer blot

# 
# FMD_VNT_A 

# FMD_VNT_O 

# FMD_VNT_SAT2 




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




