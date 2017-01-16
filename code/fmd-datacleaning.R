library(dplyr)
output.path<-"./output"
code.path<-"./code"
function.path<-"./code/functions"

textdata.path<-"./textdata"

binarydata.path<-"./binarydata"

# 
# bronsvoort_orig_data<-read.table(file.path(textdata.path,"FMD-Gustaf.csv"),sep=",",header=T,row.names=1)
# bronsvoort_orig_data<-read.table(file.path(textdata.path,"saha-fmd_lesions.csv"),sep=",",header=T,row.names=1)
# bronsvoort_lesions<-read.table(file.path(textdata.path,"saha-fmd_lesions.csv"),sep=",",header=T)
# bronsvoort_joined<-left_join(bronsvoort_orig_data,bronsvoort_lesions,by=c("hcode"="HERD_CODE","ancode"="ANIMAL_COD"))
# 
# set.seed(1000)
# eval_data_ndx<-sort(sample(1:nrow(bronsvoort_joined),200))
# ## eval_data_ndx
# #[1]   15   18   33   34   37   40   57   60   61   72   80   99  101  102  111  117  119  130  135  138  139  146  147  176  187  197  206  211  217 226 227
# #[32]  245  254  255  257  262  264  276  297  306  319  353  359  360  379  387  413  418  426  427  439  457  461  468  469  472  481  505 506  508 515  517
# #[63]  528  532  538  560  571  581  582  583  625  634  635  644  652  658  659  664  666  677  696  730  750  762  765  776  783  792  799  801  807  808  819
# #[94]  823  828  836  840  844  845  850  867  869  884  887  893  897  899  903  904  910  914  915  917  923  924  936  937  947  949  950  953  972  973  987
# #[125]  988 1002 1006 1012 1031 1075 1084 1085 1087 1093 1098 1117 1118 1121 1123 1126 1129 1131 1133 1138 1152 1156 1163 1196 1208 1231 1233 1239 1243 1244 1257
# #[156] 1261 1266 1270 1274 1278 1292 1299 1301 1304 1313 1330 1339 1341 1345 1352 1365 1371 1380 1407 1409 1411 1424 1425 1427 1434 1435 1439 1456 1463 1507 1511
# #[187] 1516 1517 1524 1533 1539 1545 1553 1566 1569 1594 1595 1596 1615 1618
# bronsvoort_eval_data<-bronsvoort_joined[eval_data_ndx,]
# write.csv(bronsvoort_eval_data,file=file.path(textdata.path,"FMD-Gustaf-eval.csv"))
# bronsvoort_training_data<-bronsvoort_joined[-eval_data_ndx,]
# write.csv(bronsvoort_training_data,file=file.path(textdata.path,"FMD-Gustaf-training.csv"))
# rm(bronsvoort_orig_data)
# rm(bronsvoort_eval_data)
# rm(bronsvoort_training_data)
# rm(bronsvoort_joined)
bronsvoort_training_data<-read.csv(file.path(textdata.path,"FMD-Gustaf-training.csv")) 
