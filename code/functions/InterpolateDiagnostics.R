##Interpolating diagnostic test data to have a common range,time domain and a certain smoothness.


InterpolateDiagnostics<-function(diagnostic_df,range,time_col=1,diagnostic_col=2,...){
  diagnostic_fit<-loess(diagnostic_df[,diagnostic_col]~diagnostic_df[,time_col],...)
  diagnostic_predicted<-data.frame(range,predict(diagnostic_fit,range))
  names(diagnostic_predicted)<-names(diagnostic_df)
  return(diagnostic_predicted)
}
