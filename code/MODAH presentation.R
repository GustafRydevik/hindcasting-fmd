ggplot(bronsvoort_training_data_clean,aes(x=monlast))+geom_histogram()+
  ggtitle("Histogram of months since last FMD incursion")+scale_x_reverse()+
  theme_grey(base_size = 28)+xlab("Months")

  
  # Graphs from Simulated_trends.R
  
  #Graphs from trendfit_caller.R 
ggplot(slope_est_df,aes(x=month,y=value,group=key))+
  geom_line(alpha=0.01)+
  geom_abline(intercept=slope_mean[1,1],slope=-slope_mean[2,1]+slope_mean[1,1]/150,col="red")+
  scale_x_reverse()+ggtitle("Estimated incidence trend of FMD incursions")+
  theme_grey(base_size=24)+
  xlab("Months since incursion")+ylab("incidence per month")


  ##graph of kinetics from dataviz
  p_3diagnostics_grid<-grid.arrange(p_month_hist+xlab("Months since incursion")+theme_grey(base_size=16),
                                    p_probang+xlab("Months since incursion")+theme_grey(base_size=16),
                                    p_vnt+xlab("Months since incursion")+theme_grey(base_size=16),
                                    p_elisa+xlab("Months since incursion")+theme_grey(base_size=16),
                                    nrow=4,ncol=1)
  
  
  
  ##Could add a graph of fitted response curves as well here 
  #Graph from allmodels.R
  monlast_allmods$model<-factor(monlast_allmods$model,levels=c("elisa","probang","vnt","3indicators"))
  ggplot(monlast_allmods,aes(x=true_monlast,y=mean))+
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.))+geom_point(col="red")+
  geom_abline(slope=1/12,intercept=0,col="blue")+
  facet_wrap(~model)+xlab("Months since incursion")+theme_grey(base_size=20)+
  ggtitle("Results from three different hindcasting models")

##Add one that compares estimated vs fitted trend as well, if I have time. 


#Discuss epidemic FMD based on FMD kinetics, and mention AI.
