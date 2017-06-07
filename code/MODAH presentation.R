ggplot(bronsvoort_training_data_clean,aes(x=monlast))+geom_histogram()+
  ggtitle("Histogram of months since last FMD incursion")+scale_x_reverse()+
  theme_grey(base_size = 28)+xlab("Months")+

  
  # Graphs from Simulated_trends.R
  
  #Graphs from trendfit_caller.R 