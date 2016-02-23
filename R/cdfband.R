do_cdfband = function(x){
  x.df <-data.frame(x = x)
  n <- dim(x.df)[1]
  fun.ecdf <- ecdf(x.df$x)
  ecdf.val <- fun.ecdf(sort(x.df$x))
  ecdf.df <- data.frame(x = sort(x.df$x), ecdf.val=ecdf.val)
  
  lwr = ecdf.df$ecdf.val-1.358*n^(-0.5)
  upr = ecdf.df$ecdf.val+1.358*n^(-0.5)
  
  predframe <- data.frame(cbind(ecdf.df, lwr, upr))
  
  dd = ggplot(ecdf.df,aes(x,ecdf.val))+
       geom_line(data=predframe) +
       geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3)+
       labs(title='CDF with K-S CI band',x='data',y='prob')
  return(dd)
}