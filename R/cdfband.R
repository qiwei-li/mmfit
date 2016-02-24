do_cdfband = function(x,g,theta){
  x.df <-data.frame(x = x)
  n <- dim(x.df)[1]
  fun.ecdf <- ecdf(x.df$x)
  ecdf.val <- fun.ecdf(sort(x.df$x))
  
  ecdf.df <- data.frame(x = sort(x.df$x), ecdf.val=ecdf.val)
  lwr = ecdf.df$ecdf.val-1.358*n^(-0.5)
  upr = ecdf.df$ecdf.val+1.358*n^(-0.5)
  predframe <- data.frame(cbind(ecdf.df, lwr, upr))
  dd = ggplot(ecdf.df,aes(x))+
       geom_line(aes(y=ecdf.val,colour="Sample")) +
       geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3)+
       labs(title='CDF with K-S CI band',x='data',y='prob')+theme(legend.title=element_blank())
  
  if(g =="poisson"){
    ecdf.df$cdf=ppois(sort(x.df$x),lambda = theta[1])
    dd = dd + geom_line(aes(y=ecdf.df$cdf,colour="Pois Estimation"))   
  } 
  
  if(g=="gamma"){
    ecdf.df$cdf=pgamma(sort(x.df$x),shape=theta[1],scale=theta[2])
    dd = dd + geom_line(aes(y=ecdf.df$cdf,colour="Gamma Estimation"))   
  }
  
  if(g=="beta"){
    ecdf.df$cdf=pbeta(sort(x.df$x),theta[1],theta[2])
    dd = dd + geom_line(aes(y=ecdf.df$cdf,colour="Beta Estimation"))
  }
  
  if(g=="mixture of 2 poissons"){
    ecdf.df$cdf=theta[3]*ppois(sort(x.df$x),theta[1])+(1-theta[3])*ppois(sort(x.df$x),theta[2])
    dd = dd + geom_line(aes(y=ecdf.df$cdf,colour="Mixed 2 Pois Estimation"))    
  }
  
  if(g=="mixture of 2 exponentials"){
    ecdf.df$cdf=theta[3]*pexp(sort(x.df$x),rate = 1.0/theta[1])+(1-theta[3])*pexp(sort(x.df$x),rate = 1.0/theta[2])
    dd = dd + geom_line(aes(y=ecdf.df$cdf,colour="Mixed 2 Exp Estimation"))    
  }
  
  return(dd)
}