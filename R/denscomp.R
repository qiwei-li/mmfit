do_denscomp = function(g, x, thetahat){
  if(g == "poisson"){
    dd = data.frame(x, dpois(x,thetahat[1]))
    names(dd)=c("x","density")
    d <- ggplot(data = dd , aes(x = x, y=density))
    d <- d + geom_line()
    d
    d <- d + geom_histogram(aes(x=x, y=..density..), alpha=0.4)
    d
  }
  
  if(g == "power law"){
    
  }
  
  if(g == "gamma"){
    dd = data.frame(x, dgamma(x,thetahat[1], thetahat[2]))
    names(dd)=c("x","density")
    d <- ggplot(data = dd , aes(x = x, y=density))
    d <- d + geom_line()
    d
    d <- d + geom_histogram(aes(x=x, y=..density..), alpha=0.4)
    d
    
  }
  
  if(g == "beta"){
    dd = data.frame(x, dbeta(x,thetahat[1], thetahat[2]))
    names(dd)=c("x","density")
    d <- ggplot(data = dd , aes(x = x, y=density))
    d <- d + geom_line()
    d
    d <- d + geom_histogram(aes(x=x, y=..density..), alpha=0.4)
    d
    
    
  }
  
  if(g == "mixture of 2 poissons"){
    
  }
  
  if(g == "mixture of 2 exponentials"){
    
    
  }
  
  
}