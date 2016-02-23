do_denscomp = function(g, x, thetahat){
  if(g == "poisson"){
    fhat = rpois(1000,thetahat[1])
  }
   
  if(g == "power law"){
    return(NULL)
  }
  
  if(g == "gamma"){
    fhat = rgamma(1000,thetahat[1], thetahat[2])
  }
  
  if(g == "beta"){
    fhat = rbeta(1000,thetahat[1], thetahat[2])
  }
  
  if(g == "mixture of 2 poissons"){
    flag = sample(2, 1000, prob = c(thetahat[3], 1-thetahat[3]), replace=TRUE)
    d1 = rpois(sum(flag==1), thetahat[1])
    d2 = rpois(sum(flag==2), thetahat[1])
    fhat = c(d1, d2)
  }
  
  if(g == "mixture of 2 exponentials"){
    flag = sample(2, 1000, prob = c(thetahat[3], 1-thetahat[3]), replace=TRUE)
    d1 = rexp(sum(flag==1), thetahat[1])
    d2 = rexp(sum(flag==2), thetahat[1])
    fhat = c(d1, d2)
  }

  d <- ggplot(data = data.frame(x), aes(x=x, y=..density..))
  d <- d + geom_histogram()
  d <- d + geom_line(data=data.frame(x=density(fhat)$x, y=density(fhat)$y), aes(x=x, y=y))
  d <- d + labs(title='parametric and nonparametric density',x='data',y='density')
  return(d)
}