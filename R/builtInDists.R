# built in distributions
builtInDists = function(name){
  if(name == "poisson"){
    g = function(th,x){
      t1=th[1]
      meanb=t1
      m1=meanb-x
      f=cbind(m1)
      return(f)
    }
  }
  
  if(name == "power law"){

  }
  
  if(name == "gamma"){
    g = function(th,x){
      k=th[1]
      theta=th[2]
      meanb=k*theta
      m1=meanb-x
      m2=k*theta^2-(x-meanb)^2
      f=cbind(m1,m2)
      return(f)
    }
  }
  
  if(name == "beta"){
    g = function(th,x){
      t1=th[1]
      t2=th[2]
      t12=t1+t2
      meanb=t1/t12
      m1=meanb-x
      m2=t1*t2/(t12^2*(t12+1))-(x-mean(x))^2
      f=cbind(m1,m2)
      return(f)
    }
    
  }
  
  if(name == "mixture of 2 poissons"){
    
  }
  
  if(name == "mixture of 2 exponentials"){
    g = function(th,x){
      mu=th[1]
      lambda=th[2]
      alpha=th[3]
      meanb=alpha*mu+(1-alpha)*lambda
      m1=meanb-x
      m2=2*(alpha*mu^2+(1-alpha)*lambda^2)-x^2
      m3=6*(alpha*mu^3+(1-alpha)*lambda^3)-x^3
      f=cbind(m1,m2,m3)
      return(f)
    }
      
  }
  return(g)
}