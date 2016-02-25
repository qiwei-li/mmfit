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
    g = function(th,x){
      gamma=th[1]
      m1= (gamma-1)/(gamma-2) - x
      f=cbind(m1)
      return(f)
    }
  }
  
  if(name == "negative binomial"){
    g = function(th,x){
      n=th[1]
      p=th[2]
      m1=n*(1-p)/p-x
      m2=n*(1-p)/p^2-(x-mean(x))^2
      f=cbind(m1,m2)
      return(f)
    }
  }
  
  if(name == "gamma"){
    g = function(th,x){
      k=th[1]
      theta=th[2]
      meanb=k*theta
      m1=meanb-x
      m2=k*theta^2-(x-mean(x))^2
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
    g = function(th,x){
      lambda1 = th[1]
      lambda2 = th[2]
      alpha = th[3]
      m1 = alpha*lambda1 + (1-alpha)*lambda2 - x
      m2 = alpha*(lambda1^2+lambda1) + (1-alpha)*(lambda2^2+lammbda2) - x^2
      m3 = alpha*(lambda1^3+3*lambda1^2+lambda1) + (1-alpha)*(lambda2^3+3*lambda2^2+lambda2) - x^3
      f = cbind(m1,m2,m3)
      return(f)
    }
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

  if(name == "mixture of 2 normals"){
    g = function(th,x){
      theta1=th[1]
      sigma1=th[2]
      theta2=th[3]
      sigma2=th[4]
      alpha=th[5]
      means=mean(x)
      m1=theta1-means
      m2=theta2-means
      
      mm1=alpha*m1+(1-alpha)*m2-(x-means)
      mm2=alpha*(sigma1^2+m1^2)+(1-alpha)*(sigma2^2+m2^2)-(x-means)^2
      mm3=alpha*(3*sigma1^2*m1+m1^3)+(1-alpha)*(3*sigma2^2*m2+m2^3)-(x-means)^3
      mm4=alpha*(3*sigma1^4+6*m1^2*sigma1^2+m1^4)+(1-alpha)*(3*sigma2^4+6*m2^2*sigma2^2+m2^4)-(x-means)^4
      mm5=alpha*(15*sigma1^4*m1+10*sigma1^2*m1^3+m1^5)+(1-alpha)*(15*sigma2^4*m2+10*sigma2^2*m2^3+m2^5)-(x-means)^5
      f=cbind(mm1,mm2,mm3,mm4,mm5)
      return(f)
    }
  }
  
  return(g)
}