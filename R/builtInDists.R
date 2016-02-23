# sustitute strings into actual functions
subst=function(obj){
  for(i in 1:length(obj$g)){
    if(class(obj$g[[i]])=="character"){
      obj$g[[i]] = builtInDists(obj$g[[i]])
    }
  }
  return(obj)
}

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
    
  }
  return(g)
}