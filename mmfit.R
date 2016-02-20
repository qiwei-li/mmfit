mmfit=function(g,x,start){
  ####### Check Input #######
  if (ncol(x)==length(g)){
    obj=structure(list(g=g,x=x,start=start),class=c("mmfit","list"))
  } else if (length(g)==1){
    g=rep(g,ncol(x))
    obj=structure(list(g=g,x=x,start=start),class=c("mmfit","list"))
  } else {
    stop("Error: g is not sufficiently defined.")  
  }
  ####### Substitute #######
  for (i in 1:length(obj$g)){
    if (class(obj$g[[i]])=="character") {
      obj$g[[i]]=subst(obj$g[[i]])
      #obj$g[i]="aa"
    }
  }
  return(obj)
}

####### Sustitute Function & G-Function Library #######
subst=function(g){
  func=switch(g,poisson=POISSON,powerlaw=POWERLAW,gamma=GAMMA,beta=BETA)
  return(func) 
}

####### POISSON #######
POISSON=function(th,x){
  t1=th[1]
  meanb=t1
  m1=meanb-x
  f=cbind(m1)
  return(f)
}

##### f(x) = a x^(-k) #####
POWERLAW=function(th,x){
##### Lacking of well-defined value #####
}

##### GAMMA #####
GAMMA=function(th,x){
  k=th[1]
  theta=th[2]
  meanb=k*theta
  m1=meanb-x
  m2=k*theta^2-(x-meanb)^2
  f=cbind(m1,m2)
  return(f)
}

#####  BETA  #####
BETA=function(th,x){
  t1=th[1]
  t2=th[2]
  t12=t1+t12
  meanb=t1/t12
  m1=meanb-x
  m2=t1*t2/(t12^2*(t12+1))-(x-meanb)^2
  f=cbind(m1,m2)
  return(f)
}

#####  Mixture 2 Poisson  #####


#####  Mixture 2 Exponetial #####

####### Example #######
#> source("mmfit.R")
#> mmfit(list("beta"),x,start)
# then the obj will be returned