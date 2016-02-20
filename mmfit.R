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
  func=switch(g,beta=BETA)
  return(func) 
}

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

####### Example #######
#> source("mmfit.R")
#> mmfit(list("beta"),x,start)
# then the obj will be returned