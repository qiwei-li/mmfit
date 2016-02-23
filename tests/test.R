library(gmm)
library(mfp)
data("bodyfat")

# example in the book
x = bodyfat$brozek/100
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


a = mmfit(g="beta",x=x,start=c(0.2,0.2))
print(a)
