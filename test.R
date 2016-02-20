library(gmm)
library(mfp)
data("bodyfat")

# example in the book
x = bodyfat$brozek/100
g = function(th, x){
  t1 = th[1]
  t2 = th[2]
  t12 = t1 + t2
  meanb = t1 / t12
  m1 = meanb - x
  m2 = t1 * t2 / (t12^2 * (t12+1)) - (x-mean(x))^2
  f = cbind(m1, m2)
  return(f)
}

# using gmm
res1 = gmm(g, x, c(alpha = 0.1, beta = 0.1))

# using mmfit
res2 = mmfit(g=list(g), x=as.data.frame(x), start = list(c(alpha=0.1, beta=0.1)))
