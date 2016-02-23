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



ggplot(data.frame(x), aes(x=x)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white") +
  geom_line(data = data.frame(dbeta(x,4.6522,19.9203)), aes(x = x)) 


dd = data.frame(x, dbeta(x,4.6522,19.9203))
names(dd)=c("x","density")
d <- ggplot(data = dd , aes(x = x, y=density))
d <- d + geom_line()
d
d <- d + geom_histogram(aes(x=x, y=..density..), alpha=0.4)
d


# good
res1
res2
