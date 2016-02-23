library(gmm)
library(ggplot2)
library(mfp)
data("bodyfat")

# example in the book
x = bodyfat$brozek/100
a = mmfit(g="gamma",x=x,start=c(0.2,0.2))
print(a)
