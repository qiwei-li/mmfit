\name{mmfit}
\alias{mmfit}
\title{mmfit}
\description{fit data with method of moment}
\usage{
mmfit(g, x, start, gd)
}
\arguments{
\item{g}{Name of the distribution. Built in option includes: {"poisson", "power law", "gamma", "beta", "negative binomial","mixture of 2 poissons", "mixture of 2 exponentials","mixture of 2 normals"}}
\item{x}{A vector of data}
\item{start}{Starting values of the estimating parameters}
\item{gd}{If user supplies g, he needs to supplies the pmf/pdf as gd. i.e. gd = function(x, list(th1, th2, ...))Default to NULL}
}
\value{
mmf object which includes the estimations and standard errors of the parameters, a graph object that compares the parametric and nonparametric density estimates, a graph object that draws the empirical cdf and an enclosing Kolmogorov-Smirnov confidence band
}
\examples{
# fit a beta distribution on a real dataset
install.packages("mfp")
library("mfp")
data("bodyfat")
x = bodyfat$brozek/100
a = mmfit(g="beta",x=x,start=c(alpha=0.2,beta=0.2))
print(a)

# fit a power law distribution on a real dataset
install.packages("poweRlaw")
library("poweRlaw")
data("moby")
x = moby
b = mmfit(g="power law",x=x,start=c(gamma = 20))
print(b)

# fit a mixture normal distribution on a real dataset
x = hist(faithful$waiting)
c = mmfit(g="mixture of 2 normals",x=faithful$waiting,start=c(mu1=50,sd1=5,mu2=80,sd2=2,prop1=0.3))
print(c)



}
\keyword{mmfit}
