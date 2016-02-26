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
install.packages("mfp")
library("mfp")
data("bodyfat")
x = bodyfat$brozek/100
a = mmfit(g="beta",x=x,start=c(alpha=0.2,beta=0.2))
print(a)

install.packages("poweRlaw")
library("poweRlaw")
data("moby")
x = moby
b = mmfit(g="power law",x=x,start=c(gamma = 20))
print(b)
}
\keyword{mmfit}
