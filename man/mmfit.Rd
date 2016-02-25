\name{mmfit}
\alias{mmfit}
\title{mmfit}
\description{fit data with method of moment}
\usage{
mmfit(g, x, start)
}
\arguments{
\item{g}{Name of the distribution. Built in option includes: {"poisson", "power law", "gamma", "beta", "negative binomial","mixture of 2 poissons", "mixture of 2 exponentials","mixture of 2 normals"}}
\item{x}{A vector of data}
\item{start}{Starting values of the estimating parameters}
}
\value{
mmf object which includes the estimations and standard errors of the parameters, a graph object that compares the parametric and nonparametric density estimates, a graph object that draws the empirical cdf and an enclosing Kolmogorov-Smirnov confidence band
}
\examples{
data("bodyfat")
x = bodyfat/100
a = mmfit(g="beta",x=x,start=c(0.2,0.2))
summary(a)
print(a)
print("mixture of 2 normals")
x = hist(faithful$waiting)
a = mmfit(g="mixture of 2 normals",x=faithful$waiting,start=c(50,5,80,2,0.3))
summary(a)
print(a)
}
\keyword{mmfit}
