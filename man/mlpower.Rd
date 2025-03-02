% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlpower.R
\name{mlpower}
\alias{mlpower}
\title{Power distribution maximum likelihood estimation}
\usage{
mlpower(x, na.rm = FALSE, ...)
}
\arguments{
\item{x}{a (non-empty) numeric vector of data values.}

\item{na.rm}{logical. Should missing values be removed?}

\item{...}{\code{epsilon} is a positive number added to \code{max(x)} as an to the
maximum likelihood. Defaults to \code{.Machine$double.eps^0.5}.}
}
\value{
\code{mlpower} returns an object of \link[base:class]{class} \code{univariateML}.
This is a named numeric vector with maximum likelihood estimates for
\code{alpha} and \code{beta} and the following attributes:
\item{\code{model}}{The name of the model.}
\item{\code{density}}{The density associated with the estimates.}
\item{\code{logLik}}{The loglikelihood at the maximum.}
\item{\code{support}}{The support of the density.}
\item{\code{n}}{The number of observations.}
\item{\code{call}}{The call as captured my \code{match.call}}
}
\description{
The maximum likelihood estimate of \code{alpha} is the maximum of \code{x} +
\code{epsilon} (see the details) and the maximum likelihood estimate of
\code{beta} is \code{1/(log(alpha)-mean(log(x)))}.
}
\details{
For the density function of the power distribution see
\link[extraDistr:PowerDist]{PowerDist}. The maximum likelihood estimator of
\code{alpha} does not exist, strictly
speaking. This is because \code{x} is supported \code{c(0, alpha)} with
an open endpoint on alpha in the \code{extraDistr} implementation of
\code{dpower}. If the endpoint was closed, \code{max(x)} would have been
the maximum likelihood estimator. To overcome this problem, we add
a possibly user specified \code{epsilon} to \code{max(x)}.
}
\examples{
mlpower(precip)
}
\references{
Arslan, G. "A new characterization of the power distribution."
Journal of Computational and Applied Mathematics 260 (2014): 99-102.
}
\seealso{
\link[extraDistr:PowerDist]{PowerDist} for the power density. \link[extraDistr:Pareto]{extraDistr::Pareto}
for the closely related Pareto distribution.
}
