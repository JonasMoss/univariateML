% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlzipf.R
\name{mlzipf}
\alias{mlzipf}
\title{Zipf distribution maximum likelihood estimation}
\usage{
mlzipf(x, na.rm = FALSE, ...)
}
\arguments{
\item{x}{a (non-empty) numeric vector of data values.}

\item{na.rm}{logical. Should missing values be removed?}

\item{...}{Not currently in use.}
}
\value{
\code{mlzipf} returns an object of \link[base:class]{class} \code{univariateML}.
This is a named numeric vector with maximum likelihood estimates for
\code{N} and \code{s} and the following attributes:
\item{\code{model}}{The name of the model.}
\item{\code{density}}{The density associated with the estimates.}
\item{\code{logLik}}{The loglikelihood at the maximum.}
\item{\code{support}}{The support of the density.}
\item{\code{n}}{The number of observations.}
\item{\code{call}}{The call as captured my \code{match.call}}
}
\description{
For the density function of the Zipf distribution see
\link[sads:dzipf]{Zipf}.
}
\details{
This function follows the same format as every other function in the package,
but most applications of Zipf's law use rank-abundance data. See, e.g., \link[sads:fitrad]{sads::fitzipf}
for estimation of this sort of data.
}
\examples{
AIC(mlzipf(corbet)) # 2729.536
AIC(mllgser(corbet)) # 2835.883
}
\references{
Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
}
\seealso{
\link[sads:dzipf]{Zipf} for the density.
}
