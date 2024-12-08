% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_select.R
\name{model_select}
\alias{model_select}
\title{Fit multiple models and select the best fit}
\usage{
model_select(
  x,
  models = univariateML_models,
  criterion = c("AIC", "BIC", "logLik"),
  na.rm = FALSE,
  type = c("both", "discrete", "continuous"),
  return = c("best", "all"),
  ...
)
}
\arguments{
\item{x}{a (non-empty) numeric vector of data values.}

\item{models}{a character vector containing the distribution models to
select from; see \code{print(univariateML_models)}. Defaults to all implemented models.}

\item{criterion}{the model selection criterion. Must be one of \code{"AIC"},
\code{"BIC"}, and \code{"logLik"}, ignoring case. Defaults to \code{"AIC"}.}

\item{na.rm}{logical. Should missing values be removed?}

\item{type}{Either \code{"both"}, \code{"discrete"}, or \code{"continuous"}. The supplied \code{models}
vector is restricted to the desired class.}

\item{return}{character length 1. "univariateML" (default) if the function
should return the single best model; "all" if a tibble data frame
of all results should be returned, sorted by decreasing model performance.}

\item{...}{unused.}
}
\value{
The return value depends on the \code{return} argument.
For \code{return = "best"} (default), \code{model_select} returns
an object of \link[base:class]{class} \code{univariateML}

For \code{return = "all"}, \code{model_select} returns a tibble data frame
with the following columns:
\item{\code{model}}{The name of the model.}
\item{\verb{d_loglik, d_aic, d_bic}}{See \verb{loglik, aic, bic}.}
\item{\code{p}}{Number of parameters fitted.}
\item{\verb{loglik, aic, bic}}{The negative log-likelihood at the maximum,
the aic, and the bic, respectively. The minimum of each of these is noted
and then subtracted from each value to give their delta versions
\verb{d_loglik, d_aic, d_bic}}. So, the model with the lowest aic will
have \code{d_aic} of 0; the \code{d_aic} of all the other models shows how much higher
their aics are from the minimum. The same goes with \code{d_loglik} and \code{d_bic}.
\item{\code{ml}}{The internal code name for the model.}
\item{\code{univariateML}}{The \code{univariateML} object for the model. This is
\code{return = "all"}, this object is returned for all tested models.}
}
\description{
Selects the best model by log-likelihood, aic, or bic.
}
\examples{
# Select among all possible continuous models.
model_select(precip, type = "continuous")

# View possible models to fit.
print(univariateML_models)

# Try out only gamma, Weibull, and exponential.
model_select(precip, c("gamma", "weibull", "exp"))

# Fit the discrete `corbet` data to all available discrete models
model_select(corbet, type = "discrete", return = "all")

}
\seealso{
Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
Distributions, Volume 1, Chapter 17. Wiley, New York.
}
