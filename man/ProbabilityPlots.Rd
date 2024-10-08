% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability_plots.R
\name{ProbabilityPlots}
\alias{ProbabilityPlots}
\alias{ppmlplot}
\alias{ppmlline}
\alias{ppmlpoints}
\alias{qqmlplot}
\alias{qqmlline}
\alias{qqmlpoints}
\title{Probability Plots Using Maximum Likelihood Estimates}
\usage{
ppmlplot(y, obj, plot.it = TRUE, datax = FALSE, ...)

ppmlline(...)

ppmlpoints(y, obj, plot.it = TRUE, datax = TRUE, ...)

qqmlplot(y, obj, plot.it = TRUE, datax = FALSE, ...)

qqmlline(y, obj, datax = FALSE, probs = c(0.25, 0.75), qtype = 7, ...)

qqmlpoints(y, obj, plot.it = TRUE, datax = TRUE, ...)
}
\arguments{
\item{y}{Numeric vector; The data to plot on the \code{y} axis when
\code{datax} is \code{FALSE}.}

\item{obj}{Either an \code{univariateML} object or a function that returns
a \code{univariateML} object when called with \code{y} as its only
argument.}

\item{plot.it}{Logical; should the result be plotted?}

\item{datax}{Logical; should \code{y} be plotted on the \code{x}-axis?
Defaults to \code{FALSE} in \code{qqmlplot} and \code{ppmlplot} but
\code{TRUE} in \code{qqmlpoints} and \code{ppmlpoints}.}

\item{...}{Graphical parameters.}

\item{probs}{Numeric vector of length two, representing probabilities.
Corresponding quantile pairs define the line drawn.}

\item{qtype}{The \code{type} of quantile computation used in \code{quantile}.}
}
\value{
For \code{qqmlplot}, \code{qqmlpoints}, \code{ppmlplot}, and
\code{ppmlpoints}, a list with components \code{x} (plotted on the x axis)
and \code{y} (plotted on the y axis). \code{qqmlline} and \code{ppmlline}
returns nothing.
}
\description{
Make quantile-quantile plots and probability-probability plots using maximum
likelihood estimation.
}
\details{
\code{qqmlplot} produces a quantile-quantile plot (Q-Q plot) of the values in
\code{y} with respect to the distribution defined by \code{obj}, which is
either a \code{univariateML} object or a function returning a
\code{univariateML} object when called with \code{y}. \code{qqmlline} adds a
line to a <U+201C>theoretical<U+201D>, quantile-quantile plot which passes through
the \code{probs} quantiles, by default the first and third quartiles.
\code{qqmlpoints}behaves like \code{stats::points} and adds a Q-Q plot to
an existing plot.

\code{ppmlplot}, \code{ppmlline}, and \code{ppmlpoints} produce
probability-probability plots (or P-P plots). They behave similarly to the
quantile-quantile plot functions.

This function is modeled after \link[stats:qqnorm]{qqnorm}.

Quantile-quantile plots and probability-probability plots are only supported
for continuous distributions.

Graphical parameters may be given as arguments to all the functions below.
}
\examples{
## Make a single probability plot with a line.

obj <- mlgamma(Nile)
qqmlplot(Nile, obj)
qqmlline(Nile, obj)

## Make multiple probability plots. datax = TRUE must be used to make this
## look good.

ppmlplot(airquality$Wind, mlgamma, main = "Many P-P plots")
ppmlpoints(airquality$Wind, mlexp, col = "red")
ppmlpoints(airquality$Wind, mlweibull, col = "purple")
ppmlpoints(airquality$Wind, mllnorm, col = "blue")
}
\references{
M. B. Wilk, R. Gnadadesikan, Probability plotting methods for the analysis
for the analysis of data, Biometrika, Volume 55, Issue 1, March 1968,
Pages 1<U+2013>17, https://doi.org/10.1093/biomet/55.1.1
}
