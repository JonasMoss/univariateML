#' Wrangles arguments for use in the plot, lines and points functions.
#'
#' @param x The input data.
#' @param range Range of the data.
#' @param points Boolean; should points be plotted by default?
#' @keywords internal

plot_wrangler <- function(x, range, points = FALSE, ...) {
  if (is.null(range)) {
    if (abs(attr(x, "support")[1]) + abs(attr(x, "support")[2]) < Inf) {
      limits <- attr(x, "support")
    } else if (abs(attr(x, "support")[1]) == 0 & abs(attr(x, "support")[2]) == Inf) {
      limits <- c(0, qml(0.99, x))
    } else {
      limits <- qml(c(0.01, 0.99), x)
    }

    range <- seq(limits[1], limits[2], length.out = 1000)
  }

  defaults <- list(
    type = if (points) "p" else "l",
    main = paste0(attr(x, "model"), " model"),
    ylab = "Density",
    xlab = "x",
    lwd = 1
  )

  args <- listmerge(
    x = defaults,
    y = list(...)
  )

  args$x <- range
  args$y <- dml(args$x, x)
  args
}

#' Plot, Lines and Points Methods for Maximum Likelihood Estimates
#'
#' The \code{plot}, \code{lines}, and \code{points} methods for \code{univariateML} objects.
#'
#' @export
#' @param x a \code{univariateML} object.
#' @param range range of \code{x} values to plot, i.e. \code{c(lower, upper)}.
#' @param ... parameters passed to \code{plot}, \code{lines}, or \code{points}.
#' @return An invisible copy of \code{x}.
#' @examples
#' plot(mlweibull(datasets::precip), main = "Annual Precipitation in US Cities")
#' lines(mlgamma(datasets::precip), lty = 2)
#' rug(datasets::precip)
#' @export
#'
plot.univariateML <- function(x, range = NULL, ...) {
  args <- plot_wrangler(x, range, points = FALSE, ...)
  do.call(graphics::plot, args)
  invisible(x)
}

#' @export
#' @rdname plot.univariateML
lines.univariateML <- function(x, range = NULL, ...) {
  args <- plot_wrangler(x, range, points = FALSE, ...)
  do.call(graphics::lines, args)
  invisible(x)
}

#' @export
#' @rdname plot.univariateML
points.univariateML <- function(x, range = NULL, ...) {
  args <- plot_wrangler(x, range, points = TRUE, ...)
  do.call(graphics::points, args)
  invisible(x)
}

#' @export
logLik.univariateML <- function(object, ...) {
  val <- attr(object, "logLik")
  attr(val, "nobs") <- attr(object, "n")
  attr(val, "df") <- length(object)
  class(val) <- "logLik"
  val
}

#' @export
coef.univariateML <- function(object, ...) {
  stats::setNames(as.numeric(object), names(object))
}

#' @export
summary.univariateML <- function(object, ...) {
  data.name <- deparse(as.list(attr(object, "call"))$x)
  digits <- list(...)$digits
  cat(
    "\nMaximum likelihood for the", attr(object, "model"), "model \n",
    "\nCall: ", deparse(attr(object, "call")), "\n\nEstimates: \n"
  )
  print.default(format(object, digits = digits), print.gap = 2L, quote = FALSE)
  cat("\nData:            ", data.name, " (", attr(object, "n"), " obs.)\n",
    "Support:         (", attr(object, "support")[1], ", ", attr(object, "support")[2], ")\n",
    "Density:         ", attr(object, "density"), "\n",
    "Log-likelihood:  ", attr(object, "logLik"), "\n",
    sep = ""
  )
  invisible(object)
}

#' @export
print.univariateML <- function(x, ...) {
  digits <- list(...)$digits
  if (is.null(digits)) digits <- 4
  cat("Maximum likelihood estimates for the", attr(x, "model"), "model \n")
  print.default(format(x, digits = digits), print.gap = 2L, quote = FALSE)
  invisible(x)
}


#' Confidence Intervals for Maximum Likelihood Estimates
#'
#' Computes a confidence interval for one or more parameters in a \code{unvariateML}
#'    object.
#'
#' \code{confint.univariateML} is a wrapper for  \code{\link{bootstrapml}} that computes
#'    confidence intervals for the main parameters of \code{object}. The main parameters of \code{object} are
#'    the members of \code{names(object)}. For instance,the main parameters of an object obtained from  \code{mlnorm} are  \code{mean} and  \code{sd}.
#'    The confidence intervals are parametric bootstrap percentile intervals with limits \code{(1-level)/2} and \code{1 - (1-level)}.
#'
#' @param object An object of class \code{univariateML}.
#' @param parm Vector of strings; the parameters to calculate a confidence
#'    interval for. Each parameter must be a member of \code{names(object)}.
#' @param level The confidence level.
#' @param Nreps Number of bootstrap iterations. Passed to \code{\link{bootstrapml}}.
#' @param ... Additional arguments passed to \code{\link{bootstrapml}}.
#' @return A matrix or vector with columns giving lower and upper confidence
#'    limits for each parameter in \code{parm}.
#' @seealso \code{\link[stats]{confint}} for the generic function and \code{\link{bootstrapml}} for the
#'    function used to calculate the confidence intervals.
#' @export
#' @examples
#' object <- mlinvgauss(airquality$Wind)
#' confint(object) # 95% confidence interval for mean and shape
#' confint(object, "mean") # 95% confidence interval for the mean parameter
#' # confint(object, "variance") # Fails since 'variance isn't a main parameter.
confint.univariateML <- function(object, parm = NULL, level = 0.95, Nreps = 1000, ...) {
  if (is.null(parm)) parm <- names(object)

  assertthat::assert_that(all(parm %in% names(object)),
    msg =
      "'parm' must contain valid parameter names or be NULL"
  )

  indices <- which(names(object) %in% parm)

  map <- function(x) x[indices]

  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)

  bootstrapml(object, map = map, probs = probs, Nreps = Nreps, ...)
}
