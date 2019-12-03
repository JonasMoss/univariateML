#' Wrangles arguments for use in the plot, lines and points functions.
#'
#' @param x The input data.
#' @param range Range of the data.
#' @param points Boolean; should points be plotted by default?
#' @keywords internal

plot_wrangler <- function(x, range, points = FALSE, ...) {
  support <- attr(x, "support")
  if (is.null(range)) {
    if (abs(support[1]) + abs(support[2]) < Inf) {
      limits <- support
    } else if (abs(support[1]) == 0 & abs(support[2]) == Inf) {
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
#' The `plot`, `lines`, and `points` methods for
#'    `univariateML` objects.
#'
#' @export
#' @param x a `univariateML` object.
#' @param range range of `x` values to plot, i.e. `c(lower, upper)`.
#' @param ... parameters passed to `plot`, `lines`, or `points`.
#' @return An invisible copy of `x`.
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
  support <- attr(object, "support")
  cat(
    "\nMaximum likelihood for the", attr(object, "model"), "model \n",
    "\nCall: ", deparse(attr(object, "call")), "\n\nEstimates: \n"
  )
  print.default(format(object, digits = digits), print.gap = 2L, quote = FALSE)
  cat("\nData:            ", data.name, " (", attr(object, "n"), " obs.)\n",
    "Support:         (", support[1], ", ", support[2], ")\n",
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
#' Computes a confidence interval for one or more parameters in a
#'    `unvariateML` object.
#'
#' `confint.univariateML` is a wrapper for  [bootstrapml()] that
#'    computes confidence intervals for the main parameters of `object`.
#'    The main parameters of `object` are the members of
#'    `names(object)`. For instance, the main parameters of an object
#'    obtained from  `mlnorm` are  `mean` and  `sd`. The
#'    confidence intervals are parametric bootstrap percentile intervals
#'    with limits `(1-level)/2` and `1 - (1-level)`.
#'
#' @param object An object of class `univariateML`.
#' @param parm Vector of strings; the parameters to calculate a confidence
#'    interval for. Each parameter must be a member of `names(object)`.
#' @param level The confidence level.
#' @param Nreps Number of bootstrap iterations. Passed to
#'    [bootstrapml()].
#' @param ... Additional arguments passed to [bootstrapml()].
#' @return A matrix or vector with columns giving lower and upper confidence
#'    limits for each parameter in `parm`.
#' @seealso [stats::confint()] for the generic function and
#'    [bootstrapml()] for the function used to calculate the
#'    confidence intervals.
#' @export
#' @examples
#' object <- mlinvgauss(airquality$Wind)
#' confint(object) # 95% confidence interval for mean and shape
#' confint(object, "mean") # 95% confidence interval for the mean parameter
#' # confint(object, "variance") # Fails since 'variance isn't a main parameter.
confint.univariateML <- function(object,
                                 parm = NULL,
                                 level = 0.95,
                                 Nreps = 1000,
                                 ...) {
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
