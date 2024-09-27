#' Probability Plots Using Maximum Likelihood Estimates
#'
#' Make quantile-quantile plots and probability-probability plots using maximum
#'   likelihood estimation.
#'
#' `qqmlplot` produces a quantile-quantile plot (Q-Q plot) of the values in
#' `y` with respect to the distribution defined by `obj`, which is
#' either a `univariateML` object or a function returning a
#' `univariateML` object when called with `y`. `qqmlline` adds a
#' line to a <U+201C>theoretical<U+201D>, quantile-quantile plot which passes through
#' the `probs` quantiles, by default the first and third quartiles.
#' `qqmlpoints`behaves like `stats::points` and adds a Q-Q plot to
#' an existing plot.
#'
#' `ppmlplot`, `ppmlline`, and `ppmlpoints` produce
#' probability-probability plots (or P-P plots). They behave similarly to the
#' quantile-quantile plot functions.
#'
#' This function is modeled after [qqnorm][stats::qqnorm].
#'
#' Quantile-quantile plots and probability-probability plots are only supported
#' for continuous distributions.
#'
#' Graphical parameters may be given as arguments to all the functions below.
#'
#' @param y Numeric vector; The data to plot on the `y` axis when
#'   `datax` is `FALSE`.
#' @param obj Either an `univariateML` object or a function that returns
#'   a `univariateML` object when called with `y` as its only
#'   argument.
#' @param plot.it Logical; should the result be plotted?
#' @param datax Logical; should `y` be plotted on the `x`-axis?
#'   Defaults to `FALSE` in `qqmlplot` and `ppmlplot` but
#'   `TRUE` in `qqmlpoints` and `ppmlpoints`.
#' @param probs Numeric vector of length two, representing probabilities.
#'   Corresponding quantile pairs define the line drawn.
#' @param qtype The `type` of quantile computation used in `quantile`.
#' @param ... Graphical parameters.
#' @return For `qqmlplot`, `qqmlpoints`, `ppmlplot`, and
#'   `ppmlpoints`, a list with components `x` (plotted on the x axis)
#'   and `y` (plotted on the y axis). `qqmlline` and `ppmlline`
#'   returns nothing.
#'
#' @examples
#' ## Make a single probability plot with a line.
#'
#' obj <- mlgamma(Nile)
#' qqmlplot(Nile, obj)
#' qqmlline(Nile, obj)
#'
#' ## Make multiple probability plots. datax = TRUE must be used to make this
#' ## look good.
#'
#' ppmlplot(airquality$Wind, mlgamma, main = "Many P-P plots")
#' ppmlpoints(airquality$Wind, mlexp, col = "red")
#' ppmlpoints(airquality$Wind, mlweibull, col = "purple")
#' ppmlpoints(airquality$Wind, mllnorm, col = "blue")
#' @name ProbabilityPlots
#' @export
#' @references
#'   M. B. Wilk, R. Gnadadesikan, Probability plotting methods for the analysis
#'   for the analysis of data, Biometrika, Volume 55, Issue 1, March 1968,
#'   Pages 1<U+2013>17, https://doi.org/10.1093/biomet/55.1.1

ppmlplot <- \(y, obj, plot.it = TRUE, datax = FALSE, ...) {
  pp <- ppqq_wrangler(y, obj, datax, pp = TRUE, ...)
  if (plot.it) do.call(graphics::plot, pp$args)
  invisible(pp$value)
}

#' @rdname ProbabilityPlots
#' @export
ppmlline <- \(...) graphics::abline(a = 0, b = 1, ...)

#' @rdname ProbabilityPlots
#' @export
ppmlpoints <- \(y, obj, plot.it = TRUE, datax = TRUE, ...) {
  pp <- ppqq_wrangler(y, obj, datax, pp = TRUE, ...)
  if (plot.it) do.call(graphics::points, pp$args)
  invisible(pp$value)
}

#' @rdname ProbabilityPlots
#' @export
qqmlplot <- \(y, obj, plot.it = TRUE, datax = FALSE, ...) {
  qq <- ppqq_wrangler(y, obj, datax, pp = FALSE, ...)
  if (plot.it) do.call(graphics::plot, qq$args)
  invisible(qq$value)
}

#' @rdname ProbabilityPlots
#' @export
qqmlline <- \(y, obj, datax = FALSE, probs = c(0.25, 0.75), qtype = 7,
                     ...) {
  obj <- to_univariateML(y, obj)
  y <- stats::quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
  x <- qml(probs, obj)

  if (datax) {
    slope <- diff(x) / diff(y)
    int <- x[1L] - slope * y[1L]
  } else {
    slope <- diff(y) / diff(x)
    int <- y[1L] - slope * x[1L]
  }

  graphics::abline(int, slope, ...)
}

#' @rdname ProbabilityPlots
#' @export
qqmlpoints <- \(y, obj, plot.it = TRUE, datax = TRUE, ...) {
  qq <- ppqq_wrangler(y, obj, datax, pp = FALSE, ...)
  if (plot.it) do.call(graphics::points, qq$args)
  invisible(qq$value)
}
