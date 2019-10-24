#' Wrangles arguments for use in ppml and qqml functions.
#'
#' @param y The input data.
#' @param obj Function or \code{"univariateML"} object.
#' @param datax logical; if true, plots the data on the x axis.
#' @param ... Arguments passed to \code{plot} or \code{points} down the line.
#' @keywords internal

ppqq_wrangler = function(y, obj, datax, pp, ...) {

  ## Nas are removed by default in this function, following qqplot.

  y = y[!is.na(y)]

  ## Error message straight out of stats::qqplot.
  if (0 == (n <- length(y))) stop("y is empty or has only NAs")

  ## I must check if the object is a "univariateML" object or a function that
  ## returns a "univariateML" object. If neither, an error is thrown.

  if(inherits(obj, "univariateML")) {
    obj = obj
  } else {
    obj = obj(y)
  }

  n = length(y)

  if(pp) {
    x = ((1:n)/(n+1))[order(order(y))]
    y = pml(q = y, obj = obj)
  } else {
    x = qml((1:n)/(n+1), obj = obj)[order(order(y))]
  }


  defaults = list(main = paste0(attr(obj, "model"), " P-P plot"),
                  ylab = "Density",
                  xlab = "x",
                  lwd  = 1)

  args = listmerge(x = defaults,
                   y = list(...))

  if(!datax) {
    args$x = x
    args$y = y
  } else {
    args$x = y
    args$y = x
  }

  pp = NULL
  pp$value =  if (datax) list(x = y, y = x) else list(x = x, y = y)
  pp$args = args
  pp

}

#' Make Probability Plots Using Maximum Likelihood Estimates
#'
#' Quantile-quantile plots and Probability-probability plots using maximum
#'   likelihood estimation.
#'
#' qqmlplot produces a quantile-quantile plot (Q-Q plot) of the values in
#' \code{y} with respect to the distribution defined by \code{obj}, which is
#' either a \code{univariateML} object or a function returning a
#' \code{univariateML} object when called with \code{y}. \code{qqmlline} adds a
#' line to a “theoretical”, quantile-quantile plot which passes through
#' the \code{probs} quantiles, by default the first and third quartiles.
#' \code{qqmlpoints}behaves like \code{stats::points} and adds a Q-Q plot to
#' an existing plot.
#'
#' \code{ppmlplot}, \code{ppmlline}, and \code{ppmlpoints} produce
#' probability-probability plots (or P-P plots). They behave similarily to the
#' quantile-quantile plot functions.
#'
#' This function is modelled after \code{stats::qqnorm}.
#'
#' Graphical parameters may be given as arguments to all the functions below.
#'
#' @param y Numeric vector; The data to plot on the \code{y} axis when
#'   \code{datax} is \code{FALSE}.
#' @param obj Either an \code{univariateML} object or a function that returns
#'   a \code{univariateML} object when called with \code{y} as its only
#'   argument.
#' @param plot.it Logical; should the result be plotted?
#' @param datax Logical; should \code{y} be plotted on the \code{x}-axis? Defaults
#'   to \code{FALSE} in \code{qqmlplot} and \code{ppmlplot} bu \code{TRUE} in
#'   \code{qqmlpoints} and \code{ppmlpoints}.
#' @param probs Numeric vector of length two, representing probabilities.
#'   Corresponding quantile pairs define the line drawn.
#' @param qtype The \code{type} of quantile computation used in \code{quantile}.
#' @param ... Graphical parameters.
#' @examples
#'   (add examples)
#' @references
#'   (add references to pp plots and qq plots)

ppmlplot = function(y, obj, plot.it = TRUE, datax = FALSE, ...) {

  pp = ppqq_wrangler(y, obj, datax, pp = TRUE, ...)
  if(plot.it) do.call(graphics::plot, pp$args)
  invisible(pp$value)

}

ppmlline = function(...) abline(a = 0, b = 1, ...)

ppmlpoints = function(y, obj, plot.it = TRUE, datax = TRUE, ...) {

  pp = ppqq_wrangler(y, obj, datax, pp = TRUE, ...)
  if(plot.it) do.call(graphics::points, pp$args)
  invisible(pp$value)

}

qqmlplot = function(y, obj, plot.it = TRUE, datax = FALSE, ...) {

  qq = ppqq_wrangler(y, obj, datax, pp = FALSE, ...)
  if(plot.it) do.call(graphics::plot, qq$args)
  invisible(qq$value)

}

qqmlline = function(...) abline(a = 0, b = 1, ...)

qqmlpoints = function(y, obj, plot.it = TRUE, datax = TRUE, ...) {

  qq = ppqq_wrangler(y, obj, datax, pp = FALSE, ...)
  if(plot.it) do.call(graphics::points, qq$args)
  invisible(qq$value)

}

