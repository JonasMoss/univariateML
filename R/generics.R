plot_wrangler = function(x, range, points = FALSE, ...) {

  if(is.null(range)) {
    if(abs(attr(x, "support")[1]) + abs(attr(x, "support")[2]) < Inf) {
      limits = attr(x, "support")
    } else if (abs(attr(x, "support")[1]) == 0 & abs(attr(x, "support")[2]) == Inf) {
      limits = c(0, qml(0.99, x))
    } else {
      limits = qml(c(0.01, 0.99), x)
    }

    range = seq(limits[1], limits[2], length.out = 1000)

  }

  defaults = list(type = if(points) "p" else "l",
                  main = paste0(attr(x, "model"), " model"),
                  ylab = "Density",
                  xlab = "x",
                  lwd  = 1)

  args = listmerge(x = defaults,
                   y = list(...))

  args$x = range
  args$y = dml(args$x, x)
  args

}


#' @export
plot.univariateML = function(x, range = NULL, ...) {

  args = plot_wrangler(x, range, points = FALSE, ...)
  do.call(graphics::plot, args)
  invisible(x)

}

#' @export
lines.univariateML = function(x, range = NULL, ...) {

  args = plot_wrangler(x, range, points = FALSE, ...)
  do.call(graphics::lines, args)
  invisible(x)

}

#' @export
points.univariateML = function(x, range = NULL, ...) {
  args = plot_wrangler(x, range, points = TRUE, ...)
  do.call(graphics::points, args)
  invisible(x)

}

#' @export
logLik.univariateML = function(object, ...) {
  val = attr(object, "logLik")
  attr(val, "nobs") = attr(object, "n")
  attr(val, "df")   = length(object)
  class(val) = "logLik"
  val
}

#' @export
coef.univariateML = function(object, ...) {
  stats::setNames(as.numeric(object), names(object))
}

#' @export
summary.univariateML <- function(x, ...) {
  data.name =  deparse(as.list(attr(x, "call"))$x)
  digits = list(...)$digits
  cat("\nMaximum likelihood for the", attr(x, "model"), "model \n",
      "\nCall: ", deparse(attr(x, "call")), "\n\nEstimates: \n")
  print.default(format(x, digits = digits), print.gap = 2L, quote = FALSE)
  cat("\nData:            ", data.name, " (", attr(x, "n"), " obs.)\n",
      "Support:         (", attr(x, "support")[1], ", ", attr(x, "support")[2],   ")\n",
      "Density:         ", attr(x, "density"), "\n",
      "Log-likelihood:  ", attr(x, "logLik"), "\n",
      sep = "")
  invisible(x)
}

#' @export
print.univariateML <- function(object, ...) {
  digits = list(...)$digits
  cat("Maximum likelihood estimates for the", attr(object, "model"), "model \n")
  print.default(format(object, digits = digits), print.gap = 2L, quote = FALSE)
  invisible(object)
}
