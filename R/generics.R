# plot.univariateML = function(x, y, ...) {
#
#   support = attr(x, "support")
#   dens = eval(parse(text = univariateML_to_density_string(obj, "d")))
#   quant = eval(parse(text = univariateML_to_density_string(obj, "q")))
#
#
# }

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
