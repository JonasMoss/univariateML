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
  as.numeric(object)
}