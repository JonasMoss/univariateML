#' arametric Bootstrap on Distributions Fitted with Maximum Likelihood
#'
#' @param obj A \code{univariateML} object.
#' @param Nreps Positive integer. The number of bootstrap samples.
#' @param map A function of the parameters of the \code{univariateML} object.
#'   Defaults to the identity.
#' @param reducer A reducer function. Defaults to \code{stats::quantile} with default
#'   argument \code{probs = c(0.025, 0.975)}.
#' @param ... Passed to reduce.
#' @return The transposed map-reduced bootstrap samples.

bootstrapml = function(obj, Nreps = 1000, map = identity, reducer = stats::quantile, ...) {

  n = attr(obj, "n")

  name = substring(strsplit(attr(obj, "density"), "::")[[1]][2], 2)
  call_to_eval = call(paste0("ml", name), parse(text = "rml(n, obj)")[[1]])

  r_fun = univariateML_to_function(obj, type = "r")
  ml_fun = univariateML_to_function(obj, type = "ml")
  bootstraps = replicate(n = n, expr = ml_fun(r_fun(n)))

  defaults = NULL

  if(identical(reducer, stats::quantile)) defaults = list(probs = c(0.025, 0.975))

  args = listmerge(x = defaults, y = list(...))

  mapped = apply(bootstraps, 2, map)

  if(is.null(dim(mapped))) {
    do.call(function(...) reducer(mapped, ...), args)
  } else {
    do.call(function(...) t(apply(mapped, 1, reducer, ...)), args)
  }

}