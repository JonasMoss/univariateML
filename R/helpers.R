#' Transform a univariateML object to a string specifying quantile, CDF,
#'     density or random variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_string = function(obj, type = c("d", "p", "q", "r")){
  type = match.arg(type)
  strings = strsplit(attr(obj, "density"), "::")[[1]]
  substring(strings[2], first = 1, last = 1) = type
  q = paste0(strings[1], "::", strings[2])
}

#' Transform a univariateML object to a quantile, CDF, density or random
#'    variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_function = function(obj, type = c("d", "p", "q", "r")){
  type = match.arg(type)
  string = univariateML_to_string(obj, type = type)
  fun = eval(parse(text = string))
  for(i in 1:length(obj)) formals(fun)[[names(obj)[i]]] = unname(obj[i])
  fun
}