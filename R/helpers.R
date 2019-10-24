#' Transform a univariateML object to a string specifying quantile, CDF,
#'     density or random variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_string = function(obj, type = c("d", "p", "q", "r", "ml")){
  type = match.arg(type)
  if(type %in% c("d", "p", "q", "r")) {
    strings = strsplit(attr(obj, "density"), "::")[[1]]
    substring(strings[2], first = 1, last = 1) = type
    paste0(strings[1], "::", strings[2])
  } else {
    paste0(type, substring(strsplit(attr(obj, "density"), "::")[[1]][2], 2))
  }

}

#' Transform a univariateML object to a quantile, CDF, density or random
#'    variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_function = function(obj, type = c("d", "p", "q", "r", "ml")){
  type = match.arg(type)
  string = univariateML_to_string(obj, type = type)
  fun = eval(parse(text = string))
  if(type %in% c("d", "p", "q", "r")) {
    for(i in 1:length(obj)) formals(fun)[[names(obj)[i]]] = unname(obj[i])
  }
  fun
}

#' Data and function to 'univariateML'
#' @param y Data to transform.
#' @param obj Function to apply.
#'

to_univariateML = function(y, obj) {

  if(inherits(obj, "univariateML")) {
    obj = obj
  } else {
    msg = "obj must be either a functiona returning a univariateML object or an univariateML object."
    tryCatch({obj = obj(y)}, error = function(cond) stop(msg))
    stopifnot(inherits(obj, "univariateML"))
  }

  obj

}


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
  if (0 == length(y)) stop("y is empty or has only NAs")

  ## I must check if the object is a "univariateML" object or a function that
  ## returns a "univariateML" object. If neither, an error is thrown.

  obj = to_univariateML(y, obj)

  n = length(y)

  if(pp) {
    x = ((1:n)/(n+1))[order(order(y))]
    y = pml(q = y, obj = obj)
  } else {
    x = qml((1:n)/(n+1), obj = obj)[order(order(y))]
  }


  defaults = list(lwd  = 1)

  if(!pp) {
    defaults$main = paste0(attr(obj, "model"), " Q-Q plot")
    if(!datax) {
      defaults$ylab = "Sample Quantiles"
      defaults$xlab = "Approximate Theoretical Quantiles"
    } else {
      defaults$ylab = "Approximate Theoretical Quantiles"
      defaults$xlab = "Sample Quantiles"
    }
  } else {
    defaults$main = paste0(attr(obj, "model"), " P-P plot")
    if(!datax) {
      defaults$ylab = "Sample Cumulative Probability"
      defaults$xlab = "Theoretical Cumulative Probability"
    } else {
      defaults$ylab = "Theoretical Cumulative Probability"
      defaults$xlab = "Sample Cumulative Probability"
    }
  }


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