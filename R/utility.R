#' Merges two lists.
#'
#' @keywords internal
#' @param x A list of default arguments.
#' @param y A list of supplied arguments
#' @param type If `merge`, the list will be merge with y
#' having priority; if `template`, named the elements of
#' y not in x will be discarded after merging.
#' @return A merged list where conflicts are solved in favor
#' of y. Does not preserve ordering.

listmerge <- function(x, y, type = c("merge", "template")) {
  type <- match.arg(type)

  if (length(y) == 0) {
    return(x)
  }

  ## Keep and not-keep are quite different.
  if (type == "merge") {
    matches <- match(names(y), names(x))
    elements_to_discard <- matches[!is.na(matches)]
    if (length(elements_to_discard) == 0) {
      combined <- c(y, x)
    } else {
      combined <- c(y, x[-elements_to_discard])
    }
    return(combined)
  }

  if (type == "template") {
    matches <- match(names(x), names(y))
    x[!is.na(matches)] <- y[matches[!is.na(matches)]]
    return(x)
  }
}

#' Simulate `n` observations from a `ml***` string using default parameters.
#' @param dens `ml***` string.
#' @param n Number of samples to take.
#' @keywords internal
simulate_default <- function(dens, n) {
  to_rdist <- function(x) {
    strings <- strsplit(x, "::")[[1]]
    substring(strings[2], first = 1, last = 1) <- "r"
    paste0(strings[1], "::", strings[2])
  }
  params <- univariateML_metadata[[dens]]$default
  names(params) <- univariateML_metadata[[dens]]$names
  params["n"] <- n
  rdist <- to_rdist(univariateML_metadata[[dens]]$density)
  rlang::exec(parse(text = rdist)[[1]], !!!as.list(params))
}

#' One-dimensional Newton--Raphson
#' @param f_over_df The term f(x_0) / f'(x_0) in Newton--Raphson.
#' @param param0 Starting parameter, typically from method of moments estimates.
#' @param ... Optional `iterlim` and `reltol` parameters.
#' @return Newton--Raphson estimate.
#' @keywords internal
newton_raphson_1d <- function(f_over_df, param0, ...) {
  dots <- list(...)
  reltol <- if (!is.null(dots$reltol)) dots$reltol else .Machine$double.eps^0.25
  iterlim <- if (!is.null(dots$iterlim)) dots$iterlim else 100

  for (i in seq(iterlim)) {
    param <- param0 - f_over_df(param0)
    if (is.nan(param)) stop("NaN in Newton--Raphson iteration.")
    if (abs((param0 - param) / param0) < reltol) break
    param0 <- param
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }
  param
}

#' Returns appropriate starting value
#' @param default Function to calculate default parameter value.
#' @param name Name of default starting value.
#' @param ... Parameter list containing an element `name`.
#' @return Default value if `name` is not present
get_start <- function(default, name, ...) {
  dots <- list(...)
  if (!is.null(dots[[name]])) {
    return(dots[[name]])
  }
  default()
}


#' Inverse digamma function
#'
#' Calculates the inverse digamma function using Newton--Raphson. Works for
#' y > -500. Uses Newton--Raphson with relative tolerance of eps^0.25.
#'
#' The number of iterations are few, 1 for most input values, especially those
#' that are large. The starting value is the lower bound found by Batir (2017).
#'
#' @param y Values to invert.
#' @references Batir, N. (2017). Inequalities for the inverses of the polygamma functions. arXiv. http://arxiv.org/abs/1705.06547

inverse_digamma <- function(y) {
  if (y < -500) {
    stop("y must be greater than -500")
  }

  param0 <- 1 / log1p(exp(-y))
  reltol <- .Machine$double.eps^0.25

  for (i in seq(5)) {
    param <- param0 + (y - digamma(param0)) / trigamma(param0)
    if (abs((param0 - param) / param0) < reltol) break
    param0 <- param
  }

  param0
}
