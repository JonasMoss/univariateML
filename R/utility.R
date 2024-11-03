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

listmerge <- \(x, y, type = c("merge", "template")) {
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

#' @keywords internal
get_reltol <- \(dots) {
  if (!is.null(dots$reltol)) {
    dots$reltol
  } else {
    .Machine$double.eps^0.25
  }
}

#' @keywords internal
get_iterlim <- \(dots) {
  if (!is.null(dots$iterlim)) dots$iterlim else 100
}

#' @keywords internal
check_iterlim <- \(i, iterlim, reltol) {
  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }
}

#' Simulate `n` observations from a `ml***` string using default parameters.
#' @param dens `ml***` string.
#' @param n Number of samples to take.
#' @keywords internal
simulate_default <- \(dens, n) {
  to_rdist <- \(x) {
    strings <- strsplit(x, "::")[[1]]
    substring(strings[2], first = 1, last = 1) <- "r"
    paste0(strings[1], "::", strings[2])
  }
  params <- metadata[[dens]]$default
  names(params) <- metadata[[dens]]$names
  params["n"] <- n
  rdist <- to_rdist(metadata[[dens]]$density)
  rlang::exec(parse(text = rdist)[[1]], !!!as.list(params))
}

#' One-dimensional Newton--Raphson
#' @param f_over_df The term f(x_0) / f'(x_0) in Newton--Raphson.
#' @param param0 Starting parameter, typically from method of moments estimates.
#' @param iterlim Iteration limit.
#' @param reltol Relative tolerance.
#' @return Newton--Raphson estimate.
#' @keywords internal
newton_raphson_1d <- \(f_over_df, param0, ...) {
  dots <- list(...)
  reltol <- get_reltol(dots)
  iterlim <- get_iterlim(dots)

  for (i in seq(iterlim)) {
    param <- param0 - f_over_df(param0)

    if (is.nan(param)) stop("NaN in Newton--Raphson iteration.")
    if (abs((param0 - param) / param0) < reltol) break

    param0 <- param
  }

  check_iterlim(i, iterlim, reltol)
  param
}
