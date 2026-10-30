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

# Internal tooling convenience; public simulation uses rml(n, mlX()).
ml_simulate <- function(dens, n) rml(n, get(dens, mode = "function")())

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

  #print(paste0("Iter: ", i))

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
  gamma_const <- 0.57721566490153286060
  zeta3  <- 1.2020569031595942854
  x <- y
  y <- if (x < -3.5) {
    t  <- x + gamma_const
    -1/t + (pi^2/6)/t^3 + (zeta3/2)/t^4
  } else if (x > 2) {
    0.5 / log1p(exp(-x)) + 0.5*(exp(x) + 0.5)
  } else {
    1 / log1p(exp(-x))
  }

  for (k in 1:6) {
    r <- digamma(y) - x
    y <- y - r / trigamma(y)
    if (abs(r) < 8*.Machine$double.eps) break
  }
  y
}



#' Log-moments estimator for Burr c parameter
#'
#' @param m1 Expected log moment.
#' @param m2 Variance log moment.
#' @return Estimated c in Burr.
logmom_burr_c <- function(m1, m2) {
  psi1_1 <- trigamma(1)

  f_over_df <- function(c) {
    k <- inverse_digamma(digamma(1) - c * m1)

    psi1_k <- trigamma(k)
    psi2_k <- psigamma(k, deriv = 2)

    c_2 <- c^(-2)
    c_3 <- c^(-3)
    term1_prime <- -2 * c_3 * (psi1_1 + psi1_k)
    term2_prime <- c_2  * (psi2_k * (-m1 / psi1_k))

    f <- c_2* (psi1_1 + psi1_k) - m2
    df <- term1_prime + term2_prime
    f / df
  }

  newton_raphson_1d(f_over_df, pi / sqrt(3 * m2))
}
