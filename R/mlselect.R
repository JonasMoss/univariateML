#' Fit multiple models and select the best fit
#'
#' Selects the best model by log-likelihood, AIC, or BIC.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param families a character vector containing the distribution families to
#'   select from; see `print(univariateML_families)`.
#' @param criterion the model selection criterion.
#' @param na.rm logical. Should missing values be removed?
#' @param ... unused.
#' @return `mlselect` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for the parameters of the best fitting model and the following
#'    attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlselect(precip)
#' @seealso
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export

mlselect <- function(x, families = univariateML_families,
                     criterion = c("aic", "bic", "loglik"),
                     na.rm = FALSE, ...) {
  check_families(families)
  criterion <- match.arg(criterion)

  mlf <- sapply(paste0("ml", families), function(x) eval(parse(text = x)))
  fits <- lapply(mlf, function(f) try(f(x, na.rm = na.rm), silent = TRUE))

  ## catch out-of-bounds errors (and similar)
  error_inds <- sapply(fits, function(fit) inherits(fit, "try-error"))
  error_msgs <- sapply(fits[error_inds], as.character)
  if (all(error_inds)) {
    details <- paste0("(", names(error_msgs), ") ",  error_msgs)
    stop("couldn't fit any model.\n", details)
  }

  ## select best model
  fits <- fits[!error_inds]
  crits <- switch(
    criterion,
    "loglik" = -sapply(fits, stats::logLik),
    "aic"    = sapply(fits, stats::AIC),
    "bic"    = sapply(fits, stats::BIC)
  )
  fits[[which.min(crits)]]
}

#' Implemented distribution families
#'
#' @examples
#' print(univariateML_families)
#' @export
univariateML_families <- substring(densities, first = 3)

check_families <- function(families) {
  is_implemented <- families %in% univariateML_families
  if (any(!is_implemented)) {
    stop(
      "The following families are not implemented: ",
         paste0(families[!is_implemented], collapse = ", "),
         ".\n See `print(univariateML_families)` for allowed values.")
  }
}