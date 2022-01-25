#' Fit multiple models and select the best fit
#'
#' Selects the best model by log-likelihood, AIC, or BIC.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param models a character vector containing the distribution models to
#'   select from; see `print(univariateML_models)`.
#' @param criterion the model selection criterion. Must be one of `"aic"`,
#'   `"bic"`, and `"loglik"`.
#' @param na.rm logical. Should missing values be removed?
#' @param ... unused.
#' @return `model_select` returns an object of [class][base::class]
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
#' model_select(precip)
#' @seealso
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export

model_select <- function(x, models = univariateML_models,
                         criterion = c("aic", "bic", "loglik"),
                         na.rm = FALSE, ...) {
  check_models(models)
  criterion <- match.arg(criterion)

  mlf <- sapply(paste0("ml", models), function(x) eval(parse(text = x)))
  fits <- lapply(mlf, function(f) try(f(x, na.rm = na.rm), silent = TRUE))

  ## catch out-of-bounds errors (and similar)
  error_inds <- sapply(fits, function(fit) inherits(fit, "try-error"))
  if (all(error_inds)) {
    error_msgs <- sapply(fits[error_inds], as.character)
    details <- paste0("(", names(error_msgs), ") ", error_msgs)
    stop("couldn't fit any model.\n", details)
  }

  ## select best model
  fits <- fits[!error_inds]
  crits <- switch(criterion,
    "loglik" = -sapply(fits, stats::logLik),
    "aic"    = sapply(fits, stats::AIC),
    "bic"    = sapply(fits, stats::BIC)
  )
  fits[[which.min(crits)]]
}

#' Implemented models
#'
#' @examples
#' print(univariateML_models)
#' @export
univariateML_models <- substring(densities, first = 3)

check_models <- function(models) {
  is_implemented <- models %in% univariateML_models
  if (any(!is_implemented)) {
    stop(
      "The following families are not implemented: ",
      paste0(models[!is_implemented], collapse = ", "),
      ".\n See `print(univariateML_models)` for allowed values."
    )
  }
}
