#' Fit multiple models and select the best fit
#'
#' Selects the best model by log-likelihood, AIC, or BIC.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param models a character vector containing the distribution models to
#'   select from; see `print(univariateML_models)`.
#' @param criterion the model selection criterion. Must be one of `"aic"`,
#'   `"bic"`, and `"loglik"`. Defaults to `"aic"`.
#' @param na.rm logical. Should missing values be removed?
#' @param rtn_class character length 1. "univariateML" (default) if the function
#'   should return the single best model; "data.frame" if a tibble data frame
#'   of all results should be returned, sorted by decreasing model performance.
#' @param ... unused.
#' @return The return value depends on the `rtn_class` argument.
#'    For `rtn_class = "univariateML"` (default), `model_select` returns
#'     an object of [class][base::class] #'    `univariateML`.
#'     This is a named numeric vector with maximum likelihood
#'    estimates for the parameters of the best fitting model and the following
#'    attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#'
#'    For `rtn_class = "data.frame"`, `model_select` returns a tibble data frame
#'    with the following columns:
#'     \item{`model`}{The name of the model.}
#'     \item{`d_loglik, d_aic, d_bic`}{See `loglik, aic, bic`.}
#'     \item{`loglik, aic, bic`}{The negative loglikelihood at the maximum,
#'     the AIC, and the BIC, respectively. The minimum of each of these is noted
#'     and then subtracted from each value to give their delta versions
#'     `d_loglik, d_aic, d_bic`}. So, the model with the lowest AIC will
#'     have `d_aic` of 0; the `d_aic` of all the other models shows how much higher
#'     their AICs are from the minimum. The same goes with `d_loglik` and `d_bic`.
#'     \item{`ml`}{The internal code name for the model.}
#'     \item{`univariateML`}{The `univariateML` object for the model. This is
#'     identical to the value returned for `rtn_class = "univariateML"`; for
#'     `rtn_class = "data.frame"`, this object is returned for all tested models,
#'     not just the best one.}
#' @examples
#' # Select among all possible models.
#' model_select(precip)
#'
#' # View possible models to fit.
#' print(univariateML_models)
#'
#' # Try out only gamma, Weibull, and exponential.
#' model_select(precip, c("gamma", "weibull", "exp"))
#'
#' @seealso
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export

model_select <- function(x, models = univariateML_models,
                         criterion = c("aic", "bic", "loglik"),
                         na.rm = FALSE, rtn_class = "univariateML", ...) {
  check_models(models)
  if (!(rtn_class %in% c("univariateML", "data.frame"))) {
    stop("rtn_class must be either 'univariateML' or 'data.frame'.")
  }

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

  if (rtn_class == "univariateML") {
    crits <- switch(criterion,
                    "loglik" = -sapply(fits, stats::logLik),
                    "aic"    = sapply(fits, stats::AIC),
                    "bic"    = sapply(fits, stats::BIC)
    )

    return(fits[[which.min(crits)]])
  }
  else {  # rtn_class == "data.frame"
    fits_tbl <- tibble::tibble(
      model = sapply(fits, function(x) attr(x, "model")) |> unname(),
      "loglik" = -sapply(fits, stats::logLik) |> unname(),
      "aic"    = sapply(fits, stats::AIC) |> unname(),
      "bic"    = sapply(fits, stats::BIC) |> unname(),
      ml = names(fits) |>
        gsub("^ml", "", x = _),
      univariateML = fits
    )
    # Sort by criterion
    fits_tbl <- fits_tbl[order(fits_tbl[[criterion]]), ]

    # Add delta criteria (difference between each criterion and the minimum)
    fits_tbl$d_loglik <- fits_tbl$loglik - min(fits_tbl$loglik)
    fits_tbl$d_aic    <- fits_tbl$aic    - min(fits_tbl$aic)
    fits_tbl$d_bic    <- fits_tbl$bic    - min(fits_tbl$bic)

    # Rearrange the columns in a more useful order
    fits_tbl <- fits_tbl[, c(
      "model", "d_loglik", "d_aic", "d_bic", "loglik", "aic", "bic", "ml", "univariateML"
    )]

    return(fits_tbl)
  }
}

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
