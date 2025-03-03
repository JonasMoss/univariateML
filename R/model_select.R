#' Fit multiple models and select the best fit
#'
#' Selects the best model by log-likelihood, aic, or bic.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param models a character vector containing the distribution models to
#'   select from; see `print(univariateML_models)`. Defaults to all implemented models.
#' @param criterion the model selection criterion. Must be one of `"AIC"`,
#'   `"BIC"`, and `"logLik"`, ignoring case. Defaults to `"AIC"`.
#' @param na.rm logical. Should missing values be removed?
#' @param type Either `"both"`, `"discrete"`, or `"continuous"`. The supplied `models`
#'   vector is restricted to the desired class.
#' @param return character length 1. "univariateML" (default) if the function
#'   should return the single best model; "all" if a tibble data frame
#'   of all results should be returned, sorted by decreasing model performance.
#' @param ... unused.
#' @return The return value depends on the `return` argument.
#'    For `return = "best"` (default), `model_select` returns
#'    an object of [class][base::class] `univariateML`
#'
#'    For `return = "all"`, `model_select` returns a tibble data frame
#'    with the following columns:
#'     \item{`model`}{The name of the model.}
#'     \item{`d_loglik, d_aic, d_bic`}{See `loglik, aic, bic`.}
#'     \item{`p`}{Number of parameters fitted.}
#'     \item{`loglik, aic, bic`}{The negative log-likelihood at the maximum,
#'     the aic, and the bic, respectively. The minimum of each of these is noted
#'     and then subtracted from each value to give their delta versions
#'     `d_loglik, d_aic, d_bic`}. So, the model with the lowest aic will
#'     have `d_aic` of 0; the `d_aic` of all the other models shows how much higher
#'     their aics are from the minimum. The same goes with `d_loglik` and `d_bic`.
#'     \item{`ml`}{The internal code name for the model.}
#'     \item{`univariateML`}{The `univariateML` object for the model. This is
#'     `return = "all"`, this object is returned for all tested models.}
#' @examples
#' # Select among all possible continuous models.
#' model_select(precip, type = "continuous")
#'
#' # View possible models to fit.
#' print(univariateML_models)
#'
#' # Try out only gamma, Weibull, and exponential.
#' model_select(precip, c("gamma", "weibull", "exp"))
#'
#' # Fit the discrete `corbet` data to all available discrete models
#' model_select(corbet, type = "discrete", return = "all")
#'
#' @seealso
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export

model_select <- function(
    x, models = univariateML_models,
    criterion = c("AIC", "BIC", "logLik"),
    na.rm = FALSE, type = c("both", "discrete", "continuous"),
    return = c("best", "all"), ...) {
  return <- match.arg(return)
  type <- match.arg(type)

  if (missing(criterion)) criterion <- "aic"
  criterion <- match.arg(tolower(criterion[1]), choices = c("aic", "bic", "loglik"))
  criterion <- toupper(criterion)

  check_models(models)
  models <- filter_models(models, type)

  mlf <- sapply(paste0("ml", models), function(x) eval(parse(text = x)))
  fits <- lapply(mlf, function(f) try(f(x, na.rm = na.rm), silent = TRUE))

  error_inds <- sapply(fits, function(fit) inherits(fit, "try-error"))
  if (all(error_inds)) {
    error_msgs <- sapply(fits[error_inds], as.character)
    details <- paste0("(", names(error_msgs), ") ", error_msgs)
    stop("Couldn't fit any model.\n", details)
  }

  fits <- fits[!error_inds]

  if (return == "best") {
    crits <- switch(criterion,
      "LOGLIK" = -sapply(fits, stats::logLik),
      "AIC"    = sapply(fits, stats::AIC),
      "BIC"    = sapply(fits, stats::BIC)
    )
    return(fits[[which.min(crits)]])
  }

  fits <- tibble::tibble(
    model = unname(sapply(fits, function(x) attr(x, "model"))),
    LOGLIK = unname(-sapply(fits, stats::logLik)),
    AIC = unname(sapply(fits, stats::AIC)),
    BIC = unname(sapply(fits, stats::BIC)),
    ml = gsub("^ml", "", names(fits)),
    univariateML = fits
  )


  fits <- fits[order(fits[[criterion]]), ]

  fits$d_logLik <- fits$LOGLIK - min(fits$LOGLIK, na.rm = TRUE)
  fits$d_AIC <- fits$AIC - min(fits$AIC, na.rm = TRUE)
  fits$d_BIC <- fits$BIC - min(fits$BIC, na.rm = TRUE)
  fits$p <- sapply(fits$univariateML, length)
  fits$logLik <- -fits$LOGLIK

  fits <- fits[, c(
    "model", "d_logLik", "d_AIC", "d_BIC", "logLik", "p", "AIC", "BIC", "ml", "univariateML"
  )]

  fits
}

filter_models <- function(models, type) {
  if (type == "both") {
    return(models)
  }
  type_ <- if (type == "continuous") "R" else "Z"
  Filter(function(model) univariateML_metadata[[paste0("ml", model)]]$support@type == type_, models)
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
