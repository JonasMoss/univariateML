#' Covariance matrix of fitted coefficients
#'
#' The default sandwich covariance uses individual score contributions and
#' observed information. Under appropriate regularity conditions it estimates
#' uncertainty for the best-fitting member even when the model is misspecified;
#' it generally needs a larger sample. Observed covariance is the inverse
#' observed information and assumes the model is correctly specified.
#' Both describe the free coefficients in their natural parameter coordinates,
#' conditional on held coefficients. Availability and conditions depend on the
#' family and parameter regime; each family's dossier (`README.md` of its
#' distribution folder in the package's C++ source) lists them under
#' `inference_numeric`.
#' A condition number is scale dependent. Large values indicate sensitivity
#' to numerical error, and covariance can also be unreliable near singular
#' members or for heavy-tailed data even when no warning is issued.
#' Robust inference additionally needs independent observations, a well-defined
#' pseudo-true parameter, finite score variance, nonsingular sensitivity and
#' suitable large-sample estimator asymptotics; the method does not establish
#' those assumptions or finite-sample coverage.
#'
#' @param object A fitted `univariateML` object.
#' @param type Either `"sandwich"` (default) or `"observed"`.
#' @param ... Unused.
#' @return A named covariance matrix with attributes `type`, `held`,
#'   `condition` (the observed-information eigenvalue condition number in
#'   natural coordinates), and `validity_conditions`.
#' @examples
#' fit <- mlnorm(c(-1, 0, 0.4, 1, 2))
#' vcov(fit)
#' vcov(fit, type = "observed")
#' @export
vcov.univariateML <- function(object, type = c("sandwich", "observed"), ...) {
  if (isTRUE(attr(object,"censored")) || !is.null(attr(object,"truncate")) ||
      (!is.null(attr(object,"zero")) && attr(object,"zero")!="none"))
    stop("Covariance for modified or censored fits is not implemented", call. = FALSE)
  type <- match.arg(type)
  data <- attr(object, "inference_data")
  if (is.null(data)) stop("Covariance requires the observations from a fitted model", call. = FALSE)
  held <- univariateML_fixed(object)
  free <- attr(object, "inference_free")
  if (is.null(free) || !identical(names(free), names(object)))
    stop("Covariance requires the original coefficient order and free mask", call. = FALSE)
  if (length(attr(object, "restricted")))
    stop("Covariance unavailable at a fitting restriction", call. = FALSE)
  result <- univariateML_inference_cpp(data, substring(univariateML_name(object), 3L),
                                      as.numeric(object), free, type == "sandwich")
  reasons <- c("ok", "invalid parameters", "invalid observation", "empty sample",
               "unavailable coefficient subset", "nonfinite or numerical failure", "singular information",
               "ill-conditioned information", "too many coefficients", "indefinite information",
               "family unavailable", "regime unavailable", "boundary or limit fit")
  if (result$status != 0L) {
    reason <- reasons[result$status + 1L]
    stop("Covariance unavailable: ", reason, if (nzchar(result$validity)) paste0("; ", result$validity), call. = FALSE)
  }
  if (result$condition > 1e6)
    warning("Observed information is poorly conditioned in natural coordinates; covariance may be numerically sensitive", call. = FALSE)
  ans <- result$matrix
  dimnames(ans) <- list(names(object)[free], names(object)[free])
  attr(ans, "type") <- type
  attr(ans, "held") <- held
  attr(ans, "condition") <- result$condition
  attr(ans, "validity_conditions") <- result$validity
  ans
}
