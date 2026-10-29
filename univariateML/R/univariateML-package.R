#' univariateML
#'
#' An `R`-package for fast, easy, and reliable maximum likelihood
#' estimation for a selection of parametric univariate densities.
#'
#' Data analysis often requires the estimation of univariate densities.
#' Maximum likelihood estimation is sensible for almost every univariate
#' density used in practice. Moreover, the maximum likelihood estimator is
#' typically consistent and efficient.
#'
#' The purpose of this package is to
#' \itemize{
#'   \item Support maximum likelihood estimation of a large selection of
#'         densities.
#'   \item Supports plenty of generics such as `plot` and `AIC` to
#'         aid your data analysis.
#' }
#'
#' @section Runtime reference repairs:
#' Evaluation uses runtime-checked repairs in
#' `univariateML_reference_fixes` for `sads::qzipf`;
#' `extraDistr::ptriang`, `pdweibull`, `dtnorm`, `ptnorm`, `qtnorm`,
#' `qgev`, `rgev`, and the upper-tail log of `pgev`;
#' and `actuar::dtrgamma`, `ptrgamma`, `qtrgamma`, `rtrgamma`,
#' `dinvtrgamma`, `pinvtrgamma`, `qinvtrgamma`, and `rinvtrgamma`.
#' The truncated-normal repairs also apply to `mlhnorm`.
#' Every repair checks its defect on each call and turns off when the
#' reference is fixed. Fitting and log likelihoods use the C++ implementation.
#' `extraDistr` has no beta-binomial quantile function; `qml` for `mlbbinom`
#' inverts `extraDistr::pbbinom` (`univariateML_reference_quantiles`).
#'
#' Read the vignettes to learn more about univariateML:
#' `browseVignettes(package = "univariateML")`
#'
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @importFrom extraDistr dlomax
#' @importFrom actuar dburr
#' @importFrom fGarch dged
#' @importFrom logitnorm dlogitnorm
#' @importFrom nakagami dnaka
#' @importFrom sads dzipf
#' @useDynLib univariateML, .registration = TRUE
## usethis namespace: end
NULL
