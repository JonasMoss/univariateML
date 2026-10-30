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
#' Evaluation uses internal C++ d/p/q/r functions checked against high-precision
#' truth tables, with stats functions for the fourteen stats families.
#'
#' Read the vignettes to learn more about univariateML:
#' `browseVignettes(package = "univariateML")`
#'
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib univariateML, .registration = TRUE
## usethis namespace: end
NULL
