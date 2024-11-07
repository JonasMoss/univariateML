densities <- list.files("R")
densities <- densities[sapply(densities, \(x) substr(x, 1, 2) == "ml")]
densities <- densities[!grepl("select", densities)]
densities <- unname(unlist(sapply(densities, \(x) strsplit(x, ".R"))))


#' Implemented models
#'
#' Vector of all supported models in `univariateML`.
#'
#' The currently supported models are `r paste0("[ml",paste0(univariateML_models, "]"))`
#'
#' @export
univariateML_models <- substring(densities, first = 3)


# This function populate all (currently empty) univariateML functions.

for (model in univariateML_models) {
  fun <- paste0("ml", model)
  assign(fun, decorator(fun))
}

rm(fun)

out <- univariateML_metadata[["mlunif"]]
out$names_ <- out$names
out$names <- NULL
`attributes<-`(mlunif, out)

# These imports are made to pass a CRAN note. The imports are not redundant,
# but are used only through eval-call in the code, which is invisible to the
# CRAN checks.

zzz <- \() {
  invisible(extraDistr::dbetapr)
  invisible(logitnorm::dlogitnorm)
  invisible(tibble::tibble)
  invisible(nakagami::dnaka)
  invisible(intervals::Intervals)
  invisible(sads::dzipf)
}
