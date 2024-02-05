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
