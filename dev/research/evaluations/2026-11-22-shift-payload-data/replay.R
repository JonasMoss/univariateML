# Run under pkgload against a package assembled and compiled by the R recipe.
pkgload::load_all(commandArgs(trailingOnly = TRUE)[[1]], quiet = TRUE)
cases <- list(
  list(fitter = mlfpareto, law = mltrgamma, x = rep(4, 8),
       held = list(min = 2, shape2 = 2, shape3 = 2)),
  list(fitter = mlpareto2, law = mlexp, x = 3:12, held = list(min = 2))
)
for (case in cases) {
  e <- tryCatch(do.call(case$fitter, c(list(x = case$x), case$held)), error = identity)
  stopifnot(inherits(e, "univariateML_limit"))
  p <- as.list(e$estimates)
  offset <- if (is.null(p$min)) case$held$min else p$min
  p$min <- NULL
  parent <- do.call(case$law, p)
  print(list(model = e$model, estimates = e$estimates, reported = e$limit_loglik,
             shifted = sum(dml(case$x - offset, parent, log = TRUE)),
             unshifted = sum(dml(case$x, parent, log = TRUE))), digits = 17)
}
