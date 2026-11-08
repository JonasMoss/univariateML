test_that("all 99 families build, evaluate, simulate and hold coefficients", {
  expect_length(univariateML_metadata, 99L)
  directory <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  for (name in names(univariateML_metadata)) {
    meta <- univariateML_metadata[[name]]
    fn <- get(name, mode = "function")
    theta <- stats::setNames(as.list(meta$simulate), meta$names)
    # A second coefficient point comes from the independently frozen golden fit.
    golden <- jsonlite::fromJSON(file.path(directory, substring(name, 3L), "golden.json"))
    for (values in list(list(), lapply(golden$estimates, as.numeric))) {
      obj <- do.call(fn, values)
      expect_true(isTRUE(attr(obj, "frozen")), info = name)
      expect_equal(attr(obj, "n"), 0L, info = name)
      args <- univariateML_dpqr_args(obj, name)
      ref <- function(kind) {
        spec <- univariateML_to_string(obj, kind)
        eval(parse(text = spec))
      }
      probabilities <- c(.2, .5, .8)
      x <- qml(probabilities, obj)
      expect_equal(qml(probabilities, obj), do.call(ref("q"), c(list(probabilities), args)), info = name)
      expect_equal(dml(x, obj), do.call(ref("d"), c(list(x), args)), info = name)
      expect_equal(pml(x, obj), do.call(ref("p"), c(list(x), args)), info = name)
      set.seed(42); actual <- rml(20, obj)
      set.seed(42); expected <- do.call(ref("r"), c(list(20), args))
      expect_equal(actual, expected, info = name)
      held <- stats::setNames(as.list(as.numeric(obj)), names(obj))
      fit <- do.call(fn, c(list(x = x), held))
      density <- get(paste0(if (startsWith(meta$density, "stats::")) "core_", "d", substring(name, 3L)), mode = "function")
      ll <- sum(do.call(density, c(list(x), held, list(log = TRUE))))
      expect_equal(as.numeric(logLik(fit)), ll, info = name)
      expect_equal(attr(logLik(fit), "df"), 0L, info = name)
      expect_equal(AIC(fit), -2*ll, info = name)
      expect_equal(BIC(fit), -2*ll, info = name)
      expect_equal(attr(fit, "fixed"), names(obj), info = name)
      adapter <- get(paste0(name, "_"), mode = "function")
      old <- tryCatch(suppressWarnings(do.call(adapter, c(list(x = x), held))), error = identity)
      if (!inherits(old, "error")) expect_equal(unname(old$logLik), ll, tolerance = 1e-8, info = paste(name, "adapter agreement"))
    }
    obj <- fn()
    for (method in list(logLik, AIC, BIC, nobs, confint, bootstrapml, summary))
      expect_error(method(obj), "built from fixed coefficients; it has no data", info = name)
    expect_error(eval(substitute(FUN(, FALSE, 1), list(FUN = fn))), "named", info = name)
  }
})

test_that("frozen models use domains and reject incompatible held data", {
  expect_s3_class(mlged(nu = .5), "univariateML")
  expect_equal(rml(5, mlbinom(size = 0)), rep(0, 5))
  expect_error(mlunif(max = NULL), "held.*max")
  expect_error(mlgamma(shape = -1), "held.*shape")
  expect_error(mlgamma(c(1, 2), shape = 2, scale = -1), "scale")
  expect_error(mlunif(c(1, 3), min = 0, max = 2), "held")
  expect_output(print(mlgamma()), "Fixed coefficients")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  expect_invisible(plot(mlgamma()))
  grDevices::dev.off()
  unlink(file)
})


test_that("fixed models accept overflow medians and reject nonfinite likelihood sums", {
  model <- mlgeom(prob=.Machine$double.xmin*.Machine$double.eps)
  expect_s3_class(model,"univariateML")
  expect_identical(qml(.5,model),Inf)
  expect_error(mlnorm(rep(1e154,4),mean=0,sd=1),
               "nonfinite log likelihood for held coefficients")
})
