test_that("restriction warnings name README bounds and reasons", {
  metadata <- getFromNamespace("univariateML_metadata", "univariateML")
  construct <- getFromNamespace("univariateML_construct", "univariateML")
  for (id in c("lst", "frechet", "ht", "gev", "gpd", "tlambda", "ged", "sged")) {
    name <- paste0("ml", id)
    m <- metadata[[name]]
    for (p in m$params) {
      domains <- p$fit_domain
      if (is.null(domains)) next
      if (!is.null(domains$restriction)) domains <- list(domains)
      for (d in domains) {
        side <- intersect(c("lower", "upper"), names(d))
        restricted <- setNames(side, p$name)
        condition <- NULL
        fit <- withCallingHandlers(
          construct(m$simulate, name, list(logLik = -1, n = 10), restricted = restricted),
          univariateML_restricted = function(w) {
            condition <<- w
            invokeRestart("muffleWarning")
          })
        expect_s3_class(condition, "univariateML_restricted")
        expect_match(conditionMessage(condition), p$name, fixed = TRUE)
        expect_match(conditionMessage(condition), format(d[[side]]), fixed = TRUE)
        expect_match(conditionMessage(condition), d$restriction$reason, fixed = TRUE)
        expect_identical(attr(fit, "restricted"), restricted)
      }
    }
  }
})

test_that("adapters flag estimated bounds and suppress held and exempt bounds", {
  x <- c(-100, -2, -.01, 0, .01, 2, 100)
  for (fn in list(mllst, mlged, mlsged)) {
    f <- NULL
    expect_warning(f <- fn(x, iterlim = 1000), class = "univariateML_restricted")
    expect_identical(unname(attr(f, "restricted")), "lower")
  }
  expect_warning(g <- mlgpd(1:4, mu = 0), class = "univariateML_restricted")
  expect_identical(attr(g, "restricted"), c(xi = "lower"))
  expect_equal(unname(g["sigma"]), 4)
  expect_equal(as.numeric(logLik(g)), -4 * log(4))
  expect_warning(v <- mlgev(0:4), class = "univariateML_restricted")
  expect_identical(attr(v, "restricted"), c(xi = "lower"))
  expect_warning(t <- mltlambda(-2:2), class = "univariateML_restricted")
  expect_identical(attr(t, "restricted"), c(lambda = "upper"))
  expect_warning(f <- mlfrechet(c(.01,.02,.1,1,10,100,1000), mu = NULL, sigma = 1),
                 class = "univariateML_restricted")
  expect_identical(attr(f, "restricted"), c(lambda = "lower"))
  expect_warning(upper <- mlgev(qgev(p = ((1:400) - 0.5)/400, mu = 0, sigma = 1, xi = 2)),
                 class = "univariateML_restricted")
  expect_identical(attr(upper, "restricted"), c(xi = "upper"))
  expect_warning(lower <- mltlambda(qtlambda(p = ((1:100) - 0.5)/100, mu = 0, sigma = 1, lambda = -2)),
                 class = "univariateML_restricted")
  expect_identical(attr(lower, "restricted"), c(lambda = "lower"))
  expect_warning(partial <- mllst(x, sigma = 1), class = "univariateML_restricted")
  expect_identical(attr(partial, "restricted"), c(df = "lower"))
  expect_warning(partial <- mlged(x, mean = 0, sd = 1), class = "univariateML_restricted")
  expect_identical(attr(partial, "restricted"), c(nu = "lower"))
  expect_no_warning(mlgpd(1:4, mu = 0, xi = -1))
  expect_no_warning(mlgev(0:4, xi = -1))
  expect_no_warning(mltlambda(-2:2, lambda = 1))
  expect_no_warning(mlged(x, mean = 0, sd = 1, nu = 1))
  expect_no_warning(mlsged(x, mean = 0, sd = 1, nu = 1, xi = 1))
  expect_no_warning(mllst(x, mu = 0, sigma = 1, df = .5))
  expect_no_warning(mlht(c(.01,.1,1,10,100)))
})

test_that("failed BFGS paths do not return starts as fits", {
  x <- c(-3,-1,-.4,.1,.3,.7,1,2,5)
  expect_error(mllst(x, iterlim = 1), "score|converge")
  normal <- qnorm(((1:100)-.5)/100)
  expect_error(mlged(normal, iterlim = 1), "converge")
  expect_error(mlsged(normal, iterlim = 1), "score|converge")
})


test_that("C++ restriction constants match README fit domains", {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "repository metadata unavailable")
  metadata <- getFromNamespace("univariateML_metadata", "univariateML")
  for (id in c("lst", "frechet", "ht", "gev", "gpd", "tlambda", "ged", "sged")) {
    header <- readLines(file.path(root, id, "mle.hpp"))
    constants <- grep("inline constexpr double k.*(Lower|Upper) =", header, value = TRUE)
    constants <- as.numeric(sub(".* = ([0-9.-]+);", "\\1", constants))
    bounds <- unlist(lapply(metadata[[paste0("ml", id)]]$params, function(p) {
      domains <- p$fit_domain
      if (is.null(domains)) return(NULL)
      if (!is.null(domains$restriction)) domains <- list(domains)
      unlist(lapply(domains, function(d) unlist(d[intersect(c("lower", "upper"), names(d))])))
    }), use.names = FALSE)
    expect_equal(constants, bounds, info = id)
  }
})

test_that("GPD accepts its closed threshold and diagnoses threshold collapse", {
  expect_no_warning(f <- mlgpd(c(0,1,2,4), mu = 0, xi = -1))
  expect_equal(unname(f["sigma"]), 4)
  expect_equal(as.numeric(logLik(f)), -4 * log(4))
  expect_error(mlgpd(c(0,1,2,4), mu = 0), "scale collapse")
  expect_no_warning(mlgpd(1:4, mu = 0, sigma = 10, xi = -2))
  expect_no_warning(mlgev(0:4, mu = 10, sigma = 5, xi = -2))
  expect_no_warning(mltlambda(-2:2, mu = 0, sigma = 10, lambda = 3))
})
