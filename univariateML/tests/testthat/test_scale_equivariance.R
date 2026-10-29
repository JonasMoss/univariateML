# Coefficients are pulled back to the original units before comparison.
# The exclusions describe models, not outcomes of particular random draws.
scale_exclusions <- c(
  mlbeta = "Fixed support (0, 1) is not closed under scaling.",
  mlkumar = "Fixed support (0, 1) is not closed under scaling.",
  mllogitnorm = "Fixed support (0, 1) is not closed under scaling.",
  mllgamma = "Fixed support (1, Inf); scaling x is not scaling log(x).",
  mlnsbeta = "Requires both support endpoints; they must be transformed too.",
  mlgenbeta = "Requires the upper endpoint; it must be transformed too.",
  mlpareto2 = "Requires a threshold; it must be transformed too.",
  mlpareto3 = "Requires a threshold; it must be transformed too.",
  mlpareto4 = "Requires a threshold; it must be transformed too.",
  mlfpareto = "Requires a threshold; it must be transformed too."
)

# Full affine families. Folded normal and the zero-threshold positive
# families permit scaling only. The golden tnorm fit uses infinite bounds.
affine_models <- c("mlbhatt", "mlcauchy", "mlemg", "mlfrechet", "mlged",
                   "mlgev", "mlgumbel", "mlhnorm", "mlhuber", "mllaplace",
                   "mllogis", "mllst", "mlnorm", "mlsged", "mlslash",
                   "mlsn", "mlsnorm", "mlsstd", "mlstd", "mltlambda",
                   "mltnorm", "mltriang", "mlunif")

pull_back_scale_fit <- function(fit, model, a, b) {
  p <- setNames(as.numeric(fit), names(fit))
  loc <- intersect(names(p), c("mean", "mu", "xi", "location"))
  if (model != "mlsn") loc <- setdiff(loc, "xi")
  if (model %in% c("mltriang", "mlunif")) loc <- names(p)
  # All remaining named dimensional parameters have the usual scale law.
  scl <- intersect(names(p), c("sd", "sigma", "scale", "beta", "omega"))
  rate <- intersect(names(p), c("rate", "lambda"))
  # Explicit exceptions to name-based rules (shape names and log coordinates).
  if (model %in% c("mlfrechet", "mltlambda")) rate <- character()
  if (model == "mlfatigue") loc <- character() # held mu = 0
  if (model == "mlgpd") loc <- character()     # held threshold = 0
  if (model == "mlpower") { scl <- "alpha"; rate <- character() }
  if (model == "mlpareto") scl <- "b"
  if (model == "mlgompertz") rate <- c("a", "b")
  if (model == "mlsgomp") rate <- "b"
  if (model == "mlinvgauss") { scl <- "shape"; rate <- character() }
  if (model == "mlbhatt") scl <- c(scl, "a")
  if (model == "mlnaka") p["scale"] <- p["scale"] / b^2
  else p[scl] <- p[scl] / b
  p[rate] <- p[rate] * b
  p[loc] <- (p[loc] - a) / b
  if (model == "mllnorm") p["meanlog"] <- p["meanlog"] - log(b)
  unname(p)
}

# Invariant coordinates remove the unit-dependent FD error. For FD/profile
# searches allow 2e-5 in coefficients (default scores 1.22e-4, profile ties
# 1e-8 per datum); smooth scalar fits allow 2e-7. Likelihood is checked in
# absolute units per observation, independently of its additive constant.
scale_search_models <- c("mlbetapr", "mlburr", "mlemg", "mlgb2", "mlged",
                         "mlht", "mlhuber", "mlinvburr", "mlinvparalogis", "mlparalogis",
                         "mlsged", "mlsn", "mlsstd", "mlstd", "mlbhatt")

for (id in univariateML_models) {
  model <- paste0("ml", id)
  if (univariateML:::univariateML_metadata[[model]]$support$type != "R") next
  if (model %in% names(scale_exclusions)) next
  test_that(paste(model, "is equivariant in its dimensional parameters"), {
    set.seed(7)
    # The nested profile searches dominate runtime; 24 observations still
    # exercise every parameter and all transformation decisions.
    n <- if (model %in% c("mlemg", "mlbhatt", "mltlambda")) 24 else 80
    x <- simulate_default(model, n)
    args <- golden_fit_args(model)
    fit <- function(x, a=0, b=1) {
      transformed <- args
      for (p in intersect(names(args), c("mu","a","b")))
        if (!is.null(args[[p]])) transformed[[p]] <- a + b*args[[p]]
      tryCatch(do.call(model, c(list(x), transformed)),
               error = function(e) conditionMessage(e))
    }
    base <- fit(x)
    for (b in c(1e-4, 1e4)) {
      # Shift in sample units avoids loss of precision in tiny observations.
      a <- if (model %in% affine_models) 3 * b else 0
      got <- fit(a + b * x, a, b)
      if (is.character(base)) {
        expect_identical(got, base) # includes escape/error decisions
      } else {
        expect_false(is.character(got))
        if (is.character(got)) next
        tol <- if (model %in% scale_search_models) 2e-5 else 2e-7
        expect_equal(pull_back_scale_fit(got, model, a, b),
                     as.numeric(base), tolerance = tol)
        delta <- attr(got, "logLik") + length(x) * log(b) - attr(base, "logLik")
        expect_lt(abs(delta) / length(x), 2e-8)
      }
    }
  })
}

# The original failures: small returns and a false df-two escape.
test_that("Student fits on DAX returns and small skew-t data are stable", {
  x <- diff(log(EuStockMarkets[, "DAX"]))
  f <- mlstd(x)
  # Optimum nu = 4.19447 (independent optim); the FD-BFGS stopping point
  # varies by ~1e-4 across machines on this flat profile. The defect
  # returned the start nu = 6.
  expect_equal(unname(f["nu"]), 4.19447, tolerance = 1e-4)
  expect_equal(as.numeric(logLik(f)), 5983.32, tolerance = 2e-6)
  for (model in c("mlstd", "mlsstd")) {
    fit <- get(model, mode = "function")
    base <- fit(x)
    for (b in c(1e-6, 1e6)) {
      got <- fit(b * x)
      expect_equal(pull_back_scale_fit(got, model, 0, b), as.numeric(base),
                   tolerance = 2e-5)
      expect_lt(abs(attr(got, "logLik") + length(x) * log(b) -
                    attr(base, "logLik")), 1e-5)
    }
  }
  set.seed(7)
  x <- simulate_default("mlsstd", 400)
  expect_equal(pull_back_scale_fit(mlsstd(1e-3*x), "mlsstd", 0, 1e-3),
               as.numeric(mlsstd(x)), tolerance = 2e-5)
})

test_that("required endpoints transform with the observations", {
  for (model in c("mlgenbeta", "mlnsbeta", "mlpareto2", "mlpareto3",
                  "mlpareto4", "mlfpareto")) {
    set.seed(7)
    x <- simulate_default(model, 40)
    args <- switch(model, mlgenbeta = list(scale = 1),
                   mlnsbeta = list(min = 0, max = 1), list(min = 0))
    fit <- function(b) tryCatch(do.call(model, c(list(b*x), lapply(args, function(v) b*v))),
                                error = function(e) conditionMessage(e))
    base <- fit(1)
    for (b in c(1e-4, 1e4)) {
      got <- fit(b)
      if (is.character(base)) expect_identical(got, base)
      else {
        expect_false(is.character(got))
        if (is.character(got)) next
        p <- setNames(as.numeric(got), names(got))
        dim <- intersect(names(p), c("scale", "min", "max"))
        p[dim] <- p[dim] / b
        expect_equal(unname(p), as.numeric(base), tolerance = 2e-5)
        expect_lt(abs(attr(got, "logLik") + length(x)*log(b) -
                      attr(base, "logLik")) / length(x), 2e-8)
      }
    }
  }
})

test_that("Gompertz fits the original large-scale failure sample", {
  set.seed(7)
  x <- simulate_default("mlgompertz", 400)
  expect_equal(pull_back_scale_fit(mlgompertz(1e3*x), "mlgompertz", 0, 1e3),
               as.numeric(mlgompertz(x)), tolerance = 2e-7)
})
