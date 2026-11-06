test_that("normal observed covariance and held coefficients use natural coordinates", {
  x <- c(-2, -1, 0, 0.5, 1, 2, 3)
  fit <- mlnorm(x)
  v <- vcov(fit, type = "observed")
  expect_equal(unname(diag(v)), c(unname(fit[2])^2 / length(x), unname(fit[2])^2 / (2 * length(x))), tolerance = 1e-8)
  expect_identical(dimnames(v), list(names(fit), names(fit)))
  expect_identical(attr(v, "type"), "observed")
  held <- mlnorm(x, mean = 0)
  expect_identical(dim(vcov(held)), c(1L, 1L))
  expect_identical(attr(vcov(held), "held"), "mean")
})

test_that("unavailable families and frozen models explain failure", {
  expect_error(vcov(mlunif(c(0, 1, 2))), "unavailable")
  expect_error(vcov(mlnorm(mean = 0, sd = 1)), "observations")
})

vcov_golden <- function(id) {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if (!nzchar(root)) root <- testthat::test_path("fixtures")
  as.numeric(jsonlite::fromJSON(file.path(root, id, "golden.json"))$data)
}

# Independent finite differences of each R log-density, using natural units.
vcov_fd <- function(fit, x, log_density) {
  theta <- as.numeric(fit)
  free <- attr(fit, "inference_free")
  t <- theta[free]
  h <- 1e-4 * pmax(1, abs(t))
  evaluate <- function(z) {
    p <- theta
    p[free] <- z
    log_density(x, p)
  }
  n <- length(x); k <- length(t)
  scores <- matrix(0, n, k)
  information <- matrix(0, k, k)
  base <- evaluate(t)
  for (i in seq_len(k)) {
    ei <- rep(0, k); ei[i] <- h[i]
    plus <- evaluate(t + ei); minus <- evaluate(t - ei)
    scores[, i] <- (plus - minus) / (2*h[i])
    information[i, i] <- -sum(plus - 2*base + minus) / h[i]^2
    if (i > 1) for (j in seq_len(i-1)) {
      ej <- rep(0, k); ej[j] <- h[j]
      information[i,j] <- information[j,i] <- -sum(evaluate(t+ei+ej) - evaluate(t+ei-ej) -
        evaluate(t-ei+ej) + evaluate(t-ei-ej)) / (4*h[i]*h[j])
    }
  }
  inverse <- solve(information)
  list(observed = inverse, sandwich = inverse %*% crossprod(scores) %*% inverse)
}

test_that("golden fits agree with independent R finite differences", {
  cases <- list(
    list("norm", mlnorm, function(x,p) dnorm(x,p[1],p[2],log=TRUE), list()),
    list("gamma", mlgamma, function(x,p) dgamma(x,p[1],scale=p[2],log=TRUE), list()),
    list("beta", mlbeta, function(x,p) dbeta(x,p[1],p[2],log=TRUE), list()),
    list("pois", mlpois, function(x,p) dpois(x,p[1],log=TRUE), list()),
    list("binom", mlbinom, function(x,p) dbinom(x,p[1],p[2],log=TRUE), list(size=10)),
    list("norm", mlnorm, function(x,p) dnorm(x,p[1],p[2],log=TRUE), list(mean=0)),
    list("betapr", mlbetapr, function(x,p) dbeta(x/(x+p[3]),p[1],p[2],log=TRUE) + log(p[3])-2*log(x+p[3]), list())
  )
  for (case in cases) {
    x <- vcov_golden(case[[1]])
    if (case[[1]] == "binom") case[[4]]$size <- max(x) + 2
    fit <- do.call(case[[2]], c(list(x), case[[4]]))
    expected <- vcov_fd(fit, x, case[[3]])
    for (type in c("observed", "sandwich")) {
      actual <- vcov(fit, type)
      expect_equal(unname(as.vector(actual)), as.vector(expected[[type]]), tolerance=8e-4,
                   info=paste(case[[1]],type))
      cpp <- univariateML:::univariateML_inference_cpp(x, case[[1]], as.numeric(fit),
                                                      unname(attr(fit,"inference_free")), type == "sandwich")
      expect_equal(unname(as.vector(actual)), as.vector(cpp$matrix), tolerance=1e-12)
    }
  }
})

test_that("pilot analytic observed information agrees with closed forms", {
  x <- vcov_golden("gamma"); g <- mlgamma(x); a <- g[[1]]; b <- g[[2]]; n <- length(x)
  H <- matrix(c(n*trigamma(a), n/b, n/b, sum(2*x/b-a)/b^2), 2)
  expect_equal(as.vector(vcov(g,"observed")), as.vector(solve(H)), tolerance=1e-10)
  x <- vcov_golden("beta"); f <- mlbeta(x); a <- f[[1]]; b <- f[[2]]; n <- length(x)
  H <- n*matrix(c(trigamma(a)-trigamma(a+b), -trigamma(a+b), -trigamma(a+b), trigamma(b)-trigamma(a+b)), 2)
  expect_equal(as.vector(vcov(f,"observed")), as.vector(solve(H)), tolerance=1e-10)
  x <- vcov_golden("pois"); f <- mlpois(x)
  expect_equal(as.numeric(vcov(f,"observed")), as.numeric(f)^2/sum(x), tolerance=1e-10)
})

test_that("conditioning and boundary diagnostics are exposed", {
  x <- vcov_golden("gamma") * 3000
  fit <- mlgamma(x)
  expect_warning(v <- vcov(fit), "poorly conditioned")
  expect_gt(attr(v,"condition"), 1e6)
  bad <- mlnorm(vcov_golden("norm")); bad[2] <- 0
  expect_error(vcov(bad), "boundary or limit")
  attr(bad,"inference_data")[1] <- Inf; bad[2] <- 1
  expect_error(vcov(bad), "invalid observation|nonfinite")
  expect_identical(attr(mlnorm(c(NA, 1, 2, 3), na.rm=TRUE),"inference_data"), c(1,2,3))
})

test_that("nonregular submodels and information failures stop explicitly", {
  x <- vcov_golden("binom")
  fit <- mlbinom(x, size=max(x)+2)
  attr(fit, "inference_free")[1] <- TRUE
  expect_error(vcov(fit), "coefficient subset.*size")
  ill <- mlgamma(vcov_golden("gamma") * 1e5)
  expect_error(vcov(ill), "ill-conditioned information")
  indefinite <- mlnorm(vcov_golden("norm"))
  indefinite[2] <- as.numeric(indefinite[2]) * 100
  expect_error(vcov(indefinite), "indefinite information")
  old <- mlnorm(vcov_golden("norm")); attr(old, "inference_free") <- NULL
  expect_error(vcov(old), "free mask")
})

# Exercise the public vcov route at exact parameter regimes without asking a
# fitter to land on a measure-zero singular set. Observations and free masks
# are explicit; these tests establish dispatch, not estimator calibration.
vcov_regime_object <- function(id, theta, free, x) {
  object <- mlnorm(c(-2, -1, 0, 1, 2))
  attributes(theta) <- attributes(object)
  names(theta) <- names(free)
  attr(theta, "model") <- univariateML:::univariateML_metadata[[paste0("ml", id)]]$model
  attr(theta, "inference_free") <- free
  attr(theta, "inference_data") <- x
  theta
}

test_that("structural singularities stop before a numerical Hessian solve", {
  cases <- list(
    list("sn", c(0, 1, 0), c(xi=TRUE, omega=FALSE, alpha=TRUE), c(-1, 0, 1)),
    list("bnbinom", c(2, 3, 2), c(size=TRUE, alpha=FALSE, beta=TRUE), c(0, 1, 2)),
    list("genbeta", c(2, 1, 3, 1), c(shape1=TRUE, shape2=FALSE, shape3=TRUE, scale=FALSE), c(.2,.4,.8))
  )
  for (case in cases) for (type in c("observed", "sandwich")) {
    fit <- do.call(vcov_regime_object, case)
    expect_error(vcov(fit, type), "singular information", info=case[[1]])
  }
})

test_that("ordinary cusp Hessian failures affect both covariance types", {
  cases <- list(
    list("laplace", c(0, 1), c(mu=TRUE, sigma=TRUE)),
    list("ged", c(0, 1, 1), c(mean=TRUE, sd=TRUE, nu=FALSE)),
    list("ged", c(0, 1, .75), c(mean=TRUE, sd=TRUE, nu=FALSE)),
    list("sged", c(0, 1, 1, 1), c(mean=TRUE, sd=TRUE, nu=FALSE, xi=FALSE)),
    list("sged", c(0, 1, .75, 1), c(mean=TRUE, sd=TRUE, nu=FALSE, xi=FALSE))
  )
  for (case in cases) for (type in c("observed", "sandwich")) {
    fit <- do.call(vcov_regime_object, c(case, list(x=c(-2,-1,.2,1,2))))
    expect_error(vcov(fit, type), "regime unavailable", info=case[[1]])
  }
})
