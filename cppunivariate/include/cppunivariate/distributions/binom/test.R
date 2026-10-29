context("mlbinom")

## Expect error when fed with overdispersed data
expect_error(
  mlbinom(seq(20), size = NULL),
  "The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument\\."
)

## Size argument works
expect_equal(unname(mlbinom(seq(20), size = 20))[1], 20)

## Error when size > max
expect_error(mlbinom(seq(21), size = 20))

## Underdispersed / invalid inputs
expect_error(
  mlbinom(rep(1, 5), size = 0),
  "`size` is smaller than `max\\(x\\)`\\."
)

test_that("binomial probability boundaries have log likelihood zero", {
  for (fit in list(mlbinom(c(5L, 5L, 5L), size = NULL),
                   mlbinom(c(5L, 5L, 5L), size = 5))) {
    expect_equal(as.numeric(fit), c(5, 1))
    expect_equal(attr(fit, "logLik"), 0)
    expect_equal(attr(fit, "logLik"), sum(dbinom(rep(5, 3), 5, 1, log = TRUE)))
  }
  for (size in c(0, 5)) {
    fit <- mlbinom(c(0L, 0L, 0L), size = size)
    expect_equal(as.numeric(fit), c(size, 0))
    expect_equal(attr(fit, "logLik"), 0)
  }
  fit <- mlbinom(c(0L, 0L, 0L), size = NULL)
  expect_equal(as.numeric(fit), c(0, 0))
  expect_equal(attr(fit, "logLik"), 0)
})

test_that("other discrete zero-data fits have the point-mass likelihood", {
  expect_equal(attr(mlpois(c(0, 0, 0)), "logLik"), 0)
  expect_equal(attr(mlgeom(c(0, 0, 0)), "logLik"), 0)
  expect_warning(fit <- mlnbinom(c(0, 0, 0)), "All observations are 0")
  expect_equal(as.numeric(fit), c(1, 1))
  expect_equal(attr(fit, "logLik"), 0)
  expect_warning(fixed <- mlnbinom(c(0, 0, 0), size = 3), "All observations are 0")
  expect_equal(attr(fixed, "logLik"), 0)
})

test_that("a held size must be a nonnegative integer", {
  # NA, NaN and Inf used to be read as "estimate size" while the fit still
  # reported size as held; fractional sizes gave a non-binomial likelihood.
  for (size in list(NA_real_, NaN, Inf, -1, 9.5)) {
    expect_error(mlbinom(c(1, 2, 3), size = size), "nonnegative integer")
  }
})

test_that("mean equal to variance escapes to the Poisson", {
  # c(0, 2): mean 1, population variance 1.
  expect_error(mlbinom(c(0, 2), size = NULL), "Use `mlpois`")
})

test_that("huge held binomial sizes have stable likelihoods", {
  x <- c(0,1,2,5,9)
  for (s in c(1e9, 2^31, 1e300)) {
    f <- mlbinom(x,size=s)
    expect_equal(attr(f,"logLik"),sum(dbinom(x,size=s,prob=mean(x)/s,log=TRUE)), tolerance=1e-10)
  }
})

test_that("free binomial size agrees with exhaustive integer profiles", {
  for (x in list(c(2,6,6),c(1,2,3,4,5),c(1,3,3,4))) {
    f <- mlbinom(x)
    sizes <- max(x):500
    ll <- vapply(sizes,function(k) sum(dbinom(x,k,mean(x)/k,log=TRUE)),0.0)
    expect_equal(unname(f["size"]),sizes[which.max(ll)])
    expect_equal(as.numeric(logLik(f)),max(ll),tolerance=1e-10)
  }
  expect_equal(unname(mlbinom(c(2,6,6))["size"]),13)
})
