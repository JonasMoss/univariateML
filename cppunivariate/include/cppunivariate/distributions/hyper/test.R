test_that("hyper requires its design and preserves fixed constraints", {
  expect_error(mlhyper(c(1, 2), N = 20), "requires `k`")
  expect_error(mlhyper(c(1, 2), k = 4), "estimating both is not supported")
  expect_error(mlhyper(0, k = 2, m = 3), "point mass at zero")
  expect_error(mlhyper(1, k = 2, N = 2^53), "2\\^53")
  expect_error(mlhyper(1.5, k = 2, N = 10), "Non-integral")
  expect_error(mlhyper(1, k = 2, N = 10, m = NA_real_), "not NA")
  x <- c(1, 2, 3, 2)
  a <- mlhyper(x, N = 20, k = 5)
  expect_equal(mlhyper(x, FALSE, 5, 20)[1:3], a[1:3])
  expect_identical(names(a), c("m", "n", "k"))
  expect_equal(sum(a[c("m", "n")]), 20)
  expect_equal(attr(a, "fixed"), c("N", "k"))
  expect_equal(attr(logLik(a), "df"), 1)
  b <- mlhyper(x, m = 8, k = 5)
  expect_equal(attr(b, "fixed"), c("m", "k"))
  expect_equal(attr(logLik(b), "df"), 1)
  c <- mlhyper(x, N = 20, m = 8, k = 5)
  expect_equal(attr(c, "fixed"), c("m", "n", "k"))
  expect_equal(attr(logLik(c), "df"), 0)
  for (fit in list(a, b, c)) {
    args <- as.list(unclass(fit)[c("m", "n", "k")])
    expect_equal(dml(x, fit, log = TRUE), do.call(dhyper, c(list(x), args, log = TRUE)))
    expect_equal(pml(x, fit), do.call(phyper, c(list(x), args)))
    expect_equal(qml(c(.1, .5, .9), fit), do.call(qhyper, c(list(c(.1, .5, .9)), args)))
    set.seed(42); actual <- rml(10, fit)
    set.seed(42); expected <- do.call(rhyper, c(list(10), args))
    expect_equal(actual, expected)
  }
})

test_that("hyper warns deterministically for ties and unidentified designs", {
  expect_warning(a <- mlhyper(5, k = 10, N = 19), "tied")
  expect_equal(unname(a["m"]), 9)
  expect_warning(b <- mlhyper(5, k = 10, m = 10), "tied")
  expect_equal(sum(b[c("m", "n")]), 19)
  expect_warning(z <- mlhyper(c(0, 0), k = 0, N = 7), "every `m`")
  expect_equal(unname(z["m"]), 0)
  expect_warning(mlhyper(0, k = 3, m = 0), "Every `N`")
  fit <- mlhyper(c(0, 1, 1), k = 1, N = 10)
  grid <- 0:10
  ll <- vapply(grid, function(m) sum(dhyper(c(0, 1, 1), m, 10-m, 1, log=TRUE)), 0)
  expect_equal(unname(fit["m"]), grid[which.max(ll)])
})

test_that("hyper exact searches agree with independent finite enumeration", {
  set.seed(2901)
  for (i in seq_len(60)) {
    N <- sample(8:30, 1); m <- sample(2:(N-2), 1); k <- sample(2:(N-1), 1)
    x <- rhyper(6, m, N-m, k)
    a <- suppressWarnings(mlhyper(x, k=k, N=N))
    grid <- max(x):(N-k+min(x))
    ll <- vapply(grid, function(m) sum(dhyper(x,m,N-m,k,log=TRUE)), 0)
    expect_equal(attr(a,"logLik"), max(ll), tolerance=1e-12)
    if (sum(x) == 0) next
    b <- suppressWarnings(mlhyper(x, k=k, m=m))
    # Derived bound beyond which every adjacent ratio is below one.
    top <- ceiling(k*(m+k)/mean(x))+2
    grid <- (m+k-min(x)):top
    ll <- vapply(grid, function(N) sum(dhyper(x,m,N-m,k,log=TRUE)), 0)
    expect_equal(attr(b,"logLik"), max(ll), tolerance=1e-12)
  }
})
