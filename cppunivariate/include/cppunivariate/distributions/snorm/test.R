context("mlsnorm")

## Data generation.
set.seed(313)
small_data <- fGarch::rsnorm(100)
tiny_data <- fGarch::rsnorm(100)

## Finds errors with na and data out of bounds.
expect_error(mlsnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsnorm(small_data)),
  coef(mlsnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dsnorm(small_data, est[1], est[2], est[3], log = TRUE)),
  attr(est, "logLik")
)

## Are the fallback methods working correctly?
expect_equal(
  log(dml(small_data, est)),
  dml(small_data, est, log = TRUE)
)
expect_equal(
  log(pml(small_data, est)),
  pml(small_data, est, log.p = TRUE)
)
expect_equal(
  1 - pml(small_data, est),
  pml(small_data, est, lower.tail = FALSE)
)
expect_equal(
  qml(1:3 / 4, est),
  qml(log(1:3 / 4), est, log.p = TRUE)
)
expect_equal(
  qml(1 - 1:3 / 4, est),
  qml(1:3 / 4, est, lower.tail = FALSE)
)


## Check class.
expect_equal(attr(est, "model"), "Skew Normal")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Independent direct-sum profile oracle: grid, then refine every nominated
# local minimum. Endpoints are included explicitly, never approximated.
snorm_profile_reference <- function(x) {
  objective <- function(c) {
    d <- x - c
    (sum(d[d < 0]^2)^(1/3) + sum(d[d >= 0]^2)^(1/3))^3
  }
  grid <- seq(min(x), max(x), length.out = 2001)
  q <- vapply(grid, objective, numeric(1))
  candidates <- which(q[2:2000] <= q[1:1999] &
                      q[2:2000] <= q[3:2001]) + 1L
  modes <- c(min(x), max(x), vapply(candidates, function(i)
    optimize(objective, grid[c(i-1L, i+1L)], tol = 1e-12)$minimum,
    numeric(1)))
  values <- vapply(modes, objective, numeric(1))
  winner <- which.min(values)
  list(boundary = if (winner <= 2L) winner else 0L,
       logLik = length(x)/2 * (log(2/pi) - log(values[winner]/length(x)) - 1))
}

test_that("exact snorm profile agrees with 240 independent grid searches", {
  set.seed(28012026)
  for (i in seq_len(240)) {
    n <- c(5L, 10L, 30L, 100L, 250L, 500L)[(i-1L) %% 6L + 1L]
    x <- switch((i-1L) %% 4L + 1L, rnorm(n), rexp(n),
                fGarch::rsnorm(n, xi = 2.5), -rexp(n))
    reference <- snorm_profile_reference(x)
    if (reference$boundary != 0L) {
      expect_error(mlsnorm(x), if (reference$boundary == 1L)
        "mode at the sample minimum.*MLE does not exist" else
        "mode at the sample maximum.*MLE does not exist", info = paste("sample", i))
    } else {
      fit <- mlsnorm(x)
      expect_gte(attr(fit, "logLik"), reference$logLik - 1e-9)
      expect_equal(sum(fGarch::dsnorm(x, fit[1], fit[2], fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance = 1e-12)
    }
  }
})

test_that("two-piece conversion matches fGarch log densities", {
  for (a in c(0.2, 1, 3)) for (b in c(0.3, 1, 4)) {
    c <- 0.7
    x <- c + c(-2*a, -0.1*a, 0, 0.1*b, 2*b)
    m1 <- sqrt(2/pi)
    mu <- c + m1*(b-a)
    sd <- sqrt(a*a-a*b+b*b-m1*m1*(b-a)^2)
    expected <- log(2/(a+b)) + dnorm((x-c)/ifelse(x<c,a,b), log=TRUE)
    expect_lt(max(abs(fGarch::dsnorm(x,mu,sd,sqrt(b/a),log=TRUE)-expected)), 1e-14)
  }
})

test_that("half-normal boundary messages name the available fitter", {
  x <- c(0, 0, 0, 1, 2, 8)
  expect_error(mlsnorm(x), "Use `mlhnorm` to fit")
  expect_error(mlsnorm(-x), "Use `mlhnorm` on `-x` \\(a half-normal reflected at the sample maximum\\)")
})
