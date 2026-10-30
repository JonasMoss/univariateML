context("mlunif")

## Data generation.
set.seed(313)
tiny_data <- stats::runif(10, 1, 7)
small_data <- stats::runif(100, 3, 9)
medium_data <- stats::runif(1000, 1 / 2, 2)
large_data <- stats::runif(10000, 13, 20)

## Checks logLiks.
expect_equal(
  sum(dunif(tiny_data,
    min = min(tiny_data),
    max = max(tiny_data), log = TRUE
  )),
  attr(mlunif(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(dunif(small_data,
    min = min(small_data),
    max = max(small_data), log = TRUE
  )),
  attr(mlunif(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(dunif(medium_data,
    min = min(medium_data),
    max = max(medium_data), log = TRUE
  )),
  attr(mlunif(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(dunif(large_data,
    min = min(large_data),
    max = max(large_data), log = TRUE
  )),
  attr(mlunif(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlunif(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlunif(small_data)),
  coef(mlunif(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlunif(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Uniform")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained unif MLE", {
  expect_error(mlunif(rep(2, 5)), "does not exist")
  expect_error(mlunif(2), "does not exist")
})

test_that("P2 uniform held endpoints are exact constrained maxima", {
  set.seed(8130)
  for (x in list(runif(20),runif(100),c(.2,.2,.8),c(.2,.8),rep(.4,5),.4)) {
    values <- c(min=-1,max=2)
    for (mask in 1:3) {
      held <- names(values)[as.logical(intToBits(mask)[1:2])]
      fit <- do.call(mlunif,c(list(x=x),as.list(values[held])))
      expect_identical(attr(fit,"fixed"),held)
      expect_identical(unname(coef(fit)[held]),unname(values[held]))
      expect_equal(attr(logLik(fit),"df"),2L-length(held))
      expect_equal(as.numeric(logLik(fit)),sum(dunif(x,fit[1],fit[2],log=TRUE)))
      free <- setdiff(names(values),held)
      if (length(free)) {
        # Independent density optimization over endpoint slack, including 0.
        objective <- function(slack) {
          p <- values
          p[free] <- if (free=="min") min(x)-slack else max(x)+slack
          -sum(dunif(x,p[1],p[2],log=TRUE))
        }
        refs <- vapply(c(.01,1,10),function(start)
          nlminb(start,objective,lower=0)$objective,0.0)
        expect_equal(as.numeric(logLik(fit)),-min(refs),tolerance=1e-8)
      }
    }
  }
  expect_error(mlunif(c(0,1),min=.1),"outside")
  expect_error(mlunif(c(0,1),max=.9),"outside")
  expect_error(mlunif(.4,min=.4),"does not exist")
  expect_error(mlunif(.4,max=.4),"does not exist")
  expect_error(mlunif(.4,min=1,max=0),"min < max")
  for (p in c("min","max")) for (bad in c(NA_real_,Inf,-Inf))
    expect_error(do.call(mlunif,c(list(x=c(0,1)),setNames(list(bad),p))),"finite")
})
