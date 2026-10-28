context("mlged")

## Data generation.
set.seed(313)
small_data <- fGarch::rged(100, 2, 3, 4)
tiny_data <- fGarch::rged(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlged(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlged(small_data)),
  coef(mlged(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlged(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dged(small_data, est[1], est[2], est[3], log = TRUE)),
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
expect_equal(attr(est, "model"), "Generalized Error")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Decision D6: attained Laplace, uniform and constrained interior oracle.
expect_error(mlged(seq(-1, 1, length.out = 40)), "Use `mlunif`")
x <- c(seq(-.02, .02, length.out = 18), -10, 10)
f <- mlged(x)
expect_equal(unname(f["nu"]), 1)
expect_equal(attr(f, "logLik"), sum(fGarch::dged(x, mean=f[1], sd=f[2], nu=1, log=TRUE)))
a <- mean(abs(x-median(x)))
expect_equal(attr(f, "logLik"), -length(x)*(log(2*a)+1))

set.seed(812)
for (shape in c(1.5, 3)) {
  x <- fGarch::rged(500, nu=shape)
  f <- mlged(x, gradtol=1e-6, iterlim=1000)
  expect_gt(unname(f["nu"]), 1)
  objective <- function(p) {
    nu <- 1+exp(p[3])
    lambda <- exp((-2/nu*log(2)+lgamma(1/nu)-lgamma(3/nu))/2)
    z <- (x-p[1])/exp(p[2])
    factor <- -p[2]
    value <- -sum(factor+log(nu)-log(lambda)-(1+1/nu)*log(2)-
                    lgamma(1/nu)-.5*(abs(z)/lambda)^nu)
    if (is.finite(value)) value else 1e100
  }
  values <- sapply(c(1.1,1.5,2,4,8), function(nu) {
    optim(c(mean(x), log(sd(x)), log(nu-1)), objective,
          method="Nelder-Mead", control=list(maxit=10000, reltol=1e-11))$value
  })
  expect_gte(attr(f,"logLik") + 1e-4, -min(values))
}
