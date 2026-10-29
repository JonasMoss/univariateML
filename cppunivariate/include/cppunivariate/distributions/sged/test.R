context("mlsged")

## Data generation.
set.seed(313)
small_data <- fGarch::rsged(100, 2, 3, 4)
tiny_data <- fGarch::rsged(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlsged(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsged(small_data)),
  coef(mlsged(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsged(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dsged(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Generalized Error")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Decision D6: attained Laplace, uniform and constrained interior oracle.
expect_error(mlsged(seq(-1, 1, length.out = 40)), "Use `mlunif`")
x <- c(seq(-.02, .02, length.out = 18), -10, 10)
f <- mlsged(x)
expect_equal(unname(f["nu"]), 1)
expect_equal(attr(f, "logLik"), sum(fGarch::dsged(x, mean=f[1], sd=f[2], nu=1, xi=f[4], log=TRUE)))
# Exact fGarch mapping of arbitrary two-piece Laplace parameters.
a <- .7; b <- 2; c <- -.3; grid <- seq(-4, 4, length.out=101)
expected <- -log(a+b) - ifelse(grid<c, (c-grid)/a, (grid-c)/b)
expect_equal(fGarch::dsged(grid, c+b-a, sqrt(a*a+b*b), 1, sqrt(b/a), log=TRUE), expected)
expect_error(mlsged(-log(((0:99)+.5)/100)), "sample minimum")
expect_error(mlsged(log(((0:99)+.5)/100)), "sample maximum")

set.seed(812)
for (shape in c(1.5, 3)) {
  x <- fGarch::rged(500, nu=shape)
  f <- mlsged(x, gradtol=1e-6, iterlim=1000)
  expect_gt(unname(f["nu"]), 1)
  objective <- function(p) {
    nu <- 1+exp(p[3])
    lambda <- exp((-2/nu*log(2)+lgamma(1/nu)-lgamma(3/nu))/2)
    z <- (x-p[1])/exp(p[2])
    factor <- -p[2]
    xi <- exp(p[4])
    m1 <- exp(lgamma(2/nu)-(lgamma(1/nu)+lgamma(3/nu))/2)
    su <- sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    z <- z*su+m1*(xi-1/xi)
    z <- z/ifelse(z>0,xi,1/xi)
    factor <- factor+log(su)+log(2/(xi+1/xi))
    value <- -sum(factor+log(nu)-log(lambda)-(1+1/nu)*log(2)-
                    lgamma(1/nu)-.5*(abs(z)/lambda)^nu)
    if (is.finite(value)) value else 1e100
  }
  values <- sapply(c(1.1,1.5,2,4,8), function(nu) {
    optim(c(mean(x), log(sd(x)), log(nu-1), 0), objective,
          method="Nelder-Mead", control=list(maxit=10000, reltol=1e-11))$value
  })
  expect_gte(attr(f,"logLik") + 1e-4, -min(values))
}

test_that("sged scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- simulate_default("mlsged",50)
  base <- tryCatch(mlsged(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlsged(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("sged large-sample fitting preserves likelihood under replication", {
  p<-((1:1000)-.5)/1000
  x<-fGarch::qsged(p,nu=3,xi=2)
  base<-mlsged(x)
  f<-mlsged(rep(x,100))
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})
