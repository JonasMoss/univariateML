context("mllomax")

## Data generation.
set.seed(313)
tiny_data <- extraDistr::rlomax(10, 1, 7)
small_data <- extraDistr::rlomax(100, 10, 3)
medium_data <- extraDistr::rlomax(1000, 1 / 2, 2)
large_data <- extraDistr::rlomax(10000, 20, 13)

## Checks if the ML is correct.
mle2 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate)

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(medium_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate)

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(large_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate)


expect_equal(mle2, as.numeric(mllomax(small_data)), tolerance = 1e-3)
expect_equal(mle3, as.numeric(mllomax(medium_data)), tolerance = 1e-3)
expect_equal(mle4, as.numeric(mllomax(large_data)), tolerance = 1e-3)

## Checks warning with small iterlim.
## expect_warning(mllomax(small_data, iterlim = 1))

## Error when the MLE does not exist.
expect_error(suppressWarnings(mllomax(airquality$Wind)))
expect_error(suppressWarnings(mllomax(tiny_data, start = median(tiny_data))))

## Finds errors with na and data out of bounds.
expect_error(mllomax(c(tiny_data, NA)))
expect_error(mllomax(c(0, tiny_data)))
expect_error(
  mllomax(rep(1, 5)),
  "Use `mlexp`"
)

## Checks that na.rm works as intended.
expect_equal(
  coef(mllomax(small_data)),
  coef(mllomax(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllomax(small_data, na.rm = TRUE)
expect_equal(
  sum(extraDistr::dlomax(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Lomax")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Control arguments use the CRAN names (lambda0, reltol, iterlim).
fit_default <- mllomax(medium_data)
fit_start <- mllomax(medium_data, lambda0 = 3 * coef(fit_default)[["lambda"]], reltol = 1e-12)
expect_equal(coef(fit_start), coef(fit_default), tolerance = 1e-6)
expect_error(mllomax(medium_data, lambda0 = -1), "`lambda0` must be a positive finite number")
expect_error(mllomax(medium_data, tol = 1e-6))

test_that("lomax fitting respects extreme scale changes", {
  set.seed(1)
  x <- simulate_default("mllomax", 50)
  f <- mllomax(x)
  for (s in c(1e-300,1e300)) {
    g <- mllomax(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-6)
  }
})

test_that("Lomax moment equality can have an attained fit or exponential escape", {
  for(x in list(c(1,1,40,60),c(1,1,1,1,20,20,20,36))) {
    expect_equal(mean(x^2),2*mean(x)^2)
    f <- mllomax(x)
    profile <- function(t) {l<-exp(t); S<-sum(log1p(l*x)); k<-length(x)/S; length(x)*(log(k)+t-1)-S}
    o <- optimize(profile,c(-15,5),maximum=TRUE,tol=1e-12)
    expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-9)
  }
  expect_error(mllomax(c(1,1,4,12)),"ml exp|mlexp|ml.*exp")
})

test_that("a negative Lomax boundary score need not imply global escape", {
  x <- c(1,1,40,59)
  expect_lt(mean(x^2),2*mean(x)^2)
  f <- mllomax(x)
  profile <- function(t) {l<-exp(t); S<-sum(log1p(l*x)); k<-length(x)/S; length(x)*(log(k)+t-1)-S}
  o <- optimize(profile,c(-10,5),maximum=TRUE,tol=1e-12)
  expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-9)
  expect_gt(attr(f,"logLik"),-length(x)*(log(mean(x))+1))
})

test_that("unresolvable Lomax starting hints fall back to the moment start", {
  x<-c(1,1,2,3,100)
  base<-mllomax(x)
  for(start in c(1e-300,1e300)) {
    f<-mllomax(x,lambda0=start)
    expect_equal(as.numeric(f),as.numeric(base),tolerance=1e-10)
  }
})
