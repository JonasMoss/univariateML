context("mllogitnorm")

## Data generation.
set.seed(313)
small_data <- plogis(rnorm(100, 2, 3))
tiny_data <- plogis(rnorm(10, 0, 1))

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllogitnorm(small_data)),
  coef(mllogitnorm(c(small_data, NA), na.rm = TRUE))
)

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(0, tiny_data)))
expect_error(mllogitnorm(c(tiny_data, 1)))

## Is the log-likelihood correct?
est <- mllogitnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dlogitnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "LogitNormal")
expect_equal(class(est), c("univariateML"))


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained logitnorm MLE", {
  expect_error(mllogitnorm(rep(.4, 5)), "does not exist")
  expect_error(mllogitnorm(.4), "does not exist")
})

 test_that("logitnorm owned dpqr obeys transformations and R conventions", {
  x <- plogis(seq(-6, 6, length.out = 31))
  a <- -.3; b <- 2.1
  expect_equal(dlogitnorm(x,a,b), dnorm(qlogis(x),a,b)/(x*(1-x)), tolerance=1e-12)
  expect_equal(integrate(function(x) dlogitnorm(x,a,b), 0, 1, rel.tol=1e-10, subdivisions=1000)$value, 1, tolerance=1e-7)
  for (lower in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
    pr <- plogitnorm(x,a,b,lower.tail=lower,log.p=lg)
    expect_equal(pr, pnorm(qlogis(x),a,b,lower.tail=lower,log.p=lg), tolerance=1e-11)
    expect_equal(qlogitnorm(pr,a,b,lower.tail=lower,log.p=lg), x, tolerance=1e-7)
  }
  expect_equal(plogitnorm(c(-Inf,-1,0,1, Inf),a,b), c(0,0,0,1,1))
  expect_equal(dlogitnorm(c(-Inf,-1,Inf),a,b), c(0,0,0))
  expect_equal(qlogitnorm(c(-Inf,0),a,b,log.p=TRUE), c(0,1))
  expect_equal(dlogitnorm(x,a,b,log=TRUE), log(dlogitnorm(x,a,b)))
  expect_length(dlogitnorm(numeric(),a,b),0)
  expect_equal(dlogitnorm(x[1:4],c(a,a+1),b), vapply(1:4,function(i) dlogitnorm(x[i],c(a,a+1)[(i-1)%%2+1],b),0.0))
  expect_identical(dlogitnorm(c(NA_real_,NaN),a,b),c(NA_real_,NaN))
  expect_warning(expect_true(is.nan(dlogitnorm(.5,a,-1))),"NaNs")
  expect_warning(expect_true(is.nan(qlogitnorm(2,a,b))),"NaNs")
  set.seed(42); r1 <- rlogitnorm(20,a,b)
  set.seed(42); expect_equal(r1,plogis(rnorm(20,a,b)),tolerance=1e-14)
  set.seed(42); expect_identical(rlogitnorm(20,a,b),r1)
  expect_length(rlogitnorm(1:3,a,b),3)
  fit <- mllogitnorm(r1)
  expect_equal(as.numeric(logLik(fit)),sum(dml(r1,fit,log=TRUE)))
  expect_equal(pml(r1,fit),plogitnorm(r1,fit[1],fit[2]))
  expect_equal(qml(.3,fit),qlogitnorm(.3,fit[1],fit[2]))
  set.seed(4); sim <- rml(5,fit)
  set.seed(4); expect_equal(sim,rlogitnorm(5,fit[1],fit[2]))
 })
 test_that("logitnorm matches frozen logitnorm values (reference.json)", {
   expect_reference_values("logitnorm")
 })

test_that("logitnorm extreme normal log tails survive inversion", {
  for (lower in c(TRUE,FALSE)) {
    p <- c(-1000,-10000,-1e-20)
    x <- qlogitnorm(p,0,.01,lower.tail=lower,log.p=TRUE)
    expect_equal(plogitnorm(x,0,.01,lower.tail=lower,log.p=TRUE),p,tolerance=1e-10)
  }
})

test_that("logitnorm integrates with plot, bootstrap and model selection", {
  set.seed(19)
  fit <- mllogitnorm(rlogitnorm(80,1,2))
  expect_true(all(is.finite(bootstrapml(fit,reps=4))))
  expect_s3_class(model_select(rlogitnorm(80,1,2),models="logitnorm"),"univariateML")
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) })
  for (kind in c("d","p","q")) expect_invisible(plot(fit,kind=kind))
})
