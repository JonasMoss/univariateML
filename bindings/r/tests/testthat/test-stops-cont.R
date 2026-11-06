test_that("continuous mathematical stops retain classes and original-unit faces", {
  caught <- function(expr) tryCatch(expr, error = identity)
  for (fit in list(function() mlexp(c(0,0,0)), function() mlnorm(c(1,1,1)),
                   function() mlgamma(c(1,1,1)), function() mlbeta(c(.5,.5,.5)))) {
    e <- caught(fit())
    expect_s3_class(e, "univariateML_nomle")
    expect_length(e$estimates, 0)
    expect_equal(nrow(e$candidates), 0L)
  }
  e <- caught(mlbhatt(c(-1,-.5,0,.5,1), a=2))
  expect_s3_class(e, "univariateML_limit")
  expect_identical(e$model, "unif")
  expect_equal(e$estimates, c(min=-2,max=2))
  expect_equal(e$limit_loglik, -5*log(4))
  e <- caught(mlgompertz(c(0,0,1,9)))
  expect_s3_class(e, "univariateML_limit")
  expect_identical(e$model, "exp")
  expect_equal(e$estimates, c(rate=.4))
  expect_equal(e$limit_loglik, 4*(log(.4)-1))
  e <- caught(mltnorm(c(2,2,3,11), a=2, b=Inf))
  expect_s3_class(e, "univariateML_limit")
  expect_identical(e$model, "unavailable:translated-exponential")
  expect_equal(e$estimates, c(mu=2,rate=.4))
  e <- caught(mlsn(c(0,.1,.2,.4,3)))
  expect_s3_class(e, "univariateML_limit")
  expect_identical(e$model, "hnorm")
  expect_equal(e$estimates, c(mu=0,sigma=sqrt(9.21/5)), tolerance=1e-8)
  expect_false(inherits(caught(mlexp(c(-1,2))), "univariateML_stop"))
  expect_false(inherits(caught(mlgamma(c(1,1,1), abstol=0)), "univariateML_stop"))
  expect_s3_class(caught(mlgev(c(0,0,0,1,2))), "univariateML_nomle")
})

test_that("Power limits use the public endpoint-then-shape order", {
  x <- .7 * (((seq_len(300) - .5) / 300)^2)
  e <- tryCatch(mlgenbeta(x, scale=1), error=identity)
  ref <- mlpower(x)
  expect_s3_class(e, "univariateML_limit")
  expect_identical(e$model, "power")
  expect_equal(e$estimates, setNames(as.numeric(ref), names(ref)), tolerance=1e-10)
  expect_equal(e$limit_loglik, as.numeric(attr(ref,"logLik")), tolerance=1e-10)
})

test_that("held subunit GED shape retains a certified half-face scale", {
  e <- tryCatch(mlsged(c(1,1,1), mean=0, nu=.5), error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"unavailable:reflected-half-ged")
  expect_equal(e$estimates,c(mu=1,scale=1/6,nu=.5),tolerance=1e-7)
  expect_equal(e$limit_loglik,3*log(3),tolerance=1e-6)
})

test_that("conditional snorm and skew-t ambiguities retain compared models", {
  fits <- list(function() mlsnorm(c(1,2),mean=0,sd=1e-4),
               function() mlst(c(1,2),xi=0,omega=1e-4,nu=1e20))
  for (i in seq_along(fits)) {
    e <- tryCatch(fits[[i]](),error=identity)
    expect_s3_class(e,"univariateML_tie")
    expect_equal(nrow(e$candidates),2L)
    expect_true(c("hnorm","ht")[[i]] %in% e$candidates$model)
    expect_equal(e$loglik_upper,Inf)
    expect_true(all(is.finite(e$candidates$loglik_lower)))
  }
})
