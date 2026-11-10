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

test_that("Newton-family subnormal reconstruction failures are numerical", {
  u <- .Machine$double.xmin * .Machine$double.eps
  for (fit in list(function() mlgumbel(c(0,u)),
                   function() mlweibull(c(u,2*u)),
                   function() mlfoldnorm(c(0,u)),
                   function() mlpig(c(0,2), mean=1e-158))) {
    e <- tryCatch(fit(),error=identity)
    expect_s3_class(e,"univariateML_numerical")
    expect_match(conditionMessage(e),"requested coefficient accuracy")
  }
})

test_that("folded-normal local boundary check survives a likelihood tie", {
  e <- .0028867513459481287
  fit <- mlfoldnorm(c(e,e,1))
  expect_equal(unname(fit["mu"]),.0037268054741952707381,tolerance=1e-10)
  expect_equal(unname(fit["sigma"]),.57734305210147493127,tolerance=1e-12)
})

test_that("small positive folded-normal basin retains coefficient accuracy", {
  fit <- mlfoldnorm(c(.02,.02,1))
  expect_equal(unname(fit["mu"]),.025828388643715272867,tolerance=1e-10)
  expect_equal(unname(fit["sigma"]),.57700337463490557989,tolerance=1e-10)
})

test_that("unresolved folded-normal quartic identification is numerical", {
  x <- c(1,1,1,sqrt(9+4*sqrt(6)))
  expect_s3_class(tryCatch(mlfoldnorm(x),error=identity),"univariateML_numerical")
  expect_equal(unname(mlfoldnorm(c(0,0,1))["mu"]),0)
})

test_that("conditional scalar fits retain coefficient accuracy", {
  x <- c(2.4349403590372,2.434940359039635,2.43494035904207,2.434940359044505,2.434940359049375)
  expect_lt(abs(mlweibull(x,scale=x[1])["shape"]/263511819158.377827834-1),1e-10)
  expect_lt(abs(mllomax(c(1,2,3),kappa=1e-14)["lambda"]/61111111111110.9798701-1),1e-10)
  expect_lt(abs(mllomax(c(1e-200,1e200),kappa=1)["lambda"]-1),1e-10)
  expect_lt(abs(mllomax(c(1e-200,1e200,1e200,1e200),kappa=1/3)["lambda"]/4.16333634234433733222e183-1),1e-10)
})

test_that("conditional free coefficients reject unrepresentable subnormals", {
  u <- .Machine$double.xmin*.Machine$double.eps
  for(fit in list(function() mlgumbel(c(0,u),sigma=u),
                  function() mlweibull(c(u,2*u),shape=1),
                  function() mllomax(c(1e158,2e158),kappa=1e158)))
    expect_s3_class(tryCatch(fit(),error=identity),"univariateML_numerical")
})
