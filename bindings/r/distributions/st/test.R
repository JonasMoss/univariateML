test_that("st evaluates the symmetric Student-t member", {
  x <- c(-2, 0, 1, 4)
  model <- mlst(xi=0, omega=1, alpha=0, nu=4)
  expect_equal(dml(x, model), dt(x, 4), tolerance=1e-10)
})
test_that("st diagnoses scale collapse", {
  expect_error(mlst(c(0,0,0,1,2)), "unbounded")
})

test_that("st flags a fit on the df restriction", {
  x <- c(-1000, -10, -1, 0, 1, 10, 1000)
  expect_warning(f <- mlst(x, alpha = 0), class = "univariateML_restricted")
  expect_identical(attr(f, "restricted"), c(nu = "lower"))
})

test_that("st conditional skew certificates preserve endpoint factors and stops", {
  f <- mlst(c(-3,-1,-.2,.1,.8,2,4),xi=0,omega=1,nu=4)
  expect_equal(unname(coef(f)["alpha"]),.16456272666935215,tolerance=1e-8)
  expect_equal(as.numeric(logLik(f)),-16.43971347475502,tolerance=1e-11)
  expect_error(mlst(c(2,2,2),xi=2,omega=1,nu=.5),class="univariateML_tie")
  face <- tryCatch(mlst(c(0,1,2),xi=0,omega=1,nu=4),univariateML_limit=identity)
  expect_s3_class(face,"univariateML_limit")
  expect_equal(face$limit_loglik,sum(log(2)+dt(c(0,1,2),4,log=TRUE))-log(2),tolerance=1e-12)
  expect_error(mlst(c(-1e-12,-1,.1,1,4),xi=0,omega=1,nu=4,iterlim=1),class="univariateML_budget")
})

test_that("st location certificates expose ties and budget witnesses", {
  expect_error(mlst(c(-2,2),omega=1,alpha=0,nu=1),class="univariateML_tie")
  expect_error(mlst(c(-3,-1,-.2,.1,.8,2,4),omega=1,alpha=2,nu=4,iterlim=1),class="univariateML_budget")
  x <- c(-3,-1,-.2,.1,.8,2,4)
  f <- mlst(x,omega=1,alpha=2,nu=4)
  g <- mlst(-x,omega=1,alpha=-2,nu=4)
  expect_equal(unname(coef(f)["xi"]),-unname(coef(g)["xi"]),tolerance=1e-9)
  expect_equal(as.numeric(logLik(f)),as.numeric(logLik(g)),tolerance=1e-11)
})

test_that("st one-sign alpha-scale certificate profiles the reachable half-t face", {
  for (sign in c(-1,1)) {
    x <- 2+sign*c(0,1,2,4)
    face <- tryCatch(mlst(x,xi=2,nu=4),univariateML_limit=identity)
    expect_s3_class(face,"univariateML_limit")
    expect_equal(unname(face$estimates["mu"]),2)
    scale <- unname(face$estimates["sigma"])
    expect_equal(face$limit_loglik,sum(log(2)+dt((x-2)/scale,4,log=TRUE)-log(scale))-log(2),tolerance=1e-11)
  }
  for (a in list(0,NULL)) {
    collapse <- tryCatch(mlst(c(0,1),xi=0,alpha=a,nu=1),univariateML_nomle=identity)
    expect_s3_class(collapse,"univariateML_nomle")
    expected <- -2*log(pi)+if(is.null(a)) log(2) else 0
    expect_lte(collapse$loglik_lower,expected)
    expect_gte(collapse$loglik_upper,expected)
  }
})

test_that("st constant-data location-skew fit preserves both limiting witnesses", {
  f <- tryCatch(mlst(c(2,2,2),omega=1,nu=4),univariateML_tie=identity)
  expect_s3_class(f,"univariateML_tie")
  expect_setequal(f$candidates$model,c("ht","unavailable:reflected-half-t"))
  bound <- 3*(log(2)+dt(0,4,log=TRUE))
  expect_lte(f$loglik_lower,bound)
  expect_gte(f$loglik_upper,bound)
})

test_that("st symmetric all-nuisance reduction transports the normal face", {
  f <- tryCatch(mlst(c(-2,-1,0,1,2),alpha=0),univariateML_limit=identity)
  expect_s3_class(f,"univariateML_limit")
  expect_identical(f$model,"sn")
  expect_equal(unname(f$estimates["alpha"]),0)
})

test_that("st refuses unresolved thin-tail score arithmetic", {
  expect_error(mlst(c(-1000,-10,0,1,2),omega=1,alpha=1e30,nu=1e20),
               class="univariateML_numerical")
})

test_that("st constant-data free-nu corner keeps both half-normal faces", {
  f <- tryCatch(mlst(c(2,2,2),omega=1),univariateML_tie=identity)
  expect_s3_class(f,"univariateML_tie")
  expect_setequal(f$candidates$model,c("hnorm","unavailable:reflected-half-normal"))
  bound <- 3*(log(2)+dnorm(0,log=TRUE))
  expect_lte(f$loglik_lower,bound)
  expect_gte(f$loglik_upper,bound)
})
