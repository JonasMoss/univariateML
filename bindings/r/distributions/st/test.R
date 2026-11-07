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
