betapr_oracle <- function(x) {
  n <- length(x); lx <- log(x)
  # Keep dbetapr away from catastrophic large-shape cancellation. Exact
  # package limit fits below cover the excluded infinite-shape faces.
  f <- function(p) {
    if(any(!is.finite(p)) || any(p[1:2] > log(1e5)) || any(p < -700) || p[3] > 700) return(1e100)
    ll <- sum(extraDistr::dbetapr(x, exp(p[1]), exp(p[2]), scale=exp(p[3]), log=TRUE))
    if(is.finite(ll)) -ll else 1e100
  }
  starts <- expand.grid(a=c(.1,.5,2,10,100),b=c(.1,.5,2,10,100))
  fits <- lapply(seq_len(nrow(starts)), function(i) {
    a <- starts$a[i]; b <- starts$b[i]
    optim(c(log(a),log(b),mean(lx)-digamma(a)+digamma(b)), f,
      method='BFGS', control=list(maxit=3000,reltol=1e-13))
  })
  fit <- fits[[which.min(vapply(fits, function(z) z$value, 0.))]]
  faces <- c(gamma=attr(mlgamma(x),'logLik'), invgamma=attr(mlinvgamma(x),'logLik'))
  face <- names(which.max(faces))
  list(ll=-fit$value, par=exp(fit$par), faces=faces,
       model=if(max(faces) >= -fit$value-1e-8*n) face else 'betapr')
}
test_that("three-parameter fits and D1 comparisons agree with a slow oracle", {
  samples <- list()
  for (seed in 1:12) {
    set.seed(seed)
    samples[[paste0("small", seed)]] <- extraDistr::rbetapr(20, 2, 3, scale=4)
  }
  for (seed in 1:3) {
    set.seed(seed)
    samples[[paste0("ordinary", seed)]] <- extraDistr::rbetapr(400, .7, 1.2, scale=3)
  }
  set.seed(1)
  samples$gamma <- rgamma(300, 2, rate=.5)
  set.seed(1)
  samples$invgamma <- extraDistr::rinvgamma(300, 2, 4)
  set.seed(1)
  samples$lnorm <- rlnorm(300, .7, .8)
  for (label in names(samples)) {
    x <- samples[[label]]
    o <- betapr_oracle(x)
    if (o$model == "betapr") {
      fit <- mlbetapr(x)
      expect_identical(names(fit), c("shape1", "shape2", "scale"))
      expect_equal(attr(fit, "logLik"), o$ll, tolerance=2e-7, info=label)
      expect_equal(as.numeric(fit), o$par, tolerance=2e-3, info=label)
      expect_equal(sum(extraDistr::dbetapr(x, fit[1], fit[2], scale=fit[3], log=TRUE)),
                   attr(fit, "logLik"), tolerance=1e-10)
    } else {
      expect_error(mlbetapr(x), paste0("Use `ml", o$model, "`"), info=label)
    }
    if (label == "gamma") expect_identical(o$model, "gamma")
    if (label == "invgamma") expect_identical(o$model, "invgamma")
  }
})

test_that("scale, controls, validation, and external density dispatch work", {
  set.seed(313)
  x <- extraDistr::rbetapr(1000, 1, 2, scale=3)
  fit <- mlbetapr(x)
  expect_s3_class(fit, "univariateML")
  expect_identical(attr(fit, "model"), "BetaPrime")
  expect_equal(coef(mlbetapr(c(x, NA), na.rm=TRUE)), coef(fit))
  expect_error(mlbetapr(c(x, NA)))
  expect_error(mlbetapr(c(x, 0)))
  expect_error(mlbetapr(rep(1, 20)))
  expect_error(mlbetapr(x, scale0=-1))
  expect_equal(coef(mlbetapr(x, shape10=1, shape20=2, scale0=3,
                            gradtol=1e-6, reltol=1e-13, abstol=1e-10, iterlim=1000)),
               coef(fit), tolerance=1e-4)
  moved <- mlbetapr(x * 7)
  expect_equal(as.numeric(moved[1:2]), as.numeric(fit[1:2]), tolerance=1e-4)
  expect_equal(as.numeric(moved[3]), as.numeric(fit[3])*7, tolerance=1e-4)
  expect_equal(dml(x[1:5], fit), extraDistr::dbetapr(x[1:5], fit[1], fit[2], scale=fit[3]))
})
