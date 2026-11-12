test_that("folded normal d/p/q/r obey normal identities and tail conventions", {
  for (mu in c(0,.1,2,40)) for (sigma in c(.2,1,3)) {
    x <- c(0,1e-10,.1,1,10,40)
    expect_equal(dfoldnorm(x = x, mu = mu, sigma = sigma),dnorm(x,mu,sigma)+dnorm(-x,mu,sigma),tolerance=1e-12)
    for (tail in c(TRUE,FALSE)) {
      ref <- if (tail) pnorm(x,mu,sigma)-pnorm(-x,mu,sigma) else pnorm(x,mu,sigma,FALSE)+pnorm(-x,mu,sigma)
      expect_equal(pfoldnorm(q = x, mu = mu, sigma = sigma, lower.tail = tail),ref,tolerance=1e-12)
      lp <- c(-1e-10,-.7,-10,-100,if(tail) -600 else -1000)
      q <- qfoldnorm(p = lp, mu = mu, sigma = sigma, lower.tail = tail, log.p = TRUE)
      expect_equal(pfoldnorm(q = q, mu = mu, sigma = sigma, lower.tail = tail, log.p = TRUE),lp,tolerance=1e-10)
      expect_equal(qfoldnorm(p = c(0, 1), mu = mu, sigma = sigma, lower.tail = tail),if(tail) c(0,Inf) else c(Inf,0))
    }
    expect_equal(pfoldnorm(q = x, mu = mu, sigma = sigma)+pfoldnorm(q = x, mu = mu, sigma = sigma, lower.tail = FALSE),rep(1,length(x)),tolerance=1e-13)
  }
  expect_equal(pfoldnorm(q = 40, mu = 0, sigma = 1, lower.tail = FALSE, log.p = TRUE),log(2)+pnorm(40,log.p=TRUE,lower.tail=FALSE),tolerance=1e-13)
  expect_equal(pfoldnorm(q = 1e-100, mu = 40, sigma = 1, log.p = TRUE),log(2e-100)+dnorm(0,40,1,log=TRUE),tolerance=1e-13)
  expect_equal(pfoldnorm(q = 1e-100, mu = 0, sigma = 1, lower.tail = FALSE, log.p = TRUE)/1e-100,-sqrt(2/pi),tolerance=1e-13)
  set.seed(72); a <- rfoldnorm(n = 50, mu = c(0, 2), sigma = c(1, 3))
  set.seed(72); expect_identical(a,abs(rnorm(50,c(0,2),c(1,3))))
  expect_length(rfoldnorm(n = 1:4, mu = 0, sigma = 1),4)
  expect_length(rfoldnorm(n = 0, mu = 0, sigma = 1),0)
  expect_length(dfoldnorm(x = numeric(), mu = 0, sigma = 1),0)

  expect_equal(dfoldnorm(c(.1,1,2), c(0,1), 1), dfoldnorm(c(.1,1,2), c(0,1,0), 1))
  expect_true(is.na(dfoldnorm(x = NA_real_, mu = 0, sigma = 1)))
  expect_true(is.nan(dfoldnorm(x = NaN, mu = 0, sigma = 1)))
  expect_warning(expect_true(is.nan(dfoldnorm(x = 1, mu = -1, sigma = 1))),"NaNs")
  expect_warning(expect_true(is.nan(pfoldnorm(q = 1, mu = 0, sigma = 0))),"NaNs")
  expect_warning(expect_true(is.nan(qfoldnorm(p = 2, mu = 0, sigma = 1))),"NaNs")
  expect_warning(expect_true(is.nan(rfoldnorm(n = 1, mu = 0, sigma = -1))),"NaNs")
  expect_equal(dfoldnorm(x = c(-1, Inf), mu = 0, sigma = 1),c(0,0))
})

test_that("folded normal fits match a slow independent multistart oracle", {
  skip_unless_tier("full")
  samples <- list(c(0,0,0,0,1), c(0,.1,.3,.8,2),c(1,2,3,4,5),c(9,10,10,11),c(rep(.1,15),1,3),c(rep(1,99),5))
  for (x in samples) {
    f <- mlfoldnorm(x)
    objective <- function(z) {
      mu <- z[1]; sigma <- exp(z[2])
      if (mu<0) return(1e100)
      a <- dnorm(x,mu,sigma,log=TRUE); b <- dnorm(-x,mu,sigma,log=TRUE)
      hi <- pmax(a,b)
      -sum(hi+log1p(exp(pmin(a,b)-hi)))
    }
    best <- min(vapply(c(0,.1,.5,1,2,5),function(k) {
      optim(c(k*mean(x),log(sd(x))),objective,method="L-BFGS-B",lower=c(0,-20),upper=c(Inf,20),control=list(factr=1))$value
    },numeric(1)))
    expect_equal(as.numeric(logLik(f)),-best,tolerance=1e-7)
    expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-12)
    expect_equal(sum(f^2),mean(x^2),tolerance=1e-10)
    for (args in list(list(mu=0),list(mu=1),list(sigma=.8),list(mu=1,sigma=.8))) {
      g <- do.call(mlfoldnorm,c(list(x),args))
      expect_setequal(attr(g,"fixed"),names(args))
      expect_equal(attr(logLik(g),"df"),2L-length(args))
      expect_equal(as.numeric(logLik(g)),sum(dml(x,g,log=TRUE)),tolerance=1e-12)
      if (length(args)==1) {
        o <- if (!is.null(args$mu)) optimize(function(t) -sum(dfoldnorm(x = x, mu = args$mu, sigma = exp(t), log = TRUE)),c(-10,10),tol=1e-10) else
          optimize(function(m) -sum(dfoldnorm(x = x, mu = m, sigma = args$sigma, log = TRUE)),c(0,max(x)),tol=1e-10)
        expect_equal(as.numeric(logLik(g)),-o$objective,tolerance=1e-8)
      }
    }
  }
  competing <- c(rep(1,99),5)
  expect_gt(mean(competing^4),3*mean(competing^2)^2)
  expect_gt(mlfoldnorm(competing)[["mu"]],0)
  expect_gt(as.numeric(logLik(mlfoldnorm(competing))),as.numeric(logLik(mlfoldnorm(competing,mu=0))))
  f <- mlfoldnorm(c(0,0,0,0,1))
  expect_identical(unname(f[["mu"]]),0)
  expect_equal(unname(f[["sigma"]]),sqrt(.2))
  h <- mlhnorm(c(0,0,0,0,1), mu = NULL)
  expect_equal(unname(f[["sigma"]]),unname(h[["sigma"]]))
  expect_equal(as.numeric(logLik(f)),as.numeric(logLik(h)))
  x <- c(.1,.3,.7,1,2,3); original <- mlfoldnorm(x)
  for (scale in c(1e-100,1e100)) {
    scaled <- mlfoldnorm(x*scale)
    expect_equal(as.numeric(scaled)/scale,as.numeric(original),tolerance=1e-6)
    expect_equal(as.numeric(logLik(scaled)),as.numeric(logLik(original))-length(x)*log(scale),tolerance=1e-12)
  }
  expect_error(mlfoldnorm(c(1,1)),"unbounded")
  expect_error(mlfoldnorm(rep(1,10)),"unbounded")
  expect_error(mlfoldnorm(c(0,0)),"unbounded")
  expect_error(mlfoldnorm(c(-1,2)),"support|nonnegative")
  expect_error(mlfoldnorm(c(1,2),mu=-1),"mu")
  expect_error(mlfoldnorm(c(1,2),sigma=0),"sigma")
  expect_equal(unname(mlfoldnorm(c(1,1),mu=0)[["sigma"]]),1)
  expect_equal(unname(mlfoldnorm(c(0,0),sigma=1)[["mu"]]),0)
  expect_equal(unname(mlfoldnorm(c(0,1,2),mu=0)[["sigma"]]),sqrt(5/3))
})

test_that("folded normal integrates with fitted-object methods", {
  set.seed(7); x <- rfoldnorm(n = 40, mu = 2, sigma = 1); f <- mlfoldnorm(x,mu=2)
  u <- c(.01,.3,.9)
  expect_equal(pml(qml(u,f),f),u,tolerance=1e-12)
  set.seed(1); a <- rml(10,f); set.seed(1); expect_identical(a,rfoldnorm(n = 10, mu = 2, sigma = f[["sigma"]]))
  expect_s3_class(model_select(x,models=c("foldnorm","norm")),"univariateML")
  expect_equal(dim(bootstrapml(f,reps=3)),c(2L,2L))
  tmp <- tempfile(fileext=".pdf"); grDevices::pdf(tmp)
  expect_error(plot(f),NA)
  grDevices::dev.off(); unlink(tmp)
})

test_that("folded normal log quantiles span the double exponent range", {
  q <- qfoldnorm(p = -1000, mu = 0, sigma = 1e+300, log.p = TRUE)
  expect_gt(q,0)
  expect_equal(pfoldnorm(q = q, mu = 0, sigma = 1e+300, log.p = TRUE),-1000,tolerance=1e-12)
  expect_true(is.finite(qfoldnorm(p = 0.5, mu = 1e+308, sigma = 1e+308)))
  expect_equal(pfoldnorm(q = 1e+300, mu = 1e+300, sigma = 1e-05),.5)
})

test_that("folded normal compares two distinct interior profile maxima", {
  x <- c(.6986839225419381,.2562548690820494,.3007670968247965,
    .1186039408776371,.19075617333842024,.2781001825837044,
    .4979965214041169,.2063506498314821,.31027919417683314,
    .30928300405819176,.2729747407467472,.2835705294774539,
    1,.3820824073712553)
  a <- c(1.3211665377044026,2.0184245292991547,3.8430493970863107)
  m2 <- mean(x^2); v <- 2*m2/(1+sqrt(1+4*m2*a^2)); mu <- a*v
  score <- vapply(seq_along(a),function(i) mean(x*tanh(a[i]*x))-mu[i],numeric(1))
  curvature <- vapply(seq_along(a),function(i)
    mean(x^2/cosh(a[i]*x)^2)-v[i]/sqrt(1+4*m2*a[i]^2),numeric(1))
  expect_lt(max(abs(score)),1e-12)
  expect_equal(sign(curvature),c(-1,1,-1))
  oracle <- vapply(seq_along(a),function(i) {
    z <- dnorm(x,mu[i],sqrt(v[i]),log=TRUE)
    w <- dnorm(-x,mu[i],sqrt(v[i]),log=TRUE)
    sum(pmax(z,w)+log1p(exp(-abs(z-w))))
  },numeric(1))
  f <- mlfoldnorm(x)
  expect_equal(as.numeric(logLik(f)),max(oracle),tolerance=1e-10)
  k <- which.max(oracle)
  expect_equal(as.numeric(f),c(mu[k],sqrt(v[k])),tolerance=1e-5)
  expect_gt(as.numeric(logLik(f)),as.numeric(logLik(mlfoldnorm(x,mu=0))))
})

test_that("foldnorm R6 collapse diagnostics and held exceptions", {
  for (v in c(0,2)) {
    expect_error(mlfoldnorm(rep(v,3)),"unbounded")
    expect_error(mlfoldnorm(rep(v,3),mu=v),"unbounded")
  }
  expect_true(is.finite(logLik(mlfoldnorm(rep(2,3),sigma=1))))
})

test_that("folded normal distinguishes conditioning at the default tolerance", {
  e <- tryCatch(mlfoldnorm(c(1,1,1,sqrt(9+4*sqrt(6)))),error=identity)
  expect_s3_class(e,"univariateML_conditioning")
  expect_s3_class(e,"univariateML_numerical")
  expect_identical(e$kind,"conditioning")
  expect_gt(nrow(e$candidates),0)
  expect_true(all(is.finite(e$candidates$loglik_lower)))
  expect_lt(e$attainable_tol,1e-10)
})
