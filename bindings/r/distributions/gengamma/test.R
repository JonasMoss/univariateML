test_that("mlgengamma fixed Q=0 reduces to lognormal", {
  x <- exp(c(-1.4,-0.3,0.2,0.6,1.1,1.8))
  fit <- mlgengamma(x,Q=0)
  normal <- mllnorm(x)
  expect_equal(unname(fit[c("mu","sigma")]), unname(normal[c("meanlog","sdlog")]), tolerance=1e-9)
  expect_identical(unname(fit["Q"]),0)
  expect_equal(attr(fit,"logLik"),attr(normal,"logLik"),tolerance=1e-10)
  expect_true(isTRUE(attr(fit,"certified")))
  loc <- mlgengamma(x,sigma=1.3,Q=0)
  expect_equal(unname(loc["mu"]),mean(log(x)),tolerance=1e-10)
  scale <- mlgengamma(x,mu=.2,Q=0)
  expect_equal(unname(scale["sigma"]),sqrt(mean((log(x)-.2)^2)),tolerance=1e-9)
})

test_that("mlgengamma validates all held coefficients and constant data", {
  x <- exp(c(-1.4,-0.3,0.2,0.6,1.1,1.8))
  expect_error(mlgengamma(x,sigma=0),"sigma")
  expect_error(mlgengamma(x,mu=Inf),"mu")
  expect_error(mlgengamma(x,Q=Inf),"Q")
  expect_error(mlgengamma(x,tol=0),"tol")
  expect_error(mlgengamma(rep(1,5)),"unbounded")
  expect_error(mlgengamma(rep(1,5),mu=0),"unbounded")
  fit <- mlgengamma(x,mu=.2,sigma=1.3,Q=-.2)
  expect_equal(as.numeric(fit),c(.2,1.3,-.2))
  expect_equal(attr(fit,"logLik"),sum(dml(x,fit,log=TRUE)),tolerance=1e-10)
})

test_that("mlgengamma checks a signed near-lognormal shape", {
  z <- qnorm(((1:200)-.5)/200)
  for(epsilon in c(1e-4,1e-8)) for(sign in c(-1,1)) {
    x <- exp(z+sign*epsilon*(z*z-mean(z*z)))
    fit <- mlgengamma(x)
    expect_lt(sign*unname(fit["Q"]),0)
    expect_gt(abs(unname(fit["Q"])),1e-10)
    expect_lt(abs(unname(fit["Q"])),.01)
    if (epsilon==1e-8) expect_identical(attr(fit,"certified"),FALSE)
  }
})

test_that("mlgengamma routes every hold combination", {
  set.seed(12470)
  x <- rml(120, mlgengamma(mu=0,sigma=1,Q=.4))
  held_values <- list(mu=0,sigma=1,Q=.4)
  for (mask in 0:7) {
    held <- names(held_values)[as.logical(bitwAnd(mask,2^(0:2)))]
    fit <- do.call(mlgengamma,c(list(x=x),held_values[held]))
    expect_identical(sort(attr(fit,"fixed")),sort(held),info=paste("mask",mask))
    for (name in held) expect_equal(unname(fit[name]),held_values[[name]])
    expect_equal(attr(fit,"logLik"),sum(dml(x,fit,log=TRUE)),tolerance=1e-9)
  }
})

test_that("shape-free holds agree with independent multistart likelihood searches", {
  skip_unless_tier("full")
  set.seed(9317)
  x <- rml(200, mlgengamma(mu=.2,sigma=.9,Q=.6))
  held_values <- list(mu=.2,sigma=.9)
  for (mask in 0:3) {
    held <- names(held_values)[as.logical(bitwAnd(mask,2^(0:1)))]
    fit <- do.call(mlgengamma,c(list(x=x),held_values[held]))
    base <- c(mu=.2,log_sigma=log(.9),Q=.6)
    free <- c(if (!("mu" %in% held)) "mu",if (!("sigma" %in% held)) "log_sigma","Q")
    nll <- function(v) {
      p <- base;p[free] <- v
      value <- suppressWarnings(-sum(dgengamma(x,p["mu"],exp(p["log_sigma"]),p["Q"],log=TRUE)))
      if (is.finite(value)) value else 1e100
    }
    starts <- list(c(mu=mean(log(x)),log_sigma=log(sd(log(x))),Q=0),
                   c(mu=mean(log(x)),log_sigma=log(sd(log(x))),Q=1),
                   c(mu=mean(log(x)),log_sigma=log(sd(log(x))),Q=-1))
    values <- numeric()
    for (start in starts) {
      a <- if (length(free)>1) optim(start[free],nll,method="Nelder-Mead",control=list(maxit=2000,reltol=1e-11)) else list(par=start[free])
      b <- optim(a$par,nll,method="BFGS",control=list(maxit=1000,reltol=1e-13))
      if (b$convergence==0 && is.finite(b$value) && b$value<1e90) values <- c(values,b$value)
    }
    expect_gt(length(values),0)
    if (length(values)) expect_gte(attr(fit,"logLik"),-min(values)-1e-6*length(x))
  }
})

test_that("strict faces retain their limit model and certificate", {
  x <- c(1,2,3)
  power <- tryCatch(mlgengamma(x),error=identity)
  expect_s3_class(power,"univariateML_limit")
  expect_identical(power$model,"power")
  expect_true(isTRUE(power$certified))
  expect_equal(power$estimates,c(alpha=max(x),beta=1/(log(max(x))-mean(log(x)))),tolerance=1e-10)
  inv <- 1/x
  pareto <- tryCatch(mlgengamma(inv),error=identity)
  expect_s3_class(pareto,"univariateML_limit")
  expect_identical(pareto$model,"pareto")
  expect_true(isTRUE(pareto$certified))
  expect_equal(pareto$estimates,c(a=1/(mean(log(inv))-log(min(inv))),b=min(inv)),tolerance=1e-10)
})

test_that("tied faces and held endpoint comparisons do not claim a strict limit", {
  two <- tryCatch(mlgengamma(c(1,2)),error=identity)
  if (inherits(two,"error")) expect_false(inherits(two,"univariateML_limit"))
  else {
    face <- -2*(1+log((log(2))/2))-sum(log(c(1,2)))
    expect_gte(attr(two,"logLik"),face-1e-8)
  }
  x <- c(1,2,3)
  held <- tryCatch(mlgengamma(x,mu=log(3)),error=identity)
  if (inherits(held,"error")) expect_false(inherits(held,"univariateML_limit"))
  else expect_true(is.finite(attr(held,"logLik")))
})

test_that("exact multiplicative symmetry can certify the lognormal member", {
  x <- 2^(-5:5)
  fit <- mlgengamma(x)
  expect_identical(unname(fit["Q"]),0)
  expect_true(isTRUE(attr(fit,"certified")))
})

test_that("gengamma transports public tolerance and conditioning witnesses", {
  for (tol in c(1e-20,1e-14)) {
    e <- tryCatch(mlgengamma(c(.2,.7,1,2,4),sigma=1,Q=0,tol=tol),error=identity)
    expect_s3_class(e,if(tol<1e-16) "univariateML_tolerance" else "univariateML_conditioning")
    expect_s3_class(e,"univariateML_numerical")
    expect_gt(nrow(e$candidates),0L)
    expect_equal(unname(unlist(tail(e$candidates,1)[c("sigma","Q")])),c(1,0))
  }
})

test_that("internal gengamma conditional brackets retain public accuracy detail", {
  tiny <- .Machine$double.xmin*.Machine$double.eps
  e <- tryCatch(mlgengamma(c(1,1),mu=tiny),error=identity)
  expect_s3_class(e,"univariateML_tolerance")
  expect_gte(e$attainable_tol,1)
  expect_true(all(e$candidates$sigma>0))
  expect_true(all(is.finite(e$candidates$loglik_lower)))
})
