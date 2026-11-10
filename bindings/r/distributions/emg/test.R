test_that("EMG evaluation matches independent convolution and direct tails", {
  for (lambda in c(.01, 1, 100, 1e6)) for (x in c(-40, -2, 0, 2, 40)) {
    ld <- log(lambda) - lambda*x + lambda^2/2 + pnorm(x-lambda, log.p=TRUE)
    if (lambda <= 100) expect_equal(demg(x = x, mu = 0, sigma = 1, lambda = lambda, log = TRUE), ld, tolerance=1e-9)
    expect_equal(demg(x = x, mu = 0, sigma = 1, lambda = lambda), exp(demg(x = x, mu = 0, sigma = 1, lambda = lambda, log = TRUE)))
    expect_equal(pemg(q = x, mu = 0, sigma = 1, lambda = lambda)+pemg(q = x, mu = 0, sigma = 1, lambda = lambda, lower.tail = FALSE), 1)
  }
  for (x in c(-5, 0, 5)) {
    oracle <- integrate(function(t) dnorm(t)*pexp(x-t), -Inf, x, rel.tol=1e-11)$value
    expect_equal(pemg(q = x, mu = 0, sigma = 1, lambda = 1), oracle, tolerance=1e-10)
  }
})
test_that("EMG tails, recycling and random generation follow stats conventions", {
  for (lambda in c(1e-300,1e-8,1,100,1e6)) for (lower in c(TRUE,FALSE)) {
    p <- c(-1000,-100,-10,log(.5),-.0001,-1e-100)
    q <- qemg(p = p, mu = 0, sigma = 1, lambda = lambda, lower.tail = lower, log.p = TRUE)
    expect_equal(pemg(q = q, mu = 0, sigma = 1, lambda = lambda, lower.tail = lower, log.p = TRUE)/p,rep(1,length(p)),tolerance=1e-7)
  }
  expect_equal(demg(x = c(0, 1), mu = 0, sigma = c(1, 2), lambda = 1),c(demg(x = 0, mu = 0, sigma = 1, lambda = 1),demg(x = 1, mu = 0, sigma = 2, lambda = 1)))
  expect_true(is.na(demg(x = NA_real_, mu = 0, sigma = 1, lambda = 1)))
  expect_warning(expect_true(is.nan(demg(x = 0, mu = 0, sigma = -1, lambda = 1))),"NaNs")
  expect_warning(expect_true(is.nan(qemg(p = 2, mu = 0, sigma = 1, lambda = 1))),"NaNs")
  expect_equal(pemg(q = 100, mu = 0, sigma = 1, lambda = 1, log.p = TRUE)/(-exp(-99.5)),1,tolerance=1e-12)
  expect_equal(pemg(q = 100, mu = 0, sigma = 1, lambda = 1e-300, lower.tail = FALSE, log.p = TRUE)/(-1e-298),1,tolerance=1e-12)
  expect_length(remg(n = 0, mu = 0, sigma = 1, lambda = 1),0)
  expect_equal(pemg(q = c(-1e+100, -1e+150), mu = 0, sigma = 1, lambda = 1, log.p = TRUE),-c(1e200,1e300)/2,tolerance=1e-14)
  q <- qemg(p = 1e-300, mu = 0, sigma = 1, lambda = 9.99988867182683e-321)
  expect_true(is.finite(q))
  expect_equal(pemg(q = q, mu = 0, sigma = 1, lambda = 9.99988867182683e-321, log.p = TRUE),log(1e-300),tolerance=1e-12)
  set.seed(123); a <- remg(n = 20, mu = 0, sigma = 1, lambda = 1)
  set.seed(123); expect_identical(a,remg(n = 20, mu = 0, sigma = 1, lambda = 1))
  set.seed(124); z <- remg(n = 30000, mu = 2, sigma = 1, lambda = 2)
  expect_equal(mean(z),2.5,tolerance=.03)
  expect_equal(var(z),1.25,tolerance=.04)
})
test_that("EMG fixed subsets agree with a slow independent optimizer", {
  skip_unless_tier("full")
  x <- qemg(p = (seq_len(40) - 0.5)/40, mu = 0, sigma = 1, lambda = 1)
  objective <- function(t) {
    mu <- t[1]; sigma <- exp(t[2]); lambda <- exp(t[3])
    -sum(log(lambda)+lambda*(mu-x)+lambda^2*sigma^2/2+pnorm((x-mu)/sigma-lambda*sigma,log.p=TRUE))
  }
  base <- c(0,0,0)
  for (mask in 0:7) {
    held <- as.logical(intToBits(mask)[1:3]); free <- which(!held)
    args <- list(x=x)
    for(j in which(held)) args[[c("mu","sigma","lambda")[j]]] <- c(0,1,1)[j]
    fit <- do.call(mlemg,args)
    if(length(free)) {
      values <- sapply(c(-.5,0,.5),function(shift) {
        optim(base[free]+shift,function(z){t<-base;t[free]<-z;objective(t)},method=if(length(free)==1) "BFGS" else "Nelder-Mead",control=list(maxit=5000,reltol=1e-12))$value
      })
      expect_equal(as.numeric(logLik(fit)),-min(values),tolerance=2e-7)
    } else expect_equal(as.numeric(logLik(fit)),-objective(base))
    expect_equal(unname(as.numeric(fit)[held]),c(0,1,1)[held])
    expect_equal(attr(logLik(fit),"df"),sum(!held))
    expect_equal(sum(dml(x,fit,log=TRUE)),as.numeric(logLik(fit)),tolerance=1e-10)
  }
  fit <- mlemg(x)
  expect_equal(pml(qml(c(.1,.5,.9),fit),fit),c(.1,.5,.9),tolerance=1e-9)
  expect_length(rml(10,fit),10)
  expect_error(mlemg(rep(1,5)),"unbounded")
  expect_error(mlemg(-2:2),"translated exponential")
  expect_error(mlemg(-2:2,mu=0,sigma=1),"use mlnorm")
  expect_error(mlemg(c(0,0,.1,4)),"translated exponential")
})
test_that("EMG integrates with plotting, bootstrap and model selection", {
  x <- qemg(p = (seq_len(40) - 0.5)/40, mu = 0, sigma = 1, lambda = 1)
  fit <- mlemg(x,mu=0,sigma=1,lambda=1)
  set.seed(17)
  boot <- bootstrapml(fit,reps=2,reducer=mean)
  expect_equal(as.numeric(boot),c(0,1,1))
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file)
  for(kind in c("d","p","q")) expect_identical(plot(fit,kind=kind),fit)
  grDevices::dev.off()
  unlink(file)
  selected <- model_select(x,models=c("norm","emg"),return="all")
  expect_true("Exponentially Modified Gaussian" %in% selected$model)
})

test_that("emg: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "emg", "golden.json"))
  x <- g$data
  expect_error(mlemg(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("EMG certified location slice retains holds and structured numerical stops", {
  x <- qemg((seq_len(17)-.5)/17, mu=2, sigma=1, lambda=1)
  fit <- mlemg(x, sigma=1, lambda=1, reltol=1e-10)
  expect_equal(unname(as.numeric(fit)[2:3]), c(1,1))
  expect_equal(sum(dml(x,fit,log=TRUE)), as.numeric(logLik(fit)), tolerance=1e-12)
  e <- tryCatch(mlemg(x,sigma=1,lambda=1,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(nrow(e$candidates)>=1)
  expect_lte(e$loglik_lower,e$loglik_upper)
  e <- tryCatch(mlemg(x,sigma=1,lambda=1,reltol=1e-30),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})
test_that("EMG proved normal domination keeps the conditional normal coefficients", {
  e <- tryCatch(mlemg(-2:2,mu=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$estimates,c(mean=2,sd=sqrt(6)))
  e <- tryCatch(mlemg(rep(4,3),sigma=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$estimates,c(mean=4,sd=2))
})
test_that("EMG constant exponential faces preserve the endpoint half-density", {
  for (held in c(FALSE,TRUE)) {
    args <- list(x=rep(4,3),lambda=2)
    if (held) args$mu <- 4
    e <- tryCatch(do.call(mlemg,args),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_equal(e$estimates,c(mu=4,rate=2))
    expect_equal(e$limit_loglik,if(held) 0 else 3*log(2))
  }
})
test_that("EMG constant classification uses the data rather than rounded variance", {
  e <- tryCatch(mlemg(rep(.1,7),iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_nomle")
})

test_that("EMG outer certificate preserves budget and unresolved witnesses", {
  e <- tryCatch(mlemg(-2:2,mu=0,sigma=1,iterlim=2),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_match(conditionMessage(e),"raising iterlim")
  expect_gte(nrow(e$candidates),2)
  expect_lte(e$loglik_lower,e$loglik_upper)
  e <- tryCatch(mlemg(-2:2,mu=0),error=identity)
  expect_s3_class(e,"univariateML_unresolved")
  expect_gte(nrow(e$candidates),2)
  expect_lte(e$loglik_lower,e$loglik_upper)
})

test_that("emg audit-support case 1374 checks the public fit", {
  x <- c(2.3413394878482614e+150,2.2897087235884532e+150,1.3183800389288958e+150,6.757171112452496e+149,5.611056796430911e+150,1.1860909484474617e+150,2.1942876383991374e+150,-3.8622205310315114e+149,5.922717764038318e+149,4.933410406124059e+150,3.8273210056823587e+149,-8.891876547554337e+149)
  fit <- suppressWarnings(tryCatch(mlemg(x,reltol=1e-10,iterlim=100,sigma=1.0518147225573364,lambda=1.209185930245896),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("emg audit-support case 1375 checks the public fit", {
  x <- c(100000002.34133948,100000002.28970872,100000001.31838004,100000000.67571712,100000005.61105679,100000001.18609095,100000002.19428764,99999999.61377795,100000000.59227178,100000004.9334104,100000000.3827321,99999999.11081235)
  fit <- suppressWarnings(tryCatch(mlemg(x,reltol=1e-10,iterlim=100,sigma=1.0518147225573364,lambda=1.209185930245896),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
    expect_equal(unname(fit["mu"]),100000000.60179031,tolerance=1e-16)
  }
})
