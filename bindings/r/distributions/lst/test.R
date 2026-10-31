test_that("location-scale t agrees with independent density likelihood optimization", {
  skip_unless_tier("full")
  for (df in c(1.5, 3, 10)) for (n in c(200, 1000)) {
    set.seed(100 + n + df * 10)
    x <- rlst(n = n, mu = 2, sigma = 1.3, df = df)
    fit <- suppressWarnings(mllst(x, gradtol = 1e-7, iterlim = 500), classes = "univariateML_restricted")
    objective <- function(p) {
      sigma <- exp(p[2]); nu <- 1 + exp(p[3])
      if (!all(is.finite(c(p[1],sigma,nu))) || sigma <= 0) return(1e100)
      -sum(dlst(x = x, mu = p[1], sigma = sigma, df = nu, log = TRUE))
    }
    candidates <- lapply(c(1.1, 2, 5, 20, 100), function(d)
      optim(c(median(x), log(IQR(x)), log(d - 1)), objective,
            method = "BFGS", control = list(maxit = 2000, reltol = 1e-12)))
    oracle <- candidates[[which.min(vapply(candidates, `[[`, 0.0, "value"))]]
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-8)
    expect_equal(as.numeric(fit), c(oracle$par[1], exp(oracle$par[2]),
                                  1 + exp(oracle$par[3])), tolerance = 2e-3)
    expect_equal(names(fit), c("mu", "sigma", "df"))
    expect_equal(attr(fit, "logLik"), sum(dlst(x = x, mu = fit[1], sigma = fit[2], df = fit[3], log = TRUE)), tolerance = 1e-10)

  }
})

test_that("Cauchy is attained and normal escapes name mlnorm", {
  set.seed(1)
  x <- rcauchy(200, location = 2, scale = 1.3)
  fit <- suppressWarnings(mllst(x), classes = "univariateML_restricted")
  expect_equal(unname(fit[3]), 1)
  expect_equal(as.numeric(fit[1:2]), as.numeric(mlcauchy(x)), tolerance = 1e-3)
  expect_error(suppressWarnings(mllst(qnorm(((1:200) - 0.5) / 200)), classes = "univariateML_restricted"), "Use `mlnorm`", fixed = TRUE)
  set.seed(1)
  expect_error(suppressWarnings(mllst(rnorm(200)), classes = "univariateML_restricted"), "Use `mlnorm`", fixed = TRUE)
})

test_that("collapse configurations are rejected and allowed ties fit", {
  for (x in list(c(1, 2), rep(1, 5), c(0, 0, 1, 2), c(0, 0, 0, 1, 2)))
    expect_error(suppressWarnings(mllst(x), classes = "univariateML_restricted"), "no value repeated n/2", fixed = TRUE)
  expect_error(suppressWarnings(mllst(c(1:5, NA)), classes = "univariateML_restricted"))
  expect_s3_class(suppressWarnings(mllst(c(0, 0, 1, 2, 10)), classes = "univariateML_restricted"), "univariateML")
})

test_that("std two-cluster escape has a location-scale fit", {
  x <- c(seq(-1e-4, 1e-4, length = 30), seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "Use `mllst`", fixed = TRUE)
  fit <- suppressWarnings(mllst(x), classes = "univariateML_restricted")
  expect_true(is.finite(attr(fit, "logLik")))
  # Free df prefers Cauchy here: the std face at df=2 is not the lst MLE.
  expect_equal(unname(fit[3]), 1)
  t2 <- optim(c(0, log(1e-4)), function(p) -sum(dlst(x = x, mu = p[1], sigma = exp(p[2]), df = 2, log = TRUE)),
    method = "BFGS", control = list(maxit = 2000, reltol = 1e-12))
  expect_gt(attr(fit, "logLik"), -t2$value + 70)
})

test_that("fixed df agrees with oracle and Cauchy, with correct metadata", {
  skip_unless_tier("full")
  x <- c(-5,-2,-1,0,0,1,2,9)
  for (df in c(1, 2.5, 10)) {
    fit <- suppressWarnings(mllst(x, df = df, gradtol = 1e-8, iterlim = 5000), classes = "univariateML_restricted")
    objective <- function(p) -sum(dlst(x = x, mu = p[1], sigma = exp(p[2]), df = df, log = TRUE))
    oracle <- optim(c(0,0), objective, method = "BFGS",
                    control = list(reltol = 1e-13, maxit = 3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-9)
    expect_equal(as.numeric(fit[1:2]), c(oracle$par[1],exp(oracle$par[2])), tolerance = 1e-4)
    expect_identical(attr(fit, "fixed"), "df")
    expect_equal(attr(logLik(fit), "df"), 2)
    expect_identical(unname(fit[3]), df)
    if (df == 1) expect_equal(as.numeric(fit[1:2]), as.numeric(mlcauchy(x)), tolerance = 1e-3)
  }
  for (df in c(-1,0,Inf,NaN)) expect_error(suppressWarnings(mllst(x, df = df), classes = "univariateML_restricted"), "positive")
  expect_error(suppressWarnings(mllst(c(0,0,0,1), df = 2), classes = "univariateML_restricted"), "unbounded")
  expect_error(suppressWarnings(mllst(c(0,0,0,1), df = 3), classes = "univariateML_restricted"), "equality")
  expect_s3_class(suppressWarnings(mllst(c(0,0,0,1), df = 4, iterlim = 5000), classes = "univariateML_restricted"), "univariateML")
  expect_s3_class(suppressWarnings(mllst(qnorm((1:100-.5)/100), df = 4), classes = "univariateML_restricted"), "univariateML")
})

test_that("held huge degrees of freedom retain the normal log density", {
  x <- c(-2, -1, 0, 1, 3)
  f <- suppressWarnings(mllst(x, df = 1e300), classes = "univariateML_restricted")
  expect_equal(attr(f, "logLik"), sum(dt((x-f[["mu"]])/f[["sigma"]], df=1e300, log=TRUE)-log(f[["sigma"]])), tolerance=1e-10)
  expect_equal(attr(f, "logLik"), attr(mlnorm(x), "logLik"), tolerance=1e-8)
})

test_that("held Cauchy degrees of freedom allow balanced two-point maxima", {
  f <- suppressWarnings(mllst(c(1,1,2,2),df=1), classes = "univariateML_restricted")
  g <- mlcauchy(c(1,1,2,2))
  expect_equal(f[["mu"]],g[["location"]])
  expect_equal(f[["sigma"]],g[["scale"]])
  expect_equal(attr(f,"logLik"),attr(g,"logLik"))
})


test_that("lst new holds agree with independent multistart likelihoods", {
  skip_unless_tier("full")
  set.seed(8100+4336)
  held_values <- c(mu=0.2,sigma=1.3,df=3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dlst(x = x, mu = p[1], sigma = p[2], df = p[3], log = TRUE)
  for(x in samples)for(mask in 1:7) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mllst,c(list(x=x),args))),error=identity)
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"[Uu]se `ml|unbounded|collapse|support cannot contain|does not exist|positive")
      next
    }
    expect_identical(unname(as.numeric(f[held])),unname(held_values[held]))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(attr(logLik(f),"df"),length(held_values)-length(held))
    expect_equal(as.numeric(logLik(f)),sum(ld(x,as.numeric(f))),tolerance=1e-7)
    free <- setdiff(names(held_values),held)
    if(!length(free))next
    encode <- function(p) {z<-c(p[1],log(p[2]));z<-c(z,log(max(p[3]-1,1e-6)));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else 1+exp(z[j])
      }
      p
    }
    objective <- function(z) {
      if(any(!is.finite(z))||any(abs(z)>30))return(1e100)
      v<-suppressWarnings(sum(ld(x,decode(z))))
      if(is.finite(v)) -v else 1e100
    }
    # Starts depend on the sample and held values, not on the fitted result.
    values <- c()
    for(shift in c(-1,0,1)) {
      start<-held_values;start[1]<-median(x)+shift;start[2]<-max(.2,sd(x))*exp(shift/2)
      start[held]<-held_values[held]
      o<-suppressWarnings(optim(encode(start),objective,method="Nelder-Mead",
           control=list(maxit=1800,reltol=1e-11)))
      values<-c(values,o$value)
    }
    expect_gte(as.numeric(logLik(f)),-min(values)-2e-6*length(x))
  }
})


test_that("lst conditional collapse, feasibility and invalid holds", {
  expect_error(suppressWarnings(mllst(c(0,0,1),mu=0,df=1), classes = "univariateML_restricted"),"unbounded")
  expect_error(suppressWarnings(mllst(c(0,1),mu=0,df=1), classes = "univariateML_restricted"),"boundary")
  expect_error(suppressWarnings(mllst(c(0,0,1),mu=0), classes = "univariateML_restricted"),"unbounded")
  expect_true(is.finite(logLik(suppressWarnings(mllst(rep(2,4),mu=0,df=3), classes = "univariateML_restricted"))))
  expect_equal(unname(suppressWarnings(mllst(rep(2,4),sigma=1,df=3), classes = "univariateML_restricted")["mu"]),2,tolerance=1e-7)
  expect_error(suppressWarnings(mllst(qnorm((1:100-.5)/100),mu=0), classes = "univariateML_restricted"),"mlnorm")
  expect_error(suppressWarnings(mllst(1:5,mu=Inf), classes = "univariateML_restricted"),"mu")
  expect_error(suppressWarnings(mllst(1:5,sigma=0), classes = "univariateML_restricted"),"sigma")
})


test_that("location-scale t held-location half ties compare all eligible faces", {
  # The conditional normal face beats the held-location Cauchy collapse.
  expect_error(suppressWarnings(mllst(c(0,1),mu=0), classes = "univariateML_restricted"),"unbounded")
})


test_that("lst held profiles preserve tiny and large units", {
  x<-c(-2,-.5,0,.7,3)
  args<-list(mu=0,df=1e20)
  base<-do.call(mllst,c(list(x=x),args))
  for(unit in c(1e-300,1e300)) {
    fit<-do.call(mllst,c(list(x=x*unit),args))
    expect_equal(unname(fit["sigma"])/unit,unname(base["sigma"]),tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit))+length(x)*log(unit),as.numeric(logLik(base)),tolerance=1e-8)
  }
  expect_equal(unname(base["sigma"]),sqrt(mean(x^2)),tolerance=1e-8)
})
