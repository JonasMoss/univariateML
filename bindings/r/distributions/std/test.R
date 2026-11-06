context("mlstd")

## Data generation.
set.seed(313)
small_data <- rstd(n = 500, mean = 2, sd = 3, nu = 4)
tiny_data <- rstd(n = 100, mean = 2, sd = 3, nu = 4)

## Finds errors with na and data out of bounds.
expect_error(mlstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlstd(small_data)),
  coef(mlstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlstd(small_data, na.rm = TRUE)
expect_equal(
  sum(dstd(x = small_data, mean = est[1], sd = est[2], nu = est[3], log = TRUE)),
  attr(est, "logLik")
)

## Are the fallback methods working correctly?
expect_equal(
  log(dml(small_data, est)),
  dml(small_data, est, log = TRUE)
)
expect_equal(
  log(pml(small_data, est)),
  pml(small_data, est, log.p = TRUE)
)
expect_equal(
  1 - pml(small_data, est),
  pml(small_data, est, lower.tail = FALSE)
)
expect_equal(
  qml(1:3 / 4, est),
  qml(log(1:3 / 4), est, log.p = TRUE)
)
expect_equal(
  qml(1 - 1:3 / 4, est),
  qml(1:3 / 4, est, lower.tail = FALSE)
)


## Check class.
expect_equal(attr(est, "model"), "Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("Student-t escapes name the limiting model", {
  set.seed(1)
  expect_error(mlstd(rnorm(200)), "Use `mlnorm`", fixed = TRUE)
  set.seed(313)
  expect_error(mlstd(rstd(n = 100, mean = 2, sd = 3, nu = 4)),
               "nu -> 2 (infinite variance)", fixed = TRUE)
  x <- c(seq(-1e-4, 1e-4, length = 30),
         seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "nu -> 2 (infinite variance)", fixed = TRUE)
})

test_that("Student-t interior fit and large-nu log likelihood agree with independent dt", {
  set.seed(313)
  x <- rstd(n = 500, mean = 0, sd = 1, nu = 4)
  fit <- mlstd(x)
  expect_equal(attr(fit, "logLik"),
               sum(dstd(x = x, mean = fit[1], sd = fit[2], nu = fit[3], log = TRUE)),
               tolerance = 1e-10)
  for (nu in c(10, 100, 343, 344, 1000, 9999, 1e4, 1e5, 1e6, 1e7)) {
    expect_equal(univariateML:::student_t_loglik_cpp(x, 0.3, 1.2, nu),
                 sum(dt((x - 0.3) / (1.2 * sqrt((nu-2)/nu)), nu, log = TRUE) -
                       log(1.2 * sqrt((nu-2)/nu))),
                 tolerance = 1e-10)
  }
})

test_that("large-nu interior fit matches dt and a slow oracle", {
  skip_unless_tier("full")
  x <- qt(((1:4000) - .5)/4000, df = 300)
  fit <- tryCatch(mlstd(x, gradtol = 1e-7, iterlim = 500),error=identity)
  if(inherits(fit,"error")) {
    # The shallow root is isolated, but exhaustive exterior exclusion can
    # exceed the fixed certificate budget. Never accept its local score alone.
    expect_s3_class(fit,"univariateML_budget")
    expect_true(nrow(fit$candidates)>0)
    expect_true(any(fit$candidates$model=="lst"))
    expect_true(is.finite(fit$loglik_lower))
    expect_identical(fit$loglik_upper,Inf)
    return(invisible(NULL))
  }
  expect_gt(unname(fit[3]), 343)
  objective <- function(p) {
    nu <- 2 + exp(p[3]); s <- exp(p[2])*sqrt((nu-2)/nu)
    -sum(dt((x-p[1])/s, nu, log = TRUE) - log(s))
  }
  p <- c(fit[1], log(fit[2]), log(fit[3]-2))
  expect_equal(attr(fit, "logLik"), -objective(p), tolerance = 1e-11)
  oracles <- lapply(c(100, 500, 2000), function(nu)
    optim(c(0, 0, log(nu-2)), objective, method = "Nelder-Mead",
          control = list(maxit = 3000, reltol = 1e-13)))
  best <- oracles[[which.min(vapply(oracles, `[[`, 0, "value"))]]
  expect_lt(abs(attr(fit, "logLik") + best$value), 1e-5)
  expect_equal(unname(fit[3]), 2 + exp(best$par[3]), tolerance = .02)
})

test_that("fixed nu agrees with density oracle and records two free parameters", {
  skip_unless_tier("full")
  x <- c(-5,-2,-1,0,0,1,2,9)
  for (nu in c(2.1,4,30)) {
    fit <- mlstd(x, nu = nu, gradtol = 1e-8, iterlim = 5000)
    objective <- function(p) -sum(dstd(x = x, mean = p[1], sd = exp(p[2]), nu = nu, log = TRUE))
    oracle <- optim(c(0,1), objective, method = "BFGS",
                    control = list(reltol = 1e-13, maxit = 3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-9)
    expect_equal(as.numeric(fit[1:2]), c(oracle$par[1],exp(oracle$par[2])), tolerance = 1e-4)
    expect_identical(attr(fit, "fixed"), "nu")
    expect_equal(attr(logLik(fit), "df"), 2)
    expect_identical(unname(fit[3]), nu)
  }
  for (nu in c(-1,0,2,Inf,NaN)) expect_error(mlstd(x, nu = nu), "greater than 2")
  expect_error(mlstd(c(0,0,0,1), nu = 2.5), "unbounded")
  expect_error(mlstd(c(0,0,0,1), nu = 3), "equality")
  expect_error(mlstd(x, nu = 4, iterlim = 0), "score tolerance")
  expect_s3_class(mlstd(qnorm((1:100-.5)/100), nu = 4), "univariateML")
})

test_that("std scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- ml_simulate("mlstd",50)
  base <- tryCatch(mlstd(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlstd(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("std large-sample fitting preserves likelihood under replication", {
  skip_unless_tier("full")
  x<-qt(((1:1000)-.5)/1000,df=4)/sqrt(2)
  base<-mlstd(x)
  y<-rep(x,100)
  f<-mlstd(y)
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

test_that("free Student t detects strict tie collapse and separates equality", {
  expect_error(mlstd(c(rep(0,8),1,2,3)),"unbounded.*multiplicity")
  for (k in c(5L,6L,7L)) {
    x <- c(rep(0,k),1,2,3)
    if(k==7L) expect_error(mlstd(x),"unbounded")
    if(k==6L) expect_error(mlstd(x),"equality.*not certified")
    if(k==5L) {
      f <- tryCatch(mlstd(x),error=identity)
      if(inherits(f,"error")) expect_false(grepl("unbounded|equality",conditionMessage(f)))
      else expect_true(all(is.finite(f)))
    }
  }
  expect_error(mlstd(c(rep(0,8),1,2,3),nu=2.5),"unbounded")
  expect_error(mlstd(c(rep(0,8),1,2,3),nu=8/3),"equality")
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("std dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 5)
  d <- function(x, ...) do.call(dstd, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(pstd, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qstd, c(list(p), pars, list(...)))
  for (x in c(-2, .3, 3)) {
    expect_equal(p(x), integrate(d, -Inf, x, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
    expect_equal(p(x, lower.tail = FALSE),
                 integrate(d, x, Inf, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
  }
  expect_equal(integrate(d, -Inf, Inf, rel.tol = 1e-9, subdivisions = 1000L)$value,
               1, tolerance = 2e-9)
  for (lower in c(TRUE, FALSE)) {
    lp <- c(-1000, -700, -100, -30, -2, -.1, -1e-12)
    expect_equal(p(q(lp, lower.tail = lower, log.p = TRUE), lower.tail = lower,
                   log.p = TRUE), lp, tolerance = 2e-11)
    prob <- c(1e-100, 1e-20, .01, .5, .99)
    expect_equal(p(q(prob, lower.tail = lower), lower.tail = lower), prob,
                 tolerance = 2e-12)
  }
  expect_equal(d(c(-Inf, Inf)), c(0, 0))
  expect_equal(d(c(-Inf, Inf), log = TRUE), c(-Inf, -Inf))
  expect_equal(p(c(-Inf, Inf)), c(0, 1))
  expect_equal(q(c(0, 1)), c(-Inf, Inf))
  expect_equal(q(c(-Inf, 0), log.p = TRUE), c(-Inf, Inf))
  expect_true(all(is.finite(d(c(-1000, 1000), log = TRUE))))
})

test_that("std dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dstd(x = numeric(), mean = 0, sd = 1, nu = 5), 0)
  expect_equal(dstd(x = c(0, 1, 2), mean = c(0, 1), sd = 1, nu = 5),
    c(dstd(x = 0, mean = 0, sd = 1, nu = 5), dstd(x = 1, mean = 1, sd = 1, nu = 5), dstd(x = 2, mean = 0, sd = 1, nu = 5)))
  expect_identical(dstd(x = NA_real_, mean = 0, sd = 1, nu = 5), NA_real_)
  expect_true(is.nan(pstd(q = NaN, mean = 0, sd = 1, nu = 5)))
  expect_warning(expect_true(is.nan(dstd(x = 0, mean = 0, sd = -1, nu = 5))), "NaNs produced")
  expect_warning(expect_true(is.nan(qstd(p = 1.1, mean = 0, sd = 1, nu = 5))), "NaNs produced")
  expect_warning(expect_true(is.nan(rstd(n = 1, mean = 0, sd = -1, nu = 5))), "NaNs produced")
  set.seed(81); first <- rstd(n = 100, mean = 0, sd = 1, nu = 5)
  set.seed(81); expect_identical(rstd(n = 100, mean = 0, sd = 1, nu = 5), first)
  expect_length(rstd(n = 1:7, mean = 0, sd = 1, nu = 5), 7)
  expect_length(rstd(n = 0, mean = 0, sd = 1, nu = 5), 0)
  expect_length(rstd(n = 3, mean = numeric(), sd = 1, nu = 5), 0)
  expect_identical(rstd(n = 1, mean = NA_real_, sd = 1, nu = 5), NA_real_)
  set.seed(712)
  draws <- rstd(n = 5000, mean = 0, sd = 1, nu = 5)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qstd(p = prob, mean = 0, sd = 1, nu = 5)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dstd(x = 0, mean = 0, sd = bad, nu = 5))), "NaNs produced")
    expect_warning(expect_true(is.nan(pstd(q = 0, mean = 0, sd = bad, nu = 5))), "NaNs produced")
    expect_warning(expect_true(is.nan(qstd(p = 0.5, mean = 0, sd = bad, nu = 5))), "NaNs produced")
  }
  fit <- mlstd(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(pstd, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qstd, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rstd, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("std matches frozen fGarch values (reference.json)", {
  expect_reference_values("std")
})

test_that("std log density survives the fGarch underflow defect", {
  expected <- dt(1e6*sqrt(1e6/(1e6-2)), df = 1e6, log = TRUE) +
    log(sqrt(1e6/(1e6-2)))
  expect_true(is.finite(dstd(x = 1e+06, mean = 0, sd = 1, nu = 1e+06, log = TRUE)))
  expect_equal(dstd(x = 1e+06, mean = 0, sd = 1, nu = 1e+06, log = TRUE), expected, tolerance = 1e-13)
})

test_that("std parameter domain is enforced", {
  for (bad in c(-1, 0, 2, Inf))
    expect_warning(expect_true(is.nan(dstd(x = 0, mean = 0, sd = 1, nu = bad))), "NaNs produced")
})

test_that("std log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qstd(p = -20000, mean = 0, sd = 1, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(pstd(q = x, mean = 0, sd = 1, nu = 1000, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qstd(p = -1e-30, mean = 0, sd = 1, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_equal(pstd(q = x, mean = 0, sd = 1, nu = 1000, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})


test_that("std new holds agree with independent multistart likelihoods", {
  skip_unless_tier("full")
  set.seed(8100+9553)
  held_values <- c(mean=0.2,sd=1.3,nu=5)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dt((x-p[1])/p[2]*sqrt(p[3]/(p[3]-2)),df=p[3],log=TRUE)+.5*log(p[3]/(p[3]-2))-log(p[2])
  for(x in samples)for(mask in 1:7) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlstd,c(list(x=x),args))),error=identity)
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
    encode <- function(p) {z<-c(p[1],log(p[2]));z<-c(z,log(p[3]-2));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else 2+exp(z[j])
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


test_that("std conditional collapse, feasibility and invalid holds", {
  expect_error(mlstd(c(0,0,0,1),mean=0,nu=2.5),"unbounded")
  expect_error(mlstd(c(0,0,0,1),mean=0,sd=1),"unbounded")
  expect_error(mlstd(c(0,0,1),mean=0,nu=2),"nu|boundary|unbounded|converge")
  expect_true(is.finite(logLik(mlstd(rep(2,4),mean=0,nu=4))))
  expect_equal(unname(mlstd(rep(2,4),sd=1,nu=4)["mean"]),2,tolerance=1e-7)
  expect_error(mlstd(qnorm((1:100-.5)/100),mean=0),"mlnorm")
  expect_error(mlstd(1:5,mean=Inf),"mean")
  expect_error(mlstd(1:5,sd=0),"sd")
})


test_that("Student-t conditional lower shape faces respect held mean and sd", {
  x<-c(-20,rep(-.1,10),rep(.1,10),20)
  expect_error(mlstd(x,mean=0),"mllst")
  expect_error(mlstd(c(0,0,1),mean=0,sd=1),"boundary")
})


test_that("std held profiles preserve tiny and large units", {
  x<-c(-2,-.5,0,.7,3)
  args<-list(mean=0,nu=1e20)
  base<-do.call(mlstd,c(list(x=x),args))
  for(unit in c(1e-300,1e300)) {
    fit<-do.call(mlstd,c(list(x=x*unit),args))
    expect_equal(unname(fit["sd"])/unit,unname(base["sd"]),tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit))+length(x)*log(unit),as.numeric(logLik(base)),tolerance=1e-8)
  }
  expect_equal(unname(base["sd"]),sqrt(mean(x^2)),tolerance=1e-8)
})

test_that("std: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "std", "golden.json"))
  x <- g$data
  expect_error(mlstd(x, iterlim=1, gradtol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("std certificate reports normal and collapse separately", {
  normal <- tryCatch(mlstd(c(-1,-1,1,1)), error=identity)
  expect_s3_class(normal,"univariateML_limit")
  expect_identical(normal$model,"norm")
  expect_equal(unname(normal$estimates),c(0,1))
  collapse <- tryCatch(mlstd(c(-1,0,0,0,0,1)), error=identity)
  expect_s3_class(collapse,"univariateML_nomle")
  expect_identical(collapse$model,"t scale collapse (no package family)")
  expect_equal(unname(collapse$estimates),c(0,2))
  expect_lte(collapse$loglik_lower,-6*log(2))
  expect_gte(collapse$loglik_upper,-6*log(2))
})

test_that("std certificate t2 stops use ordinary scale coordinates", {
  x <- c(seq(-1e-4,1e-4,length.out=30),seq(.9999,1.0001,length.out=10))
  err <- tryCatch(mlstd(x),error=identity)
  expect_s3_class(err,"univariateML_limit")
  expect_identical(err$model,"lst")
  expect_identical(names(err$estimates),c("mu","sigma","df"))
  expect_equal(unname(err$estimates["df"]),2)
  expect_true(is.finite(err$estimates["sigma"])&&err$estimates["sigma"]>0)
})

# Frozen TASK-140 sample: a finite root near nu=218 is enclosed, but the
# exterior profile comparison exhausts its budget. No RNG or refit of a limit.
test_that("std certificate budget is a terminal structured stop", {
  x <- c(
    0.050954662455960954,-0.41009991819347347,-0.27514948570271125,1.738894665940981,
    0.78279298304888556,-0.60948854075862469,0.16676804415486185,2.1605246276102874,
    -0.2387465089482371,-2.306020779595189,0.77127783907401493,0.86844496163735896,
    1.0920372377409862,-0.64839860768648283,0.46991123089786935,-0.42877220249733083,
    -0.67119918382727128,-0.022625495739689489,-1.4089791730105745,0.030657182531391439,
    0.73948383168474352,-0.91882017162685425,-0.98242394131721511,-1.2717118121176605,
    0.70728273289689891,1.1005705147845657,-0.31552546144261623,0.24877221879696262,
    -0.76383269913190954,-0.88392850638958176,0.6899451665252806,-0.069740288243165541,
    -0.49784164823829802,0.83814137621338658,-0.26457437687827884,-0.38679889567518655,
    -0.1278879326472602,-0.061343959699808992,-1.9342834627670904,-0.96501945943185763,
    0.12782341531106875,-0.53974820532464285,1.393079864160065,-1.8866780642217882,
    0.48159934707033075,0.37619107597635915,-0.7842379492323549,0.16661327780961455,
    0.065702578103739015,1.5033378387590548,-1.5584653425173214,0.55755841451860877,
    -0.77335681661524291,0.17687773844287813,-0.45347896200121202,-2.6420087840828708,
    -2.0359122776319354,0.40101402508509254,2.6373222314115758,-1.2594962766931357,
    0.896216299275718,-1.1247597997904279,-0.31742515720096587,-0.39504235064336413,
    2.0094231968658294,0.63778178928263718,1.362962386207389,0.48996014406736171,
    0.71843285483593033,0.7179914812225302,0.61612365065267904,0.2661075130421014,
    -0.65853415821813321,-1.0128687552568489,-0.59930246600735593,1.0810268064227586,
    0.56795583829798524,1.3096494218560304,0.85408895018114128,-1.3334550306342379
  )
  err <- tryCatch(mlstd(x),error=identity)
  expect_s3_class(err,"univariateML_budget")
  expect_true(any(err$candidates$model=="lst"))
  expect_true(is.finite(err$loglik_lower))
  expect_identical(err$loglik_upper,Inf)
  expect_length(err$estimates,0)
})
