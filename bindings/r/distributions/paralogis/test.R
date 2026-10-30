context("mlparalogis")

## Data generation (shape/scale parameterisation).
set.seed(313)
small_data <- rparalogis(n = 100, shape = 2, scale = 3)
medium_data <- rparalogis(n = 1000, shape = 0.8, scale = 0.5)

oracle <- function(x) {
  nll <- function(p) -sum(dparalogis(x = x, shape = exp(p[1]), scale = exp(p[2]), log = TRUE))
  o <- optim(c(0, log(median(x))), nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))
  list(estimates = exp(o$par), logLik = -o$value)
}

## The fit matches a slow optimiser on the reference density.
for (x in list(small_data, medium_data)) {
  fit <- mlparalogis(x)
  o <- oracle(x)
  expect_gte(attr(fit, "logLik"), o$logLik - 1e-6)
  expect_equal(unname(coef(fit)), o$estimates, tolerance = 1e-3)
  expect_equal(attr(fit, "logLik"),
               sum(dparalogis(x = x, shape = coef(fit)[["shape"]], scale = coef(fit)[["scale"]], log = TRUE)))
}

## Both parameters are estimated (univariateML 1.x held the scale at 1).
expect_equal(names(coef(mlparalogis(small_data))), c("shape", "scale"))

## Control arguments: shape0, gradtol, iterlim.
fit_start <- mlparalogis(medium_data, shape0 = 2, gradtol = 1e-8)
expect_equal(attr(fit_start, "logLik"), attr(mlparalogis(medium_data, gradtol = 1e-8), "logLik"), tolerance = 1e-9)
expect_error(mlparalogis(medium_data, shape0 = 0), "`shape0` must be a positive finite number")

## Input checks.
expect_error(mlparalogis(c(small_data, NA)))
expect_error(mlparalogis(c(-1, small_data)))
expect_equal(attr(mlparalogis(small_data), "model"), "Paralogistic")


test_that("fixed scale agrees with reference-density oracle and records df", {
  for (s in c(.2, 1, 7)) {
    x <- c(.03, .1, .4, .7, 1, 2, 4, 12) * s
    fit <- mlparalogis(x, scale=s)
    objective <- function(p) -sum(dparalogis(x = x, shape = exp(p[1]), scale = s, log = TRUE))
    oracle <- optim(0, objective, method="BFGS",
                    control=list(reltol=1e-13, maxit=3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance=1e-9)
    expect_equal(as.numeric(fit[seq_len(1)]), exp(oracle$par), tolerance=2e-5)
    expect_identical(attr(fit, "fixed"), "scale")
    expect_equal(attr(logLik(fit), "df"), 1)
    expect_identical(as.numeric(fit["scale"]), s)
    expect_equal(AIC(fit), -2*attr(fit,"logLik")+2*1)
    for (bad in c(0, -1, Inf, NaN))
      expect_error(mlparalogis(x, scale=bad), "scale.*positive finite")
  }
})

test_that("fixed unit scale reproduces the historical golden", {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "Historical fixture needs the distribution source tree")
  g <- jsonlite::fromJSON(file.path(root, "paralogis", "fixed-scale-golden.json"))
  fit <- mlparalogis(g$data, scale=1)
  expect_equal(as.numeric(fit[seq_along(g$estimates)]),
               as.numeric(unlist(g$estimates)), tolerance=1e-9)
  expect_equal(attr(fit,"logLik"), g$logLik, tolerance=1e-10)
})

test_that("fixed-scale paralogistic fits even constant observations", {
  fit <- mlparalogis(rep(3,10),scale=3)
  expect_equal(as.numeric(fit[1]), 2/log(2), tolerance=1e-9)
})

test_that("constant free-scale paralogis data have an unbounded likelihood", {
  expect_error(mlparalogis(rep(2,5)),"does not exist")
})

test_that("paralogis large-sample fitting preserves likelihood under replication", {
  p<-((1:1000)-.5)/1000
  x<-qparalogis(p = p, shape = 1, scale = 1)
  base<-mlparalogis(x)
  f<-mlparalogis(rep(x,100))
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})


test_that("P2 paralogis holds agree with independent multistart reference likelihoods", {
  pars <- c(shape=2, scale=1)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dparalogis
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qparalogis, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlparalogis, c(list(x=x),args)), error=identity)
    always <- TRUE
    if(always) expect_false(inherits(fit,"error"),info=paste(length(x),paste(held,collapse=",")))
    if (inherits(fit,"error")) {
      expect_match(conditionMessage(fit), "[Ll]imit|[Uu]se `ml|[Uu]nbounded|[Nn]o MLE|[Nn]o verified|unresolved|failed|does not exist")
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(pars[held]))
    expect_true(all(held %in% attr(fit,"fixed")))
    objective <- function(z) {
      p <- pars; p[free] <- exp(z)
      value <- suppressWarnings(-sum(do.call(density,c(list(x=x,log=TRUE),as.list(p)))))
      if(is.finite(value)) value else 1e100
    }
    actual <- attr(fit,"logLik")
    expect_equal(actual, -objective(log(fit[free])), tolerance=1e-7)
    if(length(free)) {
      # Starts depend only on reference parameters and observations.
      starts <- lapply(c(-1,0,1), function(s) log(pars[free])+s)
      oracle <- vapply(starts,function(z) {
        o <- suppressWarnings(nlminb(z,objective,control=list(iter.max=1000,eval.max=3000,rel.tol=1e-12)))
        -o$objective
      },0.)
      expect_gte(actual, max(oracle)-1e-6*length(x))
      expect_lte(actual, max(oracle)+1e-5*length(x))
    }
  }
})

test_that("P2 paralogis validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlparalogis(c(.2,.3,.5), shape=v),"positive|finite")
})

 test_that("P2 paralogis held shape fits constant observations", {
   for(args in list(list(shape=2),list(shape=2,scale=3))) {
     f <- do.call(mlparalogis,c(list(x=rep(2,5)),args))
     expect_identical(unname(f["shape"]),2)
     expect_true(is.finite(attr(f,"logLik")))
   }
 })

test_that("paralogis: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "paralogis", "golden.json"))
  x <- g$data
  expect_error(mlparalogis(x, iterlim=1, gradtol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("paralogis: P3 held profile exhaustion stops", {
  expect_error(mlparalogis(c(.3,.8,1.7,4.2),shape=2,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})
