context("mlgumbel")

## Data generation.
set.seed(313)
small_data <- rgumbel(n = 100, mu = 1, sigma = 1)
tiny_data <- rgumbel(n = 10, mu = 3, sigma = 7)
medium_data <- rgumbel(n = 1000, mu = 9, sigma = 11)

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(dgumbel(x = small_data, mu = p[1], sigma = p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- nlm(function(p) {
  -sum(dgumbel(x = tiny_data, mu = p[1], sigma = p[2], log = TRUE))
}, p = c(3, 7))

mle3 <- nlm(function(p) {
  -sum(dgumbel(x = medium_data, mu = p[1], sigma = p[2], log = TRUE))
}, p = c(9, 11))

expect_equal(mle1$estimate, as.numeric(mlgumbel(small_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlgumbel(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlgumbel(medium_data)),
  tolerance = 1e-5
)

expect_equal(-mle1$minimum, attr(mlgumbel(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlgumbel(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlgumbel(medium_data), "logLik"),
  tolerance = 1e-5
)

## Checks warning with small iterlim.
expect_error(mlgumbel(tiny_data, iterlim = 1), "IterationFailure|failed to converge")

## Finds errors with na and data out of bounds.
expect_error(mlgumbel(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlgumbel(small_data)),
  coef(mlgumbel(c(small_data, NA), na.rm = TRUE))
)

est <- mlgumbel(tiny_data)
## Check class.
expect_equal(attr(est, "model"), "Gumbel")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("gumbel handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(rgumbel(n = 199, mu = 0, sigma = 1), 1e3)
  objective <- function(p) -sum(dgumbel(x = x, mu = p[1], sigma = exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlgumbel(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant data have no attained gumbel MLE", {
  expect_error(mlgumbel(rep(2, 5)), "does not exist")
  expect_error(mlgumbel(2), "does not exist")
})

test_that("Gumbel variance survives a large offset", {
  x <- c(-2, -1, 0, 1, 3)
  f <- mlgumbel(x); g <- mlgumbel(x + 1e8)
  expect_equal(g[["sigma"]], f[["sigma"]], tolerance = 1e-6)
  expect_equal(attr(g, "logLik"), attr(f, "logLik"), tolerance = 1e-6)
})

test_that("Gumbel fitting respects extreme scale changes", {
  x <- c(-2, -1, 0, 1, 3)
  f <- mlgumbel(x)
  for (s in c(1e-300, 1e300)) {
    g <- mlgumbel(x*s)
    expect_equal(as.numeric(g)/s, as.numeric(f), tolerance=1e-4)
    expect_equal(attr(g,"logLik")+length(x)*log(s), attr(f,"logLik"), tolerance=1e-7)
  }
})


test_that("gumbel new holds agree with independent multistart likelihoods", {
  set.seed(8100+2925)
  held_values <- c(mu=0.2,sigma=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dgumbel(x = x, mu = p[1], sigma = p[2], log = TRUE)
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlgumbel,c(list(x=x),args))),error=identity)
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
    encode <- function(p) {z<-c(p[1],log(p[2]));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else exp(z[j])
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


test_that("gumbel conditional collapse, feasibility and invalid holds", {
  expect_equal(as.numeric(mlgumbel(rep(2,3),sigma=1)),c(2,1))
  expect_true(is.finite(logLik(mlgumbel(rep(2,3),mu=0))))
  expect_error(mlgumbel(rep(2,3),mu=2),"unbounded")
  expect_error(mlgumbel(1:4,mu=Inf),"finite")
  expect_error(mlgumbel(1:4,sigma=0),"positive")
})

test_that("gumbel: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "gumbel", "golden.json"))
  x <- g$data
  expect_error(mlgumbel(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("gumbel: P3 affine rescue preserves Newton failure", {
  expect_error(mlgumbel(c(1,2,4,8)*1e200,iterlim=1), "IterationFailure")
})
