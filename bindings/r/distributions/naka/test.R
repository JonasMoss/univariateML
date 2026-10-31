context("mlnakagami")

## Data generation.
set.seed(313)
small_data <- rnaka(n = 100, shape = 1, scale = 1)
tiny_data <- rnaka(n = 10, shape = 7, scale = 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(dnaka(x = small_data, shape = p[1], scale = p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(dnaka(x = tiny_data, shape = p[1], scale = p[2], log = TRUE))
}, p = c(7, 7))$estimate

expect_equal(mle1, as.numeric(mlnaka(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlnaka(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
## expect_warning(mlnaka(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlnaka(c(tiny_data, NA)))
expect_error(mlnaka(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlnaka(small_data)),
  coef(mlnaka(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlnaka(small_data, na.rm = TRUE)
expect_equal(
  sum(dnaka(x = small_data, shape = est[1], scale = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Nakagami")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained naka MLE", {
  expect_error(mlnaka(rep(2, 5)), "does not exist")
  expect_error(mlnaka(2), "does not exist")
})

test_that("squared-data underflow is a precision error, not constant data", {
  expect_error(mlnaka(c(1,2,3)*1e-300), "precision")
})

 test_that("naka owned dpqr obeys transformations and R conventions", {
  x <- seq(.01, 4, length.out = 31)
  a <- 1.7; b <- 2.1
  expect_equal(dnaka(x = x, shape = a, scale = b), 2*x*dgamma(x*x,a,scale=b/a), tolerance=1e-12)
  expect_equal(integrate(function(x) dnaka(x = x, shape = a, scale = b), 0, Inf, rel.tol=1e-10, subdivisions=1000)$value, 1, tolerance=1e-7)
  for (lower in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
    pr <- pnaka(q = x, shape = a, scale = b, lower.tail = lower, log.p = lg)
    expect_equal(pr, pgamma(x*x,a,scale=b/a,lower.tail=lower,log.p=lg), tolerance=1e-11)
    expect_equal(qnaka(p = pr, shape = a, scale = b, lower.tail = lower, log.p = lg), x, tolerance=1e-7)
  }
  expect_equal(pnaka(q = c(-Inf, -1, 0, Inf), shape = a, scale = b), c(0,0,0,1))
  expect_equal(dnaka(x = c(-Inf, -1, Inf), shape = a, scale = b), c(0,0,0))
  expect_equal(qnaka(p = c(-Inf, 0), shape = a, scale = b, log.p = TRUE), c(0,Inf))
  expect_equal(dnaka(x = x, shape = a, scale = b, log = TRUE), log(dnaka(x = x, shape = a, scale = b)))
  expect_length(dnaka(x = numeric(), shape = a, scale = b),0)
  expect_equal(dnaka(x = x[1:4], shape = c(a, a + 1), scale = b), vapply(1:4,function(i) dnaka(x = x[i], shape = c(a, a + 1)[(i - 1)%%2 + 1], scale = b),0.0))
  expect_identical(dnaka(x = c(NA_real_, NaN), shape = a, scale = b),c(NA_real_,NaN))
  expect_warning(expect_true(is.nan(dnaka(x = 0.5, shape = a, scale = -1))),"NaNs")
  expect_warning(expect_true(is.nan(qnaka(p = 2, shape = a, scale = b))),"NaNs")
  set.seed(42); r1 <- rnaka(n = 20, shape = a, scale = b)
  set.seed(42); expect_equal(r1,sqrt(rgamma(20,a,scale=b/a)),tolerance=1e-14)
  set.seed(42); expect_identical(rnaka(n = 20, shape = a, scale = b),r1)
  expect_length(rnaka(n = 1:3, shape = a, scale = b),3)
  fit <- mlnaka(r1)
  expect_equal(as.numeric(logLik(fit)),sum(dml(r1,fit,log=TRUE)))


  set.seed(4); sim <- rml(5,fit)
  set.seed(4); expect_equal(sim,rnaka(n = 5, shape = fit[1], scale = fit[2]))
 })
 test_that("naka matches frozen nakagami values (reference.json)", {
   expect_reference_values("naka")
 })

test_that("Nakagami boundary densities and extreme gamma tails are correct", {
  expect_equal(dnaka(x = 0, shape = 0.25, scale = 2),Inf)
  expect_equal(dnaka(x = 0, shape = 0.5, scale = 2),1/sqrt(pi))
  expect_equal(dnaka(x = 0, shape = 2, scale = 2),0)
  for (lower in c(TRUE,FALSE)) {
    p <- c(-1000,-10000,-1e-20)
    x <- qnaka(p = p, shape = 10, scale = 2, lower.tail = lower, log.p = TRUE)
    expect_equal(pnaka(q = x, shape = 10, scale = 2, lower.tail = lower, log.p = TRUE),p,tolerance=1e-10)
  }
})

test_that("naka integrates with plot, bootstrap and model selection", {
  set.seed(19)
  fit <- mlnaka(rnaka(n = 80, shape = 1, scale = 2))
  expect_true(all(is.finite(bootstrapml(fit,reps=4))))
  expect_s3_class(model_select(rnaka(n = 80, shape = 1, scale = 2),models="naka"),"univariateML")
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) })
  for (kind in c("d","p","q")) expect_invisible(plot(fit,kind=kind))
})

test_that("tiny Nakagami shape retains upper log tails at tiny amplitudes", {
  x <- 1e-200; shape <- 1e-25
  lx <- 2*log(x)+log(shape)
  expect_equal(pnaka(q = x, shape = shape, scale = 1, lower.tail = FALSE, log.p = TRUE),
               log(-expm1(shape*lx)),tolerance=1e-12)
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "naka"
  pn <- c("shape","scale")
  held_default <- c(2.0,3.0)
  names(held_default) <- pn
  density <- dnaka
  ref <- function(x,p) {
    m <- p[["shape"]]; o <- p[["scale"]]; log(2)+m*log(m)-lgamma(m)-m*log(o)+(2*m-1)*log(x)-m*x*x/o
  }
  samples <- list(n20=ml_simulate(paste0("ml",id),20),
                  n100=ml_simulate(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)
  samples$limit <- abs(rnorm(100))
  subsets <- lapply(1:(2^length(pn)-1),function(mask)
    pn[as.logical(intToBits(mask)[seq_along(pn)])])
  for (x in samples) for (held in subsets) {
    hv <- held_default
    args <- c(list(x=x),as.list(hv[held]))
    f <- do.call(get(paste0("ml",id)),args)
    expect_identical(unname(as.numeric(f[held])),unname(hv[held]))
    expect_setequal(attr(f,"fixed"),held)
    p <- setNames(as.numeric(f),pn)
    expect_equal(as.numeric(logLik(f)),sum(ref(x,p)),tolerance=1e-8)
    free <- setdiff(pn,held)
    decode <- function(z) {
      q <- hv
      q[free] <- exp(z)
      q
    }
    cost <- function(z) {
      if(any(!is.finite(z)) || any(abs(z)>40)) return(1e100)
      v <- suppressWarnings(-sum(ref(x,decode(z))))
      if(is.finite(v)) v else 1e100
    }
    if(length(free)) {
      start <- log(hv[free])
      oracles <- lapply(c(-3,-1,0,1,3),function(d)
        optim(start+d,cost,method="BFGS",control=list(maxit=1000,reltol=1e-12))$value)
      best <- min(unlist(oracles))
    } else best <- -sum(ref(x,hv))
    expect_equal(as.numeric(logLik(f)),-best,tolerance=1e-6)
  }
  for (p in pn) for (bad in c(NA_real_,NaN,Inf,-Inf,if(p=="meanlog") numeric() else c(0,-1))) {
    expect_error(do.call(get(paste0("ml",id)),c(list(x=samples$n20),setNames(list(bad),p))))
  }
})

test_that("P2 held existence boundaries", {
expect_error(mlnaka(rep(2,3),scale=4),"unbounded")
expect_true(is.finite(logLik(mlnaka(rep(2,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlnaka(c(2,3),scale=3,iterlim=1)))
})

test_that("naka: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "naka", "golden.json"))
  x <- g$data
  expect_error(mlnaka(x, iterlim=1, abstol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
