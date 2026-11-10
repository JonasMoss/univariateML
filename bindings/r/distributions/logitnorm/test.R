context("mllogitnorm")

## Data generation.
set.seed(313)
small_data <- plogis(rnorm(100, 2, 3))
tiny_data <- plogis(rnorm(10, 0, 1))

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllogitnorm(small_data)),
  coef(mllogitnorm(c(small_data, NA), na.rm = TRUE))
)

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(0, tiny_data)))
expect_error(mllogitnorm(c(tiny_data, 1)))

## Is the log-likelihood correct?
est <- mllogitnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dlogitnorm(x = small_data, mu = est[1], sigma = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Logit-normal")
expect_equal(class(est), c("univariateML"))


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained logitnorm MLE", {
  expect_error(mllogitnorm(rep(.4, 5)), "does not exist")
  expect_error(mllogitnorm(.4), "does not exist")
})

 test_that("logitnorm owned dpqr obeys transformations and R conventions", {
  x <- plogis(seq(-6, 6, length.out = 31))
  a <- -.3; b <- 2.1
  expect_equal(dlogitnorm(x = x, mu = a, sigma = b), dnorm(qlogis(x),a,b)/(x*(1-x)), tolerance=1e-12)
  expect_equal(integrate(function(x) dlogitnorm(x = x, mu = a, sigma = b), 0, 1, rel.tol=1e-10, subdivisions=1000)$value, 1, tolerance=1e-7)
  for (lower in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
    pr <- plogitnorm(q = x, mu = a, sigma = b, lower.tail = lower, log.p = lg)
    expect_equal(pr, pnorm(qlogis(x),a,b,lower.tail=lower,log.p=lg), tolerance=1e-11)
    expect_equal(qlogitnorm(p = pr, mu = a, sigma = b, lower.tail = lower, log.p = lg), x, tolerance=1e-7)
  }
  expect_equal(plogitnorm(q = c(-Inf, -1, 0, 1, Inf), mu = a, sigma = b), c(0,0,0,1,1))
  expect_equal(dlogitnorm(x = c(-Inf, -1, Inf), mu = a, sigma = b), c(0,0,0))
  expect_equal(qlogitnorm(p = c(-Inf, 0), mu = a, sigma = b, log.p = TRUE), c(0,1))
  expect_equal(dlogitnorm(x = x, mu = a, sigma = b, log = TRUE), log(dlogitnorm(x = x, mu = a, sigma = b)))
  expect_length(dlogitnorm(x = numeric(), mu = a, sigma = b),0)
  expect_equal(dlogitnorm(x = x[1:4], mu = c(a, a + 1), sigma = b), vapply(1:4,function(i) dlogitnorm(x = x[i], mu = c(a, a + 1)[(i - 1)%%2 + 1], sigma = b),0.0))
  expect_identical(dlogitnorm(x = c(NA_real_, NaN), mu = a, sigma = b),c(NA_real_,NaN))
  expect_warning(expect_true(is.nan(dlogitnorm(x = 0.5, mu = a, sigma = -1))),"NaNs")
  expect_warning(expect_true(is.nan(qlogitnorm(p = 2, mu = a, sigma = b))),"NaNs")
  set.seed(42); r1 <- rlogitnorm(n = 20, mu = a, sigma = b)
  set.seed(42); expect_equal(r1,plogis(rnorm(20,a,b)),tolerance=1e-14)
  set.seed(42); expect_identical(rlogitnorm(n = 20, mu = a, sigma = b),r1)
  expect_length(rlogitnorm(n = 1:3, mu = a, sigma = b),3)
  fit <- mllogitnorm(r1)
  expect_equal(as.numeric(logLik(fit)),sum(dml(r1,fit,log=TRUE)))


  set.seed(4); sim <- rml(5,fit)
  set.seed(4); expect_equal(sim,rlogitnorm(n = 5, mu = fit[1], sigma = fit[2]))
 })
 test_that("logitnorm matches frozen logitnorm values (reference.json)", {
   expect_reference_values("logitnorm")
 })

test_that("logitnorm extreme normal log tails survive inversion", {
  for (lower in c(TRUE,FALSE)) {
    p <- c(-1000,-10000,-1e-20)
    x <- qlogitnorm(p = p, mu = 0, sigma = 0.01, lower.tail = lower, log.p = TRUE)
    expect_equal(plogitnorm(q = x, mu = 0, sigma = 0.01, lower.tail = lower, log.p = TRUE),p,tolerance=1e-10)
  }
})

test_that("logitnorm integrates with plot, bootstrap and model selection", {
  set.seed(19)
  fit <- mllogitnorm(rlogitnorm(n = 80, mu = 1, sigma = 2))
  expect_true(all(is.finite(bootstrapml(fit,reps=4))))
  expect_s3_class(model_select(rlogitnorm(n = 80, mu = 1, sigma = 2),models="logitnorm"),"univariateML")
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) })
  for (kind in c("d","p","q")) expect_invisible(plot(fit,kind=kind))
})

test_that("P2 every held subset agrees with independent multistart density fits", {
  skip_unless_tier("full")
  set.seed(8129)
  samples <- list(plogis(rnorm(20,.3,1.2)), plogis(rnorm(100,.3,1.2)),
                  runif(20,.01,.99), runif(100,.01,.99),
                  c(.2,.2,.6,.8), c(.2,.8), rep(.4,5), .4,
                  c(1e-5, .01, .5, .99, 1-1e-5))
  values <- c(mu=0.3, sigma=1.2)
  for (values in list(c(mu=.3,sigma=1.2),c(mu=-2,sigma=.3))) {
  for (x in samples) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    free <- setdiff(names(values),held)
    fit <- do.call(mllogitnorm,c(list(x=x),as.list(values[held])))
    expect_identical(attr(fit,"fixed"),held)
    expect_identical(unname(coef(fit)[held]),unname(values[held]))
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    ll <- function(p) suppressWarnings(sum(dlogitnorm(x = x, mu = p[1], sigma = p[2], log = TRUE)))
    expect_equal(as.numeric(logLik(fit)),ll(coef(fit)),tolerance=1e-9)
    if (length(free)) {
      objective <- function(z) {
        p <- values
        p[free] <- if (free == "mu") z else exp(z)
        v <- -ll(p)
        if (is.finite(v)) v else 1e100
      }
      refs <- vapply(c(-3,0,3), function(start)
        nlminb(start,objective,control=list(iter.max=1000,rel.tol=1e-13))$objective,0.0)
      expect_lt(abs(as.numeric(logLik(fit))+min(refs)),1e-6*length(x))
    }
  }
  }
  for (p in names(values)) for (bad in c(NA_real_,Inf,-Inf,NaN))
    expect_error(do.call(mllogitnorm,c(list(x=c(.2,.8)),setNames(list(bad),p))))
  for (edge in c(0,1)) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    expect_error(do.call(mllogitnorm,c(list(x=c(edge,.4)),as.list(values[held]))))
  }
})

test_that("P2 held logit location collapse and held scale constant fits", {
  expect_error(mllogitnorm(rep(.5,5),mu=0),"does not exist")
  expect_true(is.finite(as.numeric(logLik(mllogitnorm(rep(.5,5),mu=1)))))
  expect_equal(unname(mllogitnorm(rep(.5,5),sigma=2)[1]),0)
  for (bad in c(0,-1)) expect_error(mllogitnorm(c(.2,.8),sigma=bad),"positive")
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 4748", {
  x <- c(0.2499999999995, 0.24999999999975, 0.25, 0.25000000000025, 0.25000000000075)
  fit <- mllogitnorm(x)
  expect_lte(abs(unname(fit["mu"]) / (-1.0986122886678429) - 1), 1e-10)
  expect_lte(abs(unname(fit["sigma"]) / (2.293964607844937e-12) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (135.2788871292404)) / max(1, abs(135.2788871292404)), 1e-10)
})
test_that("raw transform audit case 4745", {
  x <- c(0.8401216942096138, 0.8401216942104539, 0.840121694211294, 0.8401216942121341, 0.8401216942138143)
  fit <- mllogitnorm(x)
  expect_lte(abs(unname(fit["mu"]) / (1.6591338183876283) - 1), 1e-10)
  expect_lte(abs(unname(fit["sigma"]) / (1.0760887283831534e-11) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (129.21857717804238)) / max(1, abs(129.21857717804238)), 1e-10)
})
test_that("raw transform audit case 4747", {
  x <- c(0.249999995, 0.2499999975, 0.25, 0.2500000025, 0.2500000075)
  fit <- mllogitnorm(x)
  expect_lte(abs(unname(fit["mu"]) / (-1.0986122860014431) - 1), 1e-10)
  expect_lte(abs(unname(fit["sigma"]) / (2.2939533978240636e-08) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (89.22720969647992)) / max(1, abs(89.22720969647992)), 1e-10)
})
test_that("raw transform audit case 4744", {
  x <- c(0.8401216774088601, 0.8401216858100771, 0.840121694211294, 0.840121702612511, 0.8401217194149448)
  fit <- mllogitnorm(x)
  expect_lte(abs(unname(fit["mu"]) / (1.6591338308958958) - 1), 1e-10)
  expect_lte(abs(unname(fit["sigma"]) / (1.0761091599928096e-07) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (83.16678042701042)) / max(1, abs(83.16678042701042)), 1e-10)
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 9806", {
  x <- c(0.49999956708390103, 0.49999971241265495, 0.49999979694554963, 0.49999986286942943, 0.4999999203401591, 0.49999997384163614, 0.5000000261583639, 0.500000079659841, 0.5000001371305707, 0.5000002030544504, 0.500000287587345, 0.500000432916099)
  fit <- mllogitnorm(x)
  expect_lte(abs(unname(fit["mu"]) / (7.401486830834707e-17) - 1), 1e-10)
  expect_lte(abs(unname(fit["sigma"]) / (9.482456548773743e-07) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (166.03209679869155)) / max(1, abs(166.03209679869155)), 1e-10)
})
