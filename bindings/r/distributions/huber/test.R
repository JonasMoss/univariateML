context("mlhuber")
set.seed(782)
x <- rhuber(n = 120, mu = 2, sigma = 3, epsilon = 1.345)
for (e in c(.5, 1.345, 3, 10)) {
  for (fm in c(FALSE, TRUE)) for (fs in c(FALSE, TRUE)) {
    args <- list(x = x, epsilon = e)
    if (fm) args$mu <- 1
    if (fs) args$sigma <- 2
    fit <- do.call(mlhuber, args)
    fixed <- c("epsilon", if (fm) "mu", if (fs) "sigma")
    expect_setequal(attr(fit, "fixed"), fixed)
    expect_equal(attr(logLik(fit), "df"), 2L - fm - fs)
    if (fm) expect_equal(unname(fit["mu"]), 1)
    if (fs) expect_equal(unname(fit["sigma"]), 2)
    obj <- function(p) {
      m <- if (fm) 1 else p[1]
      s <- if (fs) 2 else exp(p[length(p)])
      -sum(dhuber(x = x, mu = m, sigma = s, epsilon = e, log = TRUE))
    }
    if (!(fm && fs)) {
      vals <- sapply(c(-2, 0, 2), function(shift) {
        start <- c(if (!fm) median(x)+shift, if (!fs) log(sd(x))+shift/3)
        optim(start, obj, method = "BFGS", control = list(reltol = 1e-12))$value
      })
      expect_equal(-as.numeric(logLik(fit)), min(vals), tolerance = 1e-7)
    }
    expect_equal(as.numeric(logLik(fit)), sum(dhuber(x = x, mu = fit[1], sigma = fit[2], epsilon = e, log = TRUE)), tolerance=1e-10)
  }
}
# Direct clipped-score Huber location iteration at a held scale.
m <- median(x)
for (i in 1:1000) {
  z <- (x-m)/2
  w <- pmin(1, 1.345/abs(z))
  next_m <- sum(w*x)/sum(w)
  if (abs(next_m-m)<1e-12) break
  m <- next_m
}
expect_equal(unname(mlhuber(x, sigma=2, epsilon = 1.345)[1]), m, tolerance=1e-9)
expect_equal(unname(mlhuber(c(-10,10), sigma=1, epsilon = 1.345)[1]), 0)
expect_error(mlhuber(rep(1,5), epsilon = 1.345), "unbounded")
expect_error(mlhuber(rep(1,5), mu=1, epsilon = 1.345), "unbounded")
expect_true(is.finite(as.numeric(logLik(mlhuber(rep(1,5), mu=0, epsilon = 1.345)))))
expect_true(is.finite(as.numeric(logLik(mlhuber(rep(1,5), sigma=2, epsilon = 1.345)))))
for (bad in c(0,-1,Inf,NaN)) {
  expect_error(mlhuber(x, sigma=bad, epsilon = 1.345), "sigma")
  expect_error(mlhuber(x, epsilon=bad), "epsilon")
}
expect_error(mlhuber(x, mu=Inf, epsilon = 1.345), "mu")
expect_equal(as.numeric(mlhuber(x, epsilon=10)[1:2]), as.numeric(mlnorm(x)), tolerance=1e-7)
expect_error(mlhuber(c(1, Inf), epsilon = 1.345), "support")
expect_error(mlhuber(x, tol=0, epsilon = 1.345), "tol")
expect_error(mlhuber(x, iterlim=0, epsilon = 1.345), "iterlim")
expect_error(mlhuber(x, iterlim=1, tol=1e-15, epsilon = 1.345), "converge")
# Tied, nonconstant samples remain bounded and fit every free subset.
for (args in list(list(), list(mu=0), list(sigma=1), list(mu=0, sigma=1))) {
  fit <- do.call(mlhuber, c(list(x=c(0,0,0,1,1), epsilon=1.345), args))
  expect_true(is.finite(as.numeric(logLik(fit))))
}

# Free-shape fit versus an independent direct three-coordinate oracle.
z <- qhuber(p = ((1:160) - 0.5)/160, mu = 2, sigma = 3, epsilon = 0.8)
for (fm in c(FALSE, TRUE)) for (fs in c(FALSE, TRUE)) {
  args <- list(x=z, epsilon=NULL)
  if (fm) args$mu <- 2
  if (fs) args$sigma <- 3
  fit <- do.call(mlhuber, args)
  expect_setequal(attr(fit,"fixed"), c(character(), if(fm) "mu", if(fs) "sigma"))
  expect_equal(attr(logLik(fit),"df"), 3L-fm-fs)
  objective <- function(p) {
    j <- 1L
    m <- if(fm) 2 else {j <- j+1L; p[1]}
    s <- if(fs) 3 else {j <- j+1L; exp(p[j-1L])}
    -sum(dhuber(x = z, mu = m, sigma = s, epsilon = exp(p[j]), log = TRUE))
  }
  starts <- expand.grid(m=c(0,2,4), s=c(1,3), e=c(.2,.8,2))
  vals <- apply(starts,1,function(a) optim(c(if(!fm) a[1], if(!fs) log(a[2]),log(a[3])),
    objective,method=if(fm && fs) "BFGS" else "Nelder-Mead",control=list(maxit=3000,reltol=1e-12))$value)
  expect_equal(-as.numeric(logLik(fit)),min(vals),tolerance=1e-7)
}
expect_error(mlhuber(c(-1,1),epsilon=NULL),"use `mlnorm`")
expect_error(mlhuber(c(-10,-.1,0,.1,10),epsilon=NULL),"use `mllaplace`")
# A held scale excludes the rescaled Laplace face.
expect_error(mlhuber(c(-1,1),sigma=1,epsilon=NULL),"use `mlnorm`")
cutoff <- unname(mlhuber(z,epsilon=NULL)["epsilon"])
expect_equal(cutoff,.8,tolerance=.08)
expect_equal(cutoff, 0.8159453415711106, tolerance = 1e-10)

test_that("huber: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "huber", "golden.json"))
  x <- g$data
  expect_error(mlhuber(x, iterlim=1, tol=1e-10, epsilon=1.345),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("huber: P3 fixed-scale fallback requires its location score", {
  x <- c(-.1,0,1e-301,1)
  expect_error(mlhuber(x,epsilon=1.345,sigma=1e-300,iterlim=1), "converge")
  fit <- mlhuber(x,epsilon=1.345,sigma=1e-300)
  expect_lt(abs(fit["mu"]-5e-302),1e-310)
})

test_that("huber certificates retain normal, Laplace and finite witnesses", {
  x <- c(-10,-.1,0,.1,10)
  e <- tryCatch(mlhuber(x,epsilon=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"laplace")
  expect_true(all(c("norm","laplace","") %in% e$candidates$model))
  expect_gte(e$loglik_upper,e$loglik_lower)
  expect_equal(e$limit_loglik,-5*(1+log(8.08)),tolerance=1e-12)
  expect_equal(unname(e$estimates),c(0,4.04),tolerance=1e-12)
  expect_equal(e$loglik_upper-e$loglik_lower,0,tolerance=1e-9)
})

test_that("huber certificates preserve a flat interval and an all-core face", {
  fit <- mlhuber(c(-10,10),sigma=1,epsilon=1)
  expect_equal(unname(fit["mu"]),0)
  expected <- as.numeric(logLik(fit))
  for (m in c(-9,-4,0,4,9)) {
    expect_equal(sum(dhuber(c(-10,10),mu=m,sigma=1,epsilon=1,log=TRUE)),expected,
                 tolerance=1e-12)
  }
  e <- tryCatch(mlhuber(rep(2,3),sigma=1,epsilon=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"norm")
  expect_equal(unname(e$estimates),c(2,1))
})

test_that("huber certificate numerical stops retain an evaluated witness", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"),"huber","golden.json"))
  e <- tryCatch(mlhuber(g$data,epsilon=1.345,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(nrow(e$candidates)>=1)
  expect_true(is.finite(e$loglik_lower))
  expect_equal(e$loglik_upper,Inf)
})

test_that("huber certificates stop at an unresolved finite-normal comparison", {
  z <- function(c) sqrt(2*pi)*(2*pnorm(c)-1)+2*exp(-c*c/2)/c
  gain <- function(c) .5*(2*exp(-c*c/2)/(c*c*z(c)))^2-log(z(c)/sqrt(2*pi))
  c <- uniroot(gain,c(.7,.85),tol=1e-14)$root
  a <- c+2*exp(-c*c/2)/(c*c*z(c))
  e <- tryCatch(mlhuber(c(a,a),mu=0,sigma=1,epsilon=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_false(e$certified)
  expect_true(any(fit_payload(e)$candidates$model=="norm"))
  expect_true(any(fit_payload(e)$candidates$model=="" & fit_payload(e)$candidates$epsilon<1,na.rm=TRUE))
  expect_gte(fit_payload(e)$loglik_upper,fit_payload(e)$loglik_lower)
})

test_that("huber numerical stops report a nonrepresentable objective", {
  e <- tryCatch(mlhuber(c(-100,100),sigma=.Machine$double.xmin,epsilon=1.345),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(is.na(e$loglik_lower))
  expect_equal(e$loglik_upper,Inf)
})

test_that("Huber exposes flat location maxima and a singleton at the kink", {
  x <- c(-10, -9, 9, 10)
  check_box <- function(fit, upper, ll) {
    expect_identical(attr(fit, "certified"), TRUE)
    expect_identical(attr(fit, "unique"), FALSE)
    expect_identical(unname(fit["mu"]), 0)
    piece <- attr(fit, "maximizer_set")$pieces[[1]]
    expect_identical(piece$kind, "box")
    expect_equal(c(piece$axes[[1]]$lower, piece$axes[[1]]$upper), c(-upper, upper))
    expect_equal(as.numeric(logLik(fit)), ll, tolerance = 1e-13)
    for (mu in c(-5, 0, 5)) {
      expect_equal(sum(dhuber(x, mu = mu, sigma = fit[["sigma"]],
                             epsilon = fit[["epsilon"]], log = TRUE)), ll,
                   tolerance = 1e-13)
    }
  }
  # 100-digit mpmath, reference.txt: upper 7.65500000000000002664535259,
  # logLik -51.40634277828734697004501652.
  check_box(mlhuber(x, sigma = 1, epsilon = 1.345), 7.655, -51.40634277828734697004501652)
  # Exact upper 6.625, logLik -15.73984499887975643758677333.
  check_box(mlhuber(x, epsilon = .5), 6.625, -15.73984499887975643758677333)
  # Upper 8.89472828053528152359114613, logLik -15.77767441526718239258782387.
  check_box(mlhuber(x, sigma = 1, epsilon = NULL), 8.89472828053528152359114613,
            -15.77767441526718239258782387)

  # Exact midpoint 0 at epsilon*sigma=9, logLik -183.6757541328186909671322059.
  fit <- mlhuber(x, sigma = 1, epsilon = 9)
  expect_identical(attr(fit, "certified"), TRUE)
  expect_identical(attr(fit, "unique"), TRUE)
  expect_identical(unname(fit["mu"]), 0)
  expect_null(attr(fit, "maximizer_set"))
  expect_equal(as.numeric(logLik(fit)), -183.6757541328186909671322059, tolerance = 1e-13)
})
