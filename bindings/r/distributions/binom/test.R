context("mlbinom")

## Expect error when fed with overdispersed data
expect_error(
  mlbinom(seq(20), size = NULL),
  "The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument\\."
)

## Size argument works
expect_equal(unname(mlbinom(seq(20), size = 20))[1], 20)

## Error when size > max
expect_error(mlbinom(seq(21), size = 20))

## Underdispersed / invalid inputs
expect_error(
  mlbinom(rep(1, 5), size = 0),
  "`size` is smaller than `max\\(x\\)`\\."
)

test_that("binomial probability boundaries have log likelihood zero", {
  for (fit in list(mlbinom(c(5L, 5L, 5L), size = NULL),
                   mlbinom(c(5L, 5L, 5L), size = 5))) {
    expect_equal(as.numeric(fit), c(5, 1))
    expect_equal(attr(fit, "logLik"), 0)
    expect_equal(attr(fit, "logLik"), sum(dbinom(rep(5, 3), 5, 1, log = TRUE)))
  }
  for (size in c(0, 5)) {
    fit <- mlbinom(c(0L, 0L, 0L), size = size)
    expect_equal(as.numeric(fit), c(size, 0))
    expect_equal(attr(fit, "logLik"), 0)
  }
  fit <- mlbinom(c(0L, 0L, 0L), size = NULL)
  expect_equal(as.numeric(fit), c(0, 0))
  expect_equal(attr(fit, "logLik"), 0)
})

test_that("other discrete zero-data fits have the point-mass likelihood", {
  expect_equal(attr(mlpois(c(0, 0, 0)), "logLik"), 0)
  expect_equal(attr(mlgeom(c(0, 0, 0)), "logLik"), 0)
  # R7 (A2): size 0 is the attained point mass at 0; prob is unidentified
  # and reported as 0.5 (P3 domains).
  expect_silent(fit <- mlnbinom(c(0, 0, 0)))
  expect_equal(as.numeric(fit), c(0, 0.5))
  expect_equal(attr(fit, "logLik"), 0)
  expect_silent(fixed <- mlnbinom(c(0, 0, 0), size = 3))
  expect_equal(attr(fixed, "logLik"), 0)
})

test_that("a held size must be a nonnegative integer", {
  # NA, NaN and Inf used to be read as "estimate size" while the fit still
  # reported size as held; fractional sizes gave a non-binomial likelihood.
  for (size in list(NA_real_, NaN, Inf, -1, 9.5)) {
    expect_error(mlbinom(c(1, 2, 3), size = size), "nonnegative integer")
  }
})

test_that("mean equal to variance escapes to the Poisson", {
  # c(0, 2): mean 1, population variance 1.
  expect_error(mlbinom(c(0, 2), size = NULL), "Use `mlpois`")
})

test_that("huge held binomial sizes have stable likelihoods", {
  x <- c(0,1,2,5,9)
  for (s in c(1e9, 2^31, 1e300)) {
    f <- mlbinom(x,size=s)
    expect_equal(attr(f,"logLik"),sum(dbinom(x,size=s,prob=mean(x)/s,log=TRUE)), tolerance=1e-10)
  }
})

test_that("free binomial size agrees with exhaustive integer profiles", {
  for (x in list(c(2,6,6),c(1,2,3,4,5),c(1,3,3,4))) {
    f <- mlbinom(x, size = NULL)
    sizes <- max(x):500
    ll <- vapply(sizes,function(k) sum(dbinom(x,k,mean(x)/k,log=TRUE)),0.0)
    expect_equal(unname(f["size"]),sizes[which.max(ll)])
    expect_equal(as.numeric(logLik(f)),max(ll),tolerance=1e-10)
  }
  expect_equal(unname(mlbinom(c(2,6,6), size = NULL)["size"]),13)
})

 test_that("binom: new holds match reference-density integer and multistart oracles", {
  skip_unless_tier("full")
  set.seed(810)
  samples <- c(lapply(c(20,100),function(n) rbinom(n,8,.4)),list(c(1,2,2),rep(2,4),c(1,1,2,3)))
  for(x in samples) {
    for(fixed_size in c(TRUE,FALSE)) {
      held <- list(prob=.4)


      args <- c(list(x=x,size=if(fixed_size) 8 else NULL),held)
      fit <- suppressWarnings(do.call(mlbinom,args))
      sizes <- if(fixed_size) 8 else seq(max(x),200)
      ll <- function(size, mixture=0) sum(stats::dbinom(x,size=size,prob=.4,log=TRUE))
      oracle <- max(vapply(sizes,function(size) {
        return(ll(size))

      },0.))
      expect_equal(attr(fit,"logLik"),oracle,tolerance=1e-6)
      for(name in names(held)) expect_identical(unname(fit[name]),held[[name]])
      expect_setequal(attr(fit,"fixed"),c(if(fixed_size) "size",names(held)))

    }
  }
  for(p in c(-.1,1.1,NA_real_,Inf)) expect_error(mlbinom(c(1,2),size=8,prob=p),"prob")
  expect_error(mlbinom(c(1,2),size=8,prob=1))
  expect_error(mlbinom(c(1,2),size=NULL,prob=0))
})

test_that("free binomial size has no magnitude cap: very large optimal sizes agree with an independent lgamma profile", {
  # TASK-85/87: the old search hard-failed once the bracket reached 1e6.
  # These samples are engineered (one observation at 0, v copies of v) to
  # have x_bar just barely below the population variance, which is
  # exactly the regime that pushes the free-size MLE far out; both have
  # a true optimal size far beyond that old cap (DeRiggi 1983 still
  # guarantees existence and a single profile maximum, since
  # x_bar/var_pop > 1 for both).
  #
  # The oracle below is independent of mlbinom: its own lgamma profile,
  # written from scratch in R (double precision), using the same
  # cancellation-free algebraic form the C++ fitter now uses --
  # subtracting the two size*log(size)-scale terms directly (as the old
  # C++ code did) loses all precision by this magnitude; see
  # dev/research/evaluations/2026-10-29-tail85-report.md.
  binom_profile_ll <- function(uniq, counts, size) {
    n <- sum(counts); xbar <- sum(uniq * counts) / n
    p <- xbar / size
    ll <- n * xbar * log(p) + n * (size - xbar) * log1p(-p)
    for (i in seq_along(uniq))
      ll <- ll + counts[i] * (lgamma(size + 1) - lgamma(uniq[i] + 1) - lgamma(size - uniq[i] + 1))
    ll
  }
  binom_profile_increment <- function(uniq, counts, z) {
    n <- sum(counts); xbar <- sum(uniq * counts) / n
    phi <- function(w) {
      if (w < 1000) return(w * log1p(1 / w) - 1)
      term <- -1 / (2 * w); s <- term
      for (j in 1:40) {
        term <- term * (-(j + 1) / (j + 2)) / w
        s <- s + term
        if (abs(term) < abs(s) * 1e-18) break
      }
      s
    }
    sum(counts * log1p(uniq / (z + 1 - uniq))) +
      n * (log1p(-xbar / (z + 1)) + phi(z - xbar) - phi(z))
  }
  binom_profile_search <- function(uniq, counts, start) {
    lo <- start; hi <- start
    it <- 0
    while (binom_profile_increment(uniq, counts, hi) > 0) {
      lo <- hi + 1; hi <- hi * 2
      it <- it + 1; if (it > 200) stop("R oracle did not bracket")
    }
    while (hi > lo) {
      mid <- lo + floor((hi - lo) / 2)
      if (mid < lo) mid <- lo
      if (mid >= hi) mid <- hi - 1
      if (binom_profile_increment(uniq, counts, mid) > 0) lo <- mid + 1 else hi <- mid
      it <- it + 1; if (it > 200) stop("R oracle did not bisect")
    }
    hi
  }

  for (v in c(1000, 5000)) {
    x <- c(0, rep(v, v))
    f <- mlbinom(x, size = NULL)
    expect_gt(unname(f["size"]), 1e6 * (v == 5000) + 1e5 * (v == 1000))

    uniq <- c(0, v); counts <- c(1, v)
    oracle_size <- binom_profile_search(uniq, counts, v)
    oracle_ll <- binom_profile_ll(uniq, counts, oracle_size)

    # The two independent (long-double C++ vs double R) searches land on
    # the same integer, or within a hair of it: near such a large
    # optimum the profile is so flat (the forward difference is O(1/size^2)
    # and its own rate of change O(1/size^3)) that double precision alone
    # cannot always resolve the very last integer -- but the region and
    # the attained log-likelihood agree tightly.
    expect_equal(unname(f["size"]), oracle_size, tolerance = 1e-3)
    expect_equal(as.numeric(logLik(f)), oracle_ll, tolerance = 1e-6)

    # Unambiguous, coarsely-resolvable checks: beats the old 1e6 cap
    # outright, and beats a grid of sizes genuinely far from the
    # optimum. Multiples of the *found* size would stay misleadingly
    # close in log-likelihood terms -- the profile is this flat even
    # several multiples out, exactly because the optimum itself is so
    # large (the first version of this test asserted exactly that and
    # failed by 1.7e-4 at 2x the found size, which was comparing two
    # double-precision evaluations at a scale where they cannot resolve
    # anything that fine; see dev/research/evaluations/2026-10-29-tail85-report.md).
    # Multiples of max(x) instead stay in a qualitatively different,
    # clearly worse regime, with margins from ~4 (10x) down to a
    # comfortably resolvable ~0.004-0.04 (100x) -- still orders of
    # magnitude above double precision's noise floor here.
    far_sizes <- unique(pmax(max(x), round(max(x) * c(1, 2, 5, 10, 50, 100))))
    for (s in far_sizes) {
      far_ll <- binom_profile_ll(uniq, counts, s)
      expect_gte(as.numeric(logLik(f)) + 1e-6, far_ll)
    }
  }

  # A size whose true optimum is astronomically large (~3e11): the search
  # must not hang or fail, and must still dramatically beat the old cap.
  v <- 1e6
  x <- c(0, rep(v, v))
  f <- mlbinom(x, size = NULL)
  expect_gt(unname(f["size"]), 1e8)
  at_cap <- sum(dbinom(x, size = 1e6, prob = mean(x) / 1e6, log = TRUE))
  expect_gt(as.numeric(logLik(f)), at_cap + 1e6)
})

test_that("discrete limit stops expose fitted faces without changing messages", {
  e <- tryCatch(mlbinom(c(0,0,6), size=NULL), error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"pois")
  expect_equal(e$estimates,c(lambda=2))
  expect_equal(e$limit_loglik,sum(dpois(c(0,0,6),2,log=TRUE)))
  expect_match(conditionMessage(e),"Use `mlpois`",fixed=TRUE)
  e <- tryCatch(mlbbinom(c(0,1,1,2),size=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"binom")
  expect_equal(e$estimates,c(size=2,prob=.5))
  expect_equal(e$limit_loglik,sum(dbinom(c(0,1,1,2),2,.5,log=TRUE)))
  e <- tryCatch(mldgamma(c(2,2,3)),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_match(e$model,"unavailable:",fixed=TRUE)
  expect_equal(e$estimates,c(lower=2,upper=3,prob=1/3))
  expect_equal(e$limit_loglik,2*log(2/3)+log(1/3))
  e <- tryCatch(mlcmb(c(0,1),size=NULL),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_identical(e$model,"binom")
  expect_equal(e$estimates,c(size=1,prob=.5))
  e <- tryCatch(mlztbinom(c(1,1,1,2,3,7,12),size=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"ztpois")
  expect_equal(e$estimates,unclass(mlztpois(c(1,1,1,2,3,7,12)))["lambda"],ignore_attr=TRUE)
})

test_that("discrete preflight budgets are classed without fabricated candidates", {
  e <- tryCatch(mlbbinom(c(0,100001),size=NULL,alpha=1,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_length(e$estimates,0)
  expect_equal(nrow(e$candidates),0)
  expect_identical(e$loglik_upper,Inf)
  e <- tryCatch(mlbinom(c(1,2),size=-1),error=identity)
  expect_s3_class(e,"error")
  expect_false(inherits(e,"univariateML_stop"))
})

test_that("large-count held-probability binomial logLik crosses the binding intact", {
  x <- c(1000000,1000001,1000003,1000008,1000011)
  fit <- mlbinom(x,size=NULL,prob=0.5022)
  ll <- sum(dml(x,fit,log=TRUE))
  expect_lte(abs(as.numeric(logLik(fit))-ll),1e-10*max(1,abs(ll)))
})
