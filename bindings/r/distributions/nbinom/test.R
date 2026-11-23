context("mlnbinom")

## Error when data is Poisson.
expect_error(
  mlnbinom(c(1, 1, 1, 1)),
  "The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead\\."
)

## Non-unique maximum likelihood.
expect_silent(mlnbinom(c(0, 0, 0)))


## Size argument works
expect_equal(unname(mlnbinom(seq(20), size = 20))[1], 20)

test_that("fractional fixed size is preserved", {
  x <- c(0, 1, 2, 5)
  fit <- mlnbinom(x, size = 2.5)
  expect_equal(fit[["size"]], 2.5)
  expect_equal(attr(fit, "logLik"), sum(dnbinom(x, size = 2.5, prob = fit[["prob"]], log = TRUE)))
})

test_that("variance equal to the mean escapes to the Poisson", {
  # Aragon, Eberly and Eberly (1992): a finite MLE exists iff the
  # population-form variance exceeds the mean. Each sample has equality.
  for (x in list(c(0, 2), c(0, 1, 2, 3, 4), c(1, 1, 2, 4, 5, 5), rep(0:4, 1000))) {
    expect_equal(mean(x^2) - mean(x)^2, mean(x))
    expect_error(mlnbinom(x), "Use `mlpois`")
  }
})

test_that("overdispersed data where Newton leaves the domain still fit", {
  x <- c(rep(0, 9), 1000)
  fit <- mlnbinom(x)
  profile <- function(size) sum(dnbinom(x, size = size, mu = mean(x), log = TRUE))
  opt <- optimize(function(t) profile(exp(t)), c(-20, 5), maximum = TRUE, tol = 1e-12)
  expect_equal(fit[["size"]], exp(opt$maximum), tolerance = 1e-6)
  expect_equal(attr(fit, "logLik"), opt$objective, tolerance = 1e-10)
  # Variance barely above the mean: the MLE is finite and large.
  y <- c(rep(0:4, 999), 0, 4)
  expect_gt(mean(y^2) - mean(y)^2, mean(y))
  fit <- mlnbinom(y)
  expect_gt(fit[["size"]], 1e3)
  expect_equal(attr(fit, "logLik"), sum(dnbinom(y, fit[["size"]], fit[["prob"]], log = TRUE)))
})

test_that("a held size must be nonnegative, finite and compatible", {
  for (size in list(0, -1, Inf, NaN, NA_real_)) {
    expect_error(mlnbinom(c(1, 2, 5), size = size), "finite|invalid")
  }
})

test_that("unrepresentable held-size probability is diagnosed", {
  expect_error(mlnbinom(c(1,2,5),size=1e300), "rounds to one")
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "nbinom"
  base <- c(size=2,prob=0.4)
  density <- stats::dnbinom
  test_that("P2 holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
    set.seed(81029)
    samples <- list(ml_simulate(paste0("ml",id),20),
                    ml_simulate(paste0("ml",id),100),
                    c(0,0,0,1,1,4), c(0,2), rpois(20,2))
    nm <- names(base)
    for (x in samples) for (mask in seq_len(2^length(nm)-1)) {
      held <- nm[as.logical(intToBits(mask)[seq_along(nm)])]
      fixed <- as.list(base[held])
      label <- paste(id,length(x),paste(held,collapse=","))
      fit <- tryCatch(suppressWarnings(do.call(get(paste0("ml",id)),c(list(x=x),fixed))),error=identity)
      if (inherits(fit,"error")) {
        # Only declared non-existence faces are allowed in these valid holds.
        expect_match(conditionMessage(fit),"mlpois",info=label)
        next
      }
      expect_identical(as.numeric(fit[held]),as.numeric(base[held]),info=label)
      expect_setequal(attr(fit,"fixed"),held)
      expect_equal(attr(logLik(fit),"df"),length(nm)-length(held),info=label)
      ll <- function(q) {
        values <- suppressWarnings(do.call(density,c(list(x=x,log=TRUE),as.list(q))))
        if(any(!is.finite(values))) -Inf else sum(values)
      }
      expect_equal(attr(fit,"logLik"),ll(setNames(as.numeric(fit),names(fit))),tolerance=1e-7,info=label)
      free <- setdiff(nm,held)
      if(!length(free)) next
      # Independently enumerate attainable free endpoints as well as interiors.
      endpoints <- lapply(free,function(p) {
        if(p %in% c("pi","p0")) return(c(NA,0,1))
        if(p=="prob") return(c(NA,1))
        if(p=="lambda" && id=="pois")return(c(NA,0))
        NA_real_
      })
      grid <- expand.grid(endpoints)
      best <- -Inf
      for (i in seq_len(nrow(grid))) {
        q <- base
        ep <- as.numeric(grid[i,])
        q[free[!is.na(ep)]] <- ep[!is.na(ep)]
        live <- free[is.na(ep)]
        if(!length(live)) {best <- max(best,ll(q));next}
        pr <- live %in% c("prob","pi","p0","theta")
        decode <- function(t) {out<-q;out[live]<-ifelse(pr,plogis(t),exp(t));out}
        objective <- function(t) {v<-ll(decode(t));if(is.finite(v)) -v else 1e100}
        z <- vapply(seq_along(live), function(j) if (pr[j]) qlogis(base[[live[j]]]) else log(base[[live[j]]]), 0.0)
        for (shift in c(-1.5,0,1.5)) {
          o <- optim(z+shift,objective,method="L-BFGS-B",lower=-16,upper=16,
                     control=list(maxit=500,factr=1e5))
          best <- max(best,ll(decode(o$par)))
        }
      }
      expect_gte(attr(fit,"logLik"),best-1e-6*length(x),label=label)
    }
  })
})

test_that("P2 NB held prob has a unique finite size or declared zero escape", {
  for(v in c(0,-1,1.1,Inf,NA_real_))expect_error(mlnbinom(c(0,1,5),prob=v))
  expect_error(mlnbinom(c(0,1,5),prob=1),"incompatible")
  expect_equal(mlnbinom(rep(0,3),prob=.4)[["size"]],0)
  a<-mlnbinom(rep(0,3),prob=1)
  expect_identical(unname(as.numeric(a)),c(0,1))
  b<-mlnbinom(rep(0,3),size=2,prob=.4)
  expect_equal(attr(b,'logLik'),6*log(.4))
  for(v in c(-1,NA_real_,Inf))expect_error(mlnbinom(c(0,1,2),size=v,prob=.4))
})

test_that("P2 held NB probability close to one retains a finite size root", {
  x<-c(0,1,5);p<-1-1e-12
  fit<-mlnbinom(x,prob=p)
  r<-fit[['size']]
  score<-length(x)*log(p)+sum(vapply(x,function(v)if(v==0)0 else sum(1/(r+seq_len(v)-1)),0))
  expect_lt(abs(score),1e-22)
  expect_identical(fit[['prob']],p)
  expect_equal(attr(fit,'logLik'),sum(dnbinom(x,r,p,log=TRUE)),tolerance=1e-9)
})

test_that("P3 size-zero boundary holds and free representatives are attained", {
  for (p in c(.2,.9,1)) {
    fit <- mlnbinom(rep(0,5),size=0,prob=p)
    expect_identical(fit[["size"]],0)
    expect_identical(fit[["prob"]],p)
    expect_equal(attr(fit,"logLik"),0)
    expect_equal(dml(c(0,1),fit),c(1,0))
    expect_error(mlnbinom(c(0,1),size=0,prob=p))
  }
  expect_silent(fit <- mlnbinom(rep(0,5)))
  expect_equal(fit[["size"]],0)
  expect_equal(attr(fit,"logLik"),0)
})

test_that("NB size-zero evaluation includes tails and quantile endpoints", {
  fit <- mlnbinom(rep(0,3),size=0,prob=.4)
  expect_equal(dml(c(0,1),fit,log=TRUE),c(0,-Inf))
  expect_equal(pml(c(-1,0),fit,lower.tail=FALSE),c(1,0))
  expect_equal(qml(c(0,.5,1),fit),c(0,0,0))
})

test_that("overdispersed near-boundary samples are not called underdispersed", {
  for (n in c(10L, 100L, 1000L, 10000L)) {
    x <- rep(c(0, 2), n / 2)
    x[2] <- 3
    expect_gt(mean((x - mean(x))^2), mean(x))
    result <- tryCatch(mlnbinom(x, iterlim = 1), error = identity)
    if (inherits(result, "error")) {
      expect_match(conditionMessage(result), "precision|bracket|convergence")
      expect_false(grepl("mlpois", conditionMessage(result), fixed = TRUE))
    } else expect_true(all(is.finite(result)))
  }
})


test_that("large counts with variance above their mean signal precision failure", {
  k <- 9743L
  m <- k * k - 1
  expect_error(mlnbinom(c(m-k, m+k), iterlim=1), "insufficient precision")
})

test_that("certificate stops carry the Poisson fit", {
  err <- tryCatch(mlnbinom(0:4),error=identity)
  expect_s3_class(err,"univariateML_limit")
  expect_identical(err$model,"pois")
  expect_equal(err$estimates,c(lambda=2))
  expect_equal(err$limit_loglik,sum(dpois(0:4,2,log=TRUE)))
  expect_match(conditionMessage(err),"mlpois")
  expect_equal(as.numeric(mlnbinom(c(0,0,1,3,6),iterlim=1)),as.numeric(mlnbinom(c(0,0,1,3,6))),tolerance=1e-6)
})

test_that("all-held subnormal members retain finite likelihood evaluation", {
  size <- .Machine$double.xmin*.Machine$double.eps
  fit <- mlnbinom(c(0,1),size=size,prob=.5)
  expect_true(is.finite(attr(fit,"logLik")))
  expect_identical(fit[["size"]],size)
})

test_that("large-count held-probability negative binomial logLik crosses the binding intact", {
  x <- c(1000000,1000001,1000003,1000008,1000011)
  fit <- mlnbinom(x,prob=0.30610275261187947)
  ll <- sum(dml(x,fit,log=TRUE))
  expect_lte(abs(as.numeric(logLik(fit))-ll),1e-10*max(1,abs(ll)))
})

test_that("negative-binomial point-law identification follows core holds", {
  z <- c(0L, 0L)
  expect_false(attr(mlnbinom(z), "unique"))
  expect_true(attr(mlnbinom(z, size=2), "unique"))
  expect_false(attr(mlnbinom(z, prob=1), "unique"))
  expect_false(attr(suppressWarnings(mlnbinom(z, zero="modified")), "unique"))
  expect_true(attr(mlnbinom(z, size=2, prob=.5, zero="modified"), "unique"))
  expect_false(attr(mlnbinom(z, zero="inflated"), "unique"))
})

test_that("short negative-binomial windows resolve underflowing event masses", {
  # Independent 100-digit log-gamma sums, TASK-219.
  windows <- list(2:12, 0:5)
  log_pmf <- list(
    c(-157.98126920882223, -141.35234791409792, -125.01110868182539,
      -108.89301299086708, -92.95723884670271, -77.17561537236561,
      -61.52752328065302, -45.99721421459682, -30.572265654198457,
      -15.242627263604412, -2.399999976000006e-7),
    c(-83.85017627418006, -66.12264271078763, -49.08825631795516,
      -32.45933502323085, -16.118095790958318, -9.999999900000002e-8))
  log_cdf <- list(
    c(-157.98126920882223, -141.35234785409793, -125.0111086018254,
      -108.89301289086707, -92.95723872670271, -77.1756152323656,
      -61.52752312065302, -45.99721403459683, -30.572265454198458,
      -15.242627043604413, 0),
    c(-83.85017627418006, -66.12264269078763, -49.08825627795516,
      -32.459334963230845, -16.11809571095832, 0))
  lower_quantiles <- list(c(4, 11, 11, 12, 12, 12), c(0, 4, 5, 5, 5, 5))
  upper_quantiles <- list(c(12, 12, 12, 12, 12, 11), c(5, 5, 5, 5, 5, 4))
  targets <- c(1e-60, 1e-12, 1e-7, .5, .9, .9999999999)
  for (i in seq_along(windows)) {
    k <- windows[[i]]
    obj <- mlnbinom(size = 1e8, prob = .5, truncate = range(k))
    expect_lt(max(abs(dml(k, obj, log = TRUE) / log_pmf[[i]] - 1)), 1e-12)
    expect_lt(max(abs(pml(k, obj, log.p = TRUE) - log_cdf[[i]])), 1e-12)
    cuts <- head(k, -1L)
    log_sf <- log1p(-exp(head(log_cdf[[i]], -1L)))
    expect_lt(max(abs(pml(cuts, obj, lower.tail = FALSE, log.p = TRUE) / log_sf - 1)), 1e-12)
    expect_warning(outside <- dml(c(min(k)-1, min(k)+.5, max(k)+1), obj), "non-integer x")
    expect_equal(outside, c(0, 0, 0))
    expect_equal(qml(targets, obj), lower_quantiles[[i]])
    expect_equal(qml(log(targets), obj, log.p = TRUE), lower_quantiles[[i]])
    expect_equal(qml(targets, obj, lower.tail = FALSE), upper_quantiles[[i]])
    expect_equal(qml(log(targets), obj, lower.tail = FALSE, log.p = TRUE), upper_quantiles[[i]])
    expect_equal(qml(c(0, 1), obj), range(k))
  }
})

test_that("size-zero negative binomial draws the point mass without consuming RNG", {
  frozen <- mlnbinom(size = 0, prob = .5, zero = "none")
  fitted <- mlnbinom(rep(0, 4), size = 0, prob = .5)
  set.seed(219)
  seed <- .Random.seed
  for (obj in list(frozen, fitted)) {
    expect_equal(dml(c(-1, 0, 1), obj), c(0, 1, 0))
    expect_equal(pml(c(-1, 0, 1), obj), c(0, 1, 1))
    expect_equal(qml(c(0, .5, 1), obj), c(0, 0, 0))
    expect_identical(rml(512, obj), rep(0, 512))
    expect_identical(rml(0, obj), numeric())
    expect_identical(rml(c(2, 3), obj), c(0, 0))
  }
  expect_identical(.Random.seed, seed)
  ordinary <- mlnbinom(size = 2, prob = .5)
  draws <- rml(16, ordinary)
  set.seed(219)
  expect_identical(draws, stats::rnbinom(16, size = 2, prob = .5))
})

test_that("a held-size negative-binomial window publishes its feasible probability interval", {
  for (x in c(0, 2)) {
    fit <- mlnbinom(rep(x, 4), size = 3, truncate = c(x, x))
    set <- attr(fit, "maximizer_set")
    expect_identical(set$coordinates, c("size", "prob"))
    expect_equal(set$pieces[[1]]$axes[[1]]$lower, 3)
    expect_identical(set$pieces[[1]]$axes[[2]]$closed, c(FALSE, x == 0))
    expect_equal(as.numeric(logLik(fit)), 0)
  }
})
