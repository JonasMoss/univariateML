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
        expect_match(conditionMessage(fit),"mlpois|mlzip|mlztpois|mlzmpois",info=label)
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
        if(p=="prob") return(c(NA,if(id=="zmlogarithmic") 0 else 1))
        if(p=="size" && id %in% c("ztnbinom","zmnbinom")) return(c(NA,0))
        if(p=="lambda" && id %in% c("pois","ztpois","zmpois"))return(c(NA,0))
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
