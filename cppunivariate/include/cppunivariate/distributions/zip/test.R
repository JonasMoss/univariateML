context("mlzip")

## Warning when ml is not unique
expect_warning(mlzip(c(0, 0, 0)))

set.seed(1)
expect_equal(unname(mlzip(rpois(10, 1))[2]), 0)

test_that("ordinary zero-inflated samples attain the oracle likelihood", {
  samples <- list(
    c(0, 1, 2, 1, 1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3),
    c(0, 0, 4)
  )
  lambda <- c(1.423265, 3.920695)
  pi <- c(0.4730435, 0.6599249)
  oracle_ll <- c(-23.0097617, -3.52318909)
  for (i in seq_along(samples)) {
    fit <- mlzip(samples[[i]])
    expect_lt(abs(unname(fit["lambda"]) / lambda[i] - 1), 1e-4)
    expect_lt(abs(unname(fit["pi"]) / pi[i] - 1), 1e-4)
    expect_gte(attr(fit, "logLik"), oracle_ll[i] - 1e-6)
    expect_true(all(is.finite(fit)))
    expect_equal(attr(fit, "logLik"),
                 sum(dzip(x = samples[[i]], lambda = fit["lambda"], pi = fit["pi"], log = TRUE)),
                 tolerance = 1e-10)
  }
})

test_that("ZIP boundary fits are finite and represent their distributions", {
  expect_warning(zero <- mlzip(c(0, 0, 0)), "all observations are zero")
  expect_equal(as.numeric(zero), c(1, 1))
  expect_equal(attr(zero, "logLik"), 0)
  for (x in list(c(1, 2, 3), c(0, 1, 1, 2))) {
    fit <- mlzip(x)
    expect_equal(unname(fit["pi"]), 0)
    expect_equal(unname(fit["lambda"]), mean(x))
    expect_equal(attr(fit, "logLik"), sum(dpois(x, mean(x), log = TRUE)))
  }
  expect_error(mlzip_(integer()), "requires nonempty nonnegative count data")
  expect_error(mlzip_(c(-1L, 0L)), "requires nonempty nonnegative count data")
})

test_that("ZIP Poisson face has an accurate likelihood at the count ceiling", {
  x<-c(.Machine$integer.max-1,.Machine$integer.max)
  f<-mlzip(x)
  expect_equal(f[["pi"]],0)
  expect_equal(attr(f,"logLik"),sum(dpois(x,mean(x),log=TRUE)),tolerance=1e-10)
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "zip"
  base <- c(lambda=2,pi=0.3)
  density <- dzip
  test_that("P2 holds agree with independent multistart reference likelihoods", {
    set.seed(81029)
    samples <- list(simulate_default(paste0("ml",id),20),
                    simulate_default(paste0("ml",id),100),
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

test_that("P2 ZIP held boundaries and zero escape", {
  expect_error(mlzip(c(0,1,2),lambda=0),"lambda positive")
  for(v in c(-1,Inf,NA_real_))expect_error(mlzip(c(0,1,2),lambda=v))
  for(v in c(-.1,1,1.1,Inf,NA_real_))expect_error(mlzip(c(0,1,2),pi=v))
  expect_error(suppressWarnings(mlzip(rep(0,3),pi=.2)),"mlpois")
  a<-suppressWarnings(mlzip(rep(0,3),pi=1))
  expect_identical(unname(as.numeric(a['pi'])),1)
  b<-suppressWarnings(mlzip(rep(0,3),lambda=2))
  expect_identical(unname(as.numeric(b)),c(2,1))
  c<-mlzip(c(0,1,2),pi=0)
  expect_equal(unname(as.numeric(c['lambda'])),1)
  expect_equal(attr(c,'logLik'),sum(dpois(c(0,1,2),1,log=TRUE)))
})

test_that("P2 fixed inflation compares separated rate peaks", {
  x<-c(rep(0,99),10)
  for(pi in c(.001,.1,.9)) {
    fit<-mlzip(x,pi=pi)
    objective<-function(t) {
      rate<-exp(t)
      if(!is.finite(rate)) return(1e100)
      -sum(dzip(x = x, lambda = rate, pi = pi, log = TRUE))
    }
    values<-vapply(seq(-5,4,length.out=9),function(t)
      optim(t,objective,method="BFGS",control=list(maxit=500,reltol=1e-12))$value,0)
    expect_gte(attr(fit,"logLik"),-min(values)-1e-7)
  }
})
