test_that("ztnbinom agrees with a multi-start reference-density oracle", {
  set.seed(13226)
  x <- do.call(rztnbinom, list(250, size = 2, prob = 0.4))
  fit <- mlztnbinom(x)
  density <- dztnbinom
  ll <- function(p) sum(do.call(density, c(list(x = x), as.list(p), list(log = TRUE))))
  expect_equal(attr(fit, "logLik"), ll(setNames(as.numeric(fit), names(fit))), tolerance=1e-8)
  oracle <- function(held = NULL) {
    free <- setdiff(names(fit), names(held))
    objective <- function(t) {
      p <- setNames(as.numeric(fit), names(fit)); p[names(held)] <- held
      p[free] <- vapply(seq_along(free), function(j) if(free[j] %in% c("size", "lambda")) exp(t[j]) else plogis(t[j]), numeric(1))
      val <- -ll(p); if(is.finite(val)) val else 1e100
    }
    t0 <- vapply(free, function(v) if(v %in% c("size","lambda")) log(fit[v]) else qlogis(fit[v]), numeric(1))
    min(vapply(c(-.7,0,.7), function(delta) optim(t0+delta,objective,method="BFGS",control=list(maxit=2000,reltol=1e-12))$value,numeric(1)))
  }
  best <- oracle()
  expect_gte(attr(fit,"logLik"), -best-1e-6)
  fixed <- mlztnbinom(x, size=2.5)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 2.5)
  expect_equal(attr(logLik(fixed),"df"), 1L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=2.5))-1e-6)
  for(bad in c(-1,Inf,NaN)) expect_error(mlztnbinom(x,size=bad),"size")
  expect_error(mlztnbinom_(integer()),"nonempty")
  expect_error(mlztnbinom_(c(-1L,1L)),"count")
  expect_error(mlztnbinom(c(0,1,2)))
  expect_equal(attr(mlztnbinom(rep(1,4)),"logLik"),0)
})

test_that("ztnbinom declares its infinite-size escape", {
 x <- c(1,2,2,3,3,3)
 expect_error(mlztnbinom(x), "Use `mlztpois`")
 expect_s3_class(mlztnbinom(x,size=20), "univariateML")
})

test_that("ztnbinom returns the attained logarithmic member", {
 x <- c(1,1,1,1,1,2,3,10,30); fit <- mlztnbinom(x)
 expect_equal(unname(fit["size"]),0)
 fixed <- mlztnbinom(x,size=0)
 expect_identical(attr(fixed,"fixed"),"size")
 expect_equal(attr(fixed,"logLik"),attr(fit,"logLik"))
})

test_that("ztnbinom preserves fixed size at attained boundaries", {
 zero <- mlztnbinom(c(1,1),size=5)
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),1L)
 one <- mlztnbinom(c(1,1),size=5)
 expect_equal(unname(one["size"]),5)
})

test_that("ztnbinom fixed zero size agrees with the reference oracle", {
 x <- c(1,1,1,2,3,10,30)
 fit <- mlztnbinom(x,size=0)
 objective <- function(t) -sum(dztnbinom(x = x, size = 0, prob = plogis(t[1]), log = TRUE))
 best <- min(vapply(c(-1,0,1),function(s)optim(rep(s,1),objective,method="BFGS",control=list(reltol=1e-12))$value,numeric(1)))
 expect_equal(attr(fit,"logLik"),-best,tolerance=1e-7)
 expect_equal(sum(dztnbinom(x = x, size = 0, prob = fit["prob"], log = TRUE)),attr(fit,"logLik"),tolerance=1e-10)
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "ztnbinom"
  base <- c(size=2,prob=0.4)
  density <- dztnbinom
  test_that("P2 holds agree with independent multistart reference likelihoods", {
    set.seed(81029)
    samples <- list(simulate_default(paste0("ml",id),20),
                    simulate_default(paste0("ml",id),100),
                    c(1,1,1,2,2,4), c(1,2), rztpois(n = 20, lambda = 2))
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

test_that("P2 truncated NB held probability includes logarithmic endpoint", {
  for(v in c(0,-1,1.1,Inf,NA_real_))expect_error(mlztnbinom(c(1,2,5),prob=v))
  expect_error(mlztnbinom(c(1,2,5),prob=1))
  a<-mlztnbinom(rep(1,3),prob=1)
  expect_equal(attr(a,'logLik'),0)
  b<-mlztnbinom(c(1,2,5),size=0,prob=.4)
  expect_equal(attr(b,'logLik'),sum(dlgser(x = c(1, 2, 5), theta = 0.6, log = TRUE)))
  expect_error(mlztnbinom(c(1,2,5),size=-1,prob=.4),"size")
})

test_that("P2 held-probability search respects iteration failure", {
  expect_error(mlztnbinom(c(1,2,5),prob=.4,iterlim=1),"Profile search")
})

test_that("P3 insufficient profile refinement stops", {
 expect_error(mlztnbinom(c(0,0,1,2,8)[c(3,4,5)],iterlim=1),"Profile search exhausted")
})
