context("mlzinb")

zinb_oracle <- function(x, size = NULL) {
  objective <- function(t) {
    r <- if (is.null(size)) exp(t[1]) else size
    p <- plogis(t[if (is.null(size)) 2 else 1])
    pi <- plogis(t[if (is.null(size)) 3 else 2])
    if (!is.finite(r) || r < 0 || p <= 0) return(1e100)
    -sum(dzinb(x = x, size = r, prob = p, pi = pi, log = TRUE))
  }
  min(vapply(c(.2, 1, 5, 20), function(r) {
    start <- c(if (is.null(size)) log(r), qlogis(.4), qlogis(.2))
    optim(start, objective, method = "BFGS", control = list(maxit=2000, reltol=1e-12))$value
  }, numeric(1)))
}

test_that("profiled ZINB agrees with multistart external oracle", {
  set.seed(718)
  for (r in c(.5, 2, 5)) {
    x <- rzinb(n = 1500, size = r, prob = 0.35, pi = 0.3)
    fit <- mlzinb(x)
    expect_equal(attr(fit,"logLik"), -zinb_oracle(x), tolerance=1e-7)
    fixed <- mlzinb(x, size=1.5)
    expect_equal(unname(fixed["size"]), 1.5)
    expect_equal(attr(fixed,"fixed"), "size")
    expect_equal(attr(logLik(fixed),"df"), 2L)
    expect_equal(attr(logLik(fit),"df"), 3L)
    expect_equal(attr(fixed,"logLik"), -zinb_oracle(x,1.5), tolerance=1e-7)
  }
})

test_that("ZINB faces and fixed-size subset have the declared behavior", {
  # Underdispersed positive counts force the ZIP face; no moment test in fitter.
  expect_error(mlzinb(c(rep(0,50),rep(4,50))), "Use `mlzip`")
  expect_error(mlzinb(rep(2,100)), "Use `mlpois`")
  expect_s3_class(mlzinb(rep(2,100),size=1.5),"univariateML")
  for (bad in c(-1,Inf,-Inf,NaN))
    expect_error(mlzinb(0:4,size=bad), "size must")
})

test_that("all-zero representative supports external probability evaluation", {
  expect_silent(fit <- mlzinb(rep(0,10)))
  expect_equal(as.numeric(fit), c(0,.5,0))
  expect_equal(dml(c(0,1),fit),c(1,0))
  expect_equal(pml(c(-1,0,1),fit),c(0,1,1))
  expect_equal(qml(c(0,.5,1),fit),c(0,0,0))
  expect_silent(fixed <- mlzinb(rep(0,10),size=.5))
  expect_equal(as.numeric(fixed),c(.5,1,0))
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "zinb"
  base <- c(size=2,prob=0.4,pi=0.3)
  density <- dzinb
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

test_that("P2 ZINB held boundaries, incompatible data and constrained faces", {
  for(v in c(0,-1,1.1,Inf,NA_real_))expect_error(mlzinb(c(0,1,5),prob=v))
  for(v in c(-1,1.1,Inf,NA_real_))expect_error(mlzinb(c(0,1,5),pi=v))
  expect_error(mlzinb(c(0,1,5),prob=1),"compatible")
  expect_error(mlzinb(c(0,1,5),pi=1),"compatible")
  expect_equal(mlzinb(rep(0,3),prob=.4,pi=.3)[["size"]],0)
  expect_equal(attr(suppressWarnings(mlzinb(rep(0,3),prob=.4)),"logLik"),0)
  expect_equal(attr(suppressWarnings(mlzinb(rep(0,3),pi=.3)),"logLik"),0)
  expect_error(mlzinb(c(rep(0,50),rep(4,50)),pi=.5),"mlzip")
  expect_error(mlzinb(rep(2,100),pi=0),"mlpois")
  a<-mlzinb(c(0,1,5),size=2,prob=.4)
  z<-mean(c(0,1,5)==0); f0<-.4^2
  expect_equal(unname(as.numeric(a['pi'])),max(0,(z-f0)/(1-f0)))
  b<-suppressWarnings(mlzinb(rep(0,3),size=2,prob=.4,pi=.3))
  expect_equal(attr(b,'logLik'),3*log(.3+.7*.4^2))
})

test_that("P2 constrained ZINB reports insufficient iterations", {
  expect_error(mlzinb(c(0,1,2,5),prob=.4,iterlim=1),"MLE failed")
  expect_error(mlzinb(c(0,1,2,5),size=2,pi=.3,iterlim=1),"MLE failed")
})

test_that("P2 fixed size and inflation compare separated probability peaks", {
  x<-c(rep(0,99),10)
  for(pi in c(.001,.1,.9)) {
    fit<-mlzinb(x,size=2,pi=pi)
    objective<-function(t) {
      prob<-plogis(t)
      if(prob<=0) return(1e100)
      -sum(dzinb(x = x, size = 2, prob = prob, pi = pi, log = TRUE))
    }
    values<-vapply(seq(-5,5,length.out=9),function(t)
      optim(t,objective,method="BFGS",control=list(maxit=500,reltol=1e-12))$value,0)
    expect_gte(attr(fit,"logLik"),-min(values)-1e-7)
  }
})

test_that("P3 size-zero boundary holds and free representatives are attained", {
  for (p in c(.2,.9,1)) {
    fit <- mlzinb(rep(0,5),size=0,prob=p)
    expect_identical(fit[["size"]],0)
    expect_identical(fit[["prob"]],p)
    expect_equal(attr(fit,"logLik"),0)
    expect_equal(dml(c(0,1),fit),c(1,0))
    expect_error(mlzinb(c(0,1),size=0,prob=p))
  }
  expect_silent(fit <- mlzinb(rep(0,5)))
  expect_equal(fit[["size"]],0)
  expect_equal(attr(fit,"logLik"),0)
})

test_that("P3 insufficient profile refinement stops", {
 expect_error(mlzinb(c(0,0,1,2,8)[c(1,2,3,4,5)],iterlim=1),"failed|converge")
})

test_that("zinb zero boundary works through generated d/p/q/r", {
  expect_equal(dzinb(c(0,1),size=0,prob=.4,pi=.3),c(1,0))
  expect_equal(dzinb(c(0,1),size=0,prob=.4,pi=.3,log=TRUE),c(0,-Inf))
  for(lower in c(TRUE,FALSE)) {
    cdf <- if(lower) c(0,1,1) else c(1,0,0)
    expect_equal(pzinb(c(-1,0,1),size=0,prob=.4,pi=.3,lower.tail=lower),cdf)
    expect_equal(pzinb(c(-1,0,1),size=0,prob=.4,pi=.3,lower.tail=lower,log.p=TRUE),log(cdf))
    expect_equal(qzinb(c(0,.5,1),size=0,prob=.4,pi=.3,lower.tail=lower),rep(0,3))
    expect_equal(qzinb(c(-Inf,-10000,0),size=0,prob=.4,pi=.3,lower.tail=lower,log.p=TRUE),rep(0,3))
  }
  expect_equal(rzinb(20,size=0,prob=.4,pi=.3),rep(0,20))
})
