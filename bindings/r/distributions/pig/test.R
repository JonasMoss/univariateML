context("mlpig")
test_that("Poisson and zero faces are explicit", {
  for (x in list(rep(1,10), c(0,1,1,2), c(0,2), rep(2,10)))
    expect_error(mlpig(x), "Use `mlpois`")
  poisson_sample <- qpois((seq_len(100)-.5)/100,2)
  expect_lte(mean((poisson_sample-mean(poisson_sample))^2),mean(poisson_sample))
  expect_error(mlpig(poisson_sample), "Use `mlpois`")
  expect_equal(as.numeric(mlpig(rep(0,3))),c(1,0))
  expect_equal(as.numeric(mlpig(rep(0,3),mean=2)),c(2,0))
  expect_error(mlpig(rep(0,3), shape=2), "point-mass")
  fit <- mlpig(rep(0,3), mean=2,shape=3)
  expect_equal(attr(fit,"logLik"),3*dpig(x = 0, mean = 2, shape = 3, log = TRUE))
})
test_that("all fixed subsets agree with accurate density oracle", {
  skip_unless_tier("full")
  x <- c(0,0,0,0,1,2,3,5,10)
  for (held in list(list(),list(mean=3),list(shape=3),list(mean=3,shape=3))) {
    fit <- do.call(mlpig,c(list(x=x),held))
    expect_equal(sort(attr(fit,"fixed")),sort(if (is.null(names(held))) character() else names(held)))
    expect_equal(attr(fit,"logLik"),sum(dpig(x = x, mean = fit[["mean"]], shape = fit[["shape"]], log = TRUE)),tolerance=1e-9)
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    free <- setdiff(c("mean","shape"),names(held))
    if (length(free)) {
      objective <- function(z) {
        pars <- as.list(setNames(as.numeric(fit),names(fit)))
        pars[free] <- exp(z)
        -sum(do.call(dpig,c(list(x=x,log=TRUE),pars)))
      }
      for (mult in c(.5,1,2)) {
        opt <- optim(log(as.numeric(fit[free])*mult),objective,method="BFGS",control=list(reltol=1e-12))
        expect_lte(-opt$value,attr(fit,"logLik")+1e-7)
      }
    }
  }
})
test_that("fixed controls reject invalid values", {
  for (v in list(0,-1,Inf,NA_real_)) {
    expect_error(mlpig(c(0,1,10),mean=v))
    expect_error(mlpig(c(0,1,10),shape=v))
  }
})

test_that("a small proved finite improvement is retained by the shape certificate", {
  x <- c(rep(0,10002),rep(2,10000),3)
  expect_gt(mean((x-mean(x))^2),mean(x))
  fit <- mlpig(x)
  expect_true(is.finite(fit[["shape"]]))
  expect_gt(attr(fit,"logLik"),sum(dpois(x,mean(x),log=TRUE)))
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "pig"
  base <- c(mean=2,shape=3)
  # Independent half-integer Bessel formula; actuar cancels near Poisson.
  density <- function(x,mean,shape,log=TRUE) {
    A <- 2+shape/mean^2; z <- sqrt(shape*A)
    .5*log(2*shape/pi)+shape/mean+(x-.5)/2*log(shape/A)+
      log(besselK(z,abs(x-.5),expon.scaled=TRUE))-z-lgamma(x+1)
  }
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

test_that("P3 PIG shape zero is attained under compatible holds", {
 for(m in c(.2,2,20)) {
   f<-mlpig(rep(0,4),mean=m,shape=0)
   expect_equal(as.numeric(f),c(m,0));expect_equal(attr(f,"logLik"),0)
   expect_equal(dml(c(0,1),f),c(1,0))
   expect_error(mlpig(c(0,1),mean=m,shape=0))
 }
 expect_equal(as.numeric(mlpig(rep(0,4),shape=0)),c(1,0))
})

test_that("pig zero boundary works through generated d/p/q/r", {
  expect_equal(dpig(c(0,1),mean=2,shape=0),c(1,0))
  expect_equal(dpig(c(0,1),mean=2,shape=0,log=TRUE),c(0,-Inf))
  for(lower in c(TRUE,FALSE)) {
    cdf <- if(lower) c(0,1,1) else c(1,0,0)
    expect_equal(ppig(c(-1,0,1),mean=2,shape=0,lower.tail=lower),cdf)
    expect_equal(ppig(c(-1,0,1),mean=2,shape=0,lower.tail=lower,log.p=TRUE),log(cdf))
    expect_equal(qpig(c(0,.5,1),mean=2,shape=0,lower.tail=lower),rep(0,3))
    expect_equal(qpig(c(-Inf,-10000,0),mean=2,shape=0,lower.tail=lower,log.p=TRUE),rep(0,3))
  }
  expect_equal(rpig(20,mean=2,shape=0),rep(0,20))
})

test_that("held positive shape uses the unique mean certificate", {
  f <- mlpig(1,shape=1/9)
  expect_equal(unname(f[["mean"]]),2/3,tolerance=1e-9)
  expect_equal(attr(f,"fixed"),"shape")
  expect_equal(attr(logLik(f),"df"),1L)
  for (s in c(1e-20,.01,3,1e20)) {
    x <- c(0,0,0,0,10)
    f <- mlpig(x,shape=s)
    expect_gt(f[["mean"]],mean(x)/2)
    expect_lt(f[["mean"]],2*mean(x))
    expect_equal(attr(f,"logLik"),sum(dpig(x,f[["mean"]],s,log=TRUE)),tolerance=1e-9)
  }
  e <- tryCatch(mlpig(c(0,0,0,0,10),shape=3,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_equal(e$kind,"numerical")
  expect_equal(e$candidates$shape,3)
  expect_equal(e$loglik_upper,Inf)
  e <- tryCatch(mlpig(.Machine$integer.max,shape=3,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_equal(e$kind,"budget")
})

test_that("held-shape zero limit retains its fitted face", {
  e <- tryCatch(mlpig(rep(0,3),shape=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"pois")
  expect_equal(e$estimates,c(lambda=0))
  expect_equal(e$limit_loglik,0)
  expect_equal(e$loglik_lower,0)
  expect_equal(e$loglik_upper,0)
})

test_that("held-mean binary-count shape uses its closed-form certificate", {
  x <- c(0,1,1)
  f <- mlpig(x,mean=3,iterlim=1)
  w <- sqrt(1-2*mean(x)/3)
  expected <- 2*mean(x)^2/(w*(1+w)^2)
  expect_equal(f[["shape"]],expected,tolerance=1e-12)
  expect_equal(f[["mean"]],3)
  expect_equal(attr(f,"fixed"),"mean")
  expect_equal(attr(f,"logLik"),sum(dpig(x,3,expected,log=TRUE)),tolerance=1e-12)
  e <- tryCatch(mlpig(1,mean=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"pois")
  expect_equal(e$estimates,c(lambda=2))
  expect_equal(e$limit_loglik,dpois(1,2,log=TRUE))
  e <- tryCatch(mlpig(x,mean=4/3),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_s3_class(mlpig(x,mean=4/3+.Machine$double.eps,iterlim=1),"univariateML")
})

test_that("general shape certificates transport limit, tie and budget details", {
  e <- tryCatch(mlpig(c(0,1,2,3),mean=2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"pois")
  expect_equal(e$estimates,c(lambda=2))
  e <- tryCatch(mlpig(c(0,1,2),mean=1+sqrt(1/3)),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_equal(e$candidates$model,"pois")
  expect_equal(e$loglik_upper,Inf)
  e <- tryCatch(mlpig(c(0,0,.Machine$integer.max),iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_equal(e$kind,"budget")
  e <- tryCatch(mlpig(c(0,0,10),iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(nrow(e$candidates)>0)
})
