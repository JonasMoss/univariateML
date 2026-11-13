test_that("zmnbinom agrees with a multi-start reference-density oracle", {
  skip_unless_tier("full")
  set.seed(13219)
  x <- do.call(rzmnbinom, list(250, size = 2, prob = 0.4, p0 = 0.3))
  fit <- .modifier_zmnbinom(x)
  density <- dzmnbinom
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
  fixed <- .modifier_zmnbinom(x, size=2.5)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 2.5)
  expect_equal(attr(logLik(fixed),"df"), 2L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=2.5))-1e-6)
  for(bad in c(-1,Inf,NaN)) expect_error(.modifier_zmnbinom(x,size=bad),"size")
  expect_error(modifier_zmnbinom_(integer()),"nonempty")
  expect_error(modifier_zmnbinom_(c(-1L,1L)),"count")
  expect_warning(zero <- .modifier_zmnbinom(c(0,0,0)),"unidentified")
  expect_equal(unname(zero["p0"]),1)
  expect_warning(two <- .modifier_zmnbinom(c(0,0,1,1)),"mlbinom")
  expect_equal(attr(two,"logLik"),attr(mlbinom(c(0,0,1,1),size=1),"logLik"))
  expect_equal(unname(.modifier_zmnbinom(x[x>0])["p0"]),0)
})

test_that("zmnbinom declares its infinite-size escape", {
 x <- c(0,0,1,2,2,3,3,3)
 expect_error(.modifier_zmnbinom(x), "Use `mlpois.*zero = .modified")
 expect_s3_class(.modifier_zmnbinom(x,size=20), "univariateML")
})

test_that("zmnbinom returns the attained logarithmic member", {
 x <- c(0,0,1,1,1,1,1,2,3,10,30); fit <- .modifier_zmnbinom(x)
 expect_equal(unname(fit["size"]),0)
 fixed <- .modifier_zmnbinom(x,size=0)
 expect_identical(attr(fixed,"fixed"),"size")
 expect_equal(attr(fixed,"logLik"),attr(fit,"logLik"))
})

test_that("zmnbinom preserves fixed size at attained boundaries", {
 expect_warning(zero <- .modifier_zmnbinom(c(0,0),size=5),"unidentified")
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),2L)
 expect_warning(one <- .modifier_zmnbinom(c(0,1,1),size=5),"mlbinom")
 expect_equal(unname(one["size"]),5)
})

test_that("zmnbinom fixed zero size agrees with the reference oracle", {
  skip_unless_tier("full")
 x <- c(0,0,1,1,1,2,3,10,30)
 fit <- .modifier_zmnbinom(x,size=0)
 objective <- function(t) -sum(dzmnbinom(x = x, size = 0, prob = plogis(t[1]), p0 = plogis(t[2]), log = TRUE))
 best <- min(vapply(c(-1,0,1),function(s)optim(rep(s,2),objective,method="BFGS",control=list(reltol=1e-12))$value,numeric(1)))
 expect_equal(attr(fit,"logLik"),-best,tolerance=1e-7)
 expect_equal(sum(dzmnbinom(x = x, size = 0, prob = fit["prob"], p0 = fit["p0"], log = TRUE)),attr(fit,"logLik"),tolerance=1e-10)
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "zmnbinom"
  base <- c(size=2,prob=0.4,p0=0.3)
  density <- dzmnbinom
  test_that("P2 holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
    set.seed(81029)
    samples <- list(ml_simulate(paste0(".modifier_",id),20),
                    ml_simulate(paste0(".modifier_",id),100),
                    c(0,0,0,1,1,4), c(0,2), rpois(20,2))
    nm <- names(base)
    for (x in samples) for (mask in seq_len(2^length(nm)-1)) {
      held <- nm[as.logical(intToBits(mask)[seq_along(nm)])]
      fixed <- as.list(base[held])
      label <- paste(id,length(x),paste(held,collapse=","))
      fit <- tryCatch(suppressWarnings(do.call(get(paste0(".modifier_",id)),c(list(x=x),fixed))),error=identity)
      if (inherits(fit,"error")) {
        # Only declared non-existence faces are allowed in these valid holds.
        expect_match(conditionMessage(fit),"mlpois|.modifier_zip|.modifier_ztpois|.modifier_zmpois",info=label)
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

test_that("P2 modified holds separate zero and positive likelihoods", {
  x<-c(0,0,1,2,5)
  for(p in c(0,1,-.1,1.1,Inf,NA_real_))expect_error(.modifier_zmnbinom(x,p0=p))
  for(p in c(-1,Inf,NA_real_))expect_error(.modifier_zmnbinom(x,prob=p))
  f<-suppressWarnings(.modifier_zmnbinom(rep(0,3),prob=0.4,p0=.7))
  expect_equal(attr(f,"logLik"),3*log(.7))
  expect_identical(unname(as.numeric(f["prob"])),0.4)
  # A held empirical zero fraction changes only the binomial factor.
  a<-suppressWarnings(.modifier_zmnbinom(x,prob=0.4))
  d<-suppressWarnings(.modifier_zmnbinom(x,prob=0.4,p0=mean(x==0)))
  expect_equal(as.numeric(a),as.numeric(d))
  expect_equal(attr(a,"logLik"),attr(d,"logLik"))
  e<-.modifier_zmnbinom(c(1,2,5),prob=0.4,p0=0)
  expect_true(is.finite(attr(e,"logLik")))
})

test_that("P2 held p0 retains the conditional Poisson escape", {
  expect_error(.modifier_zmnbinom(c(0,0,1,2,2,3,3,3),p0=.25),"mlpois.*zero = .modified")
  a<-.modifier_zmnbinom(c(0,0,1,1,1,2,3,10,30),size=0,prob=.4,p0=.2)
  expect_identical(unname(as.numeric(a['size'])),0)
  expect_error(.modifier_zmnbinom(c(0,1,2),size=-1,prob=.4),"size")
})

test_that("P2 held-probability search respects iteration failure", {
  expect_s3_class(.modifier_zmnbinom(c(0,1,2,5),prob=.4,iterlim=1),"univariateML")
})

test_that("P3 insufficient profile refinement stops", {
 expect_error(.modifier_zmnbinom(c(0,0,1,2,8)[c(1,2,3,4,5)],iterlim=1),"Profile certificate.*unresolved")
})

test_that("delegated certificate retains modified Poisson stop details", {
  x <- c(0,0,1,2,2,3,3,3)
  for(p in list(NULL,.4)) {
    e <- tryCatch(.modifier_zmnbinom(x,p0=p),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_identical(e$model,"zmpois")
    expect_named(e$estimates,c("lambda","p0"))
    face <- .modifier_zmpois(x,p0=p)
    expect_equal(unname(e$estimates),unname(as.numeric(face)))
    expect_equal(e$limit_loglik,attr(face,"logLik"))
  }
  e <- tryCatch(.modifier_zmnbinom(c(0,0,1,2,5),iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})
