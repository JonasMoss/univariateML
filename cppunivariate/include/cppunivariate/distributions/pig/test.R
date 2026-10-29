context("mlpig")
test_that("Poisson and zero faces are explicit", {
  for (x in list(rep(1,10), c(0,1,1,2), c(0,2), rep(2,10)))
    expect_error(mlpig(x), "Use `mlpois`")
  poisson_sample <- qpois((seq_len(100)-.5)/100,2)
  expect_lte(mean((poisson_sample-mean(poisson_sample))^2),mean(poisson_sample))
  expect_error(mlpig(poisson_sample), "Use `mlpois`")
  expect_error(mlpig(rep(0,3)), "point-mass")
  expect_error(mlpig(rep(0,3), mean=2), "point-mass")
  expect_error(mlpig(rep(0,3), shape=2), "point-mass")
  fit <- mlpig(rep(0,3), mean=2,shape=3)
  expect_equal(attr(fit,"logLik"),3*actuar::dpoisinvgauss(0,2,3,log=TRUE))
})
test_that("all fixed subsets agree with accurate actuar oracle", {
  x <- c(0,0,0,0,1,2,3,5,10)
  for (held in list(list(),list(mean=3),list(shape=3),list(mean=3,shape=3))) {
    fit <- do.call(mlpig,c(list(x=x),held))
    expect_equal(sort(attr(fit,"fixed")),sort(if (is.null(names(held))) character() else names(held)))
    expect_equal(attr(fit,"logLik"),sum(actuar::dpoisinvgauss(x,fit[["mean"]],fit[["shape"]],log=TRUE)),tolerance=1e-9)
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    free <- setdiff(c("mean","shape"),names(held))
    if (length(free)) {
      objective <- function(z) {
        pars <- as.list(setNames(as.numeric(fit),names(fit)))
        pars[free] <- exp(z)
        -sum(do.call(actuar::dpoisinvgauss,c(list(x=x,log=TRUE),pars)))
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

test_that("a tiny finite overdispersion gain loses the mandated face tie", {
  x <- c(rep(0,10002),rep(2,10000),3)
  expect_gt(mean((x-mean(x))^2),mean(x))
  expect_error(mlpig(x),"Use `mlpois`")
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
