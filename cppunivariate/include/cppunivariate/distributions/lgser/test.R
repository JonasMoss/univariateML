context("mllgser")

## Finds error with only 1s.
expect_error(
  mllgser(c(1, 1, 1)),
  "All observations equals 1; the maximum likelihood estimator does not exist\\."
)

test_that("theta stays in (0, 1) when nearly every count is one", {
  # The Lambert-W closed form lost all precision near mean(x) = 1 and
  # returned theta = -1.1e-16 for c(1, 1, 1, 2) and theta > 1 for
  # c(rep(1, 9), 2). The score equation mean(x) = -theta/((1-theta)log(1-theta))
  # must hold at the returned theta.
  for (x in list(c(1, 1, 1, 2), c(rep(1, 9), 2), c(rep(1, 1e5), 2), c(1, 2), c(1, 1e6))) {
    fit <- mllgser(x)
    theta <- fit[["theta"]]
    expect_gt(theta, 0)
    expect_lt(theta, 1)
    expect_equal(-theta / ((1 - theta) * log1p(-theta)), mean(x), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"), sum(dlgser(x = x, theta = theta, log = TRUE)), tolerance = 1e-10)
  }
})

# P2: independent multistart reference-density oracle for every nonempty hold.
local({
  id <- "lgser"
  base <- c(theta=0.6)
  density <- dlgser
  test_that("P2 holds agree with independent multistart reference likelihoods", {
    set.seed(81029)
    samples <- list(ml_simulate(paste0("ml",id),20),
                    ml_simulate(paste0("ml",id),100),
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

test_that("P2 held theta validates domain and compatibility", {
  for(v in c(0,Inf,NA_real_,NaN))expect_error(mllgser(c(1,2),theta=v))
  expect_error(mllgser(c(1,2),theta=0))
  fit<-mllgser(rep(1,3),theta=0.5)
  expect_identical(unname(as.numeric(fit)),0.5)
  expect_identical(attr(fit,"fixed"),"theta")
})
