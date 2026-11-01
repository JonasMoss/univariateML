context("mllomax")

## Data generation.
set.seed(313)
tiny_data <- rlomax(n = 10, lambda = 1, kappa = 7)
small_data <- rlomax(n = 100, lambda = 10, kappa = 3)
medium_data <- rlomax(n = 1000, lambda = 1/2, kappa = 2)
large_data <- rlomax(n = 10000, lambda = 20, kappa = 13)

## Checks if the ML is correct.
mle2 <- suppressWarnings(nlm(function(p) {
  -sum(dlomax(x = small_data, lambda = p[1], kappa = p[2], log = TRUE))
}, p = c(1, 1))$estimate)

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(dlomax(x = medium_data, lambda = p[1], kappa = p[2], log = TRUE))
}, p = c(1, 1))$estimate)

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(dlomax(x = large_data, lambda = p[1], kappa = p[2], log = TRUE))
}, p = c(1, 1))$estimate)

expect_equal(mle2, as.numeric(mllomax(small_data)), tolerance = 1e-3)
expect_equal(mle3, as.numeric(mllomax(medium_data)), tolerance = 1e-3)
expect_equal(mle4, as.numeric(mllomax(large_data)), tolerance = 1e-3)

## Checks warning with small iterlim.
## expect_warning(mllomax(small_data, iterlim = 1))

## Error when the MLE does not exist.
expect_error(suppressWarnings(mllomax(airquality$Wind)))
expect_error(suppressWarnings(mllomax(tiny_data, start = median(tiny_data))))

## Finds errors with na and data out of bounds.
expect_error(mllomax(c(tiny_data, NA)))
expect_error(mllomax(c(0, tiny_data)))
expect_error(
  mllomax(rep(1, 5)),
  "Use `mlexp`"
)

## Checks that na.rm works as intended.
expect_equal(
  coef(mllomax(small_data)),
  coef(mllomax(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllomax(small_data, na.rm = TRUE)
expect_equal(
  sum(dlomax(x = small_data, lambda = est[1], kappa = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Lomax")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Control arguments use the CRAN names (lambda0, reltol, iterlim).
fit_default <- mllomax(medium_data)
fit_start <- mllomax(medium_data, lambda0 = 3 * coef(fit_default)[["lambda"]], reltol = 1e-12)
expect_equal(coef(fit_start), coef(fit_default), tolerance = 1e-6)
expect_error(mllomax(medium_data, lambda0 = -1), "`lambda0` must be a positive finite number")
expect_error(mllomax(medium_data, tol = 1e-6))

test_that("lomax fitting respects extreme scale changes", {
  set.seed(1)
  x <- ml_simulate("mllomax", 50)
  f <- mllomax(x)
  for (s in c(1e-300,1e300)) {
    g <- mllomax(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-6)
  }
})

test_that("Lomax moment equality can have an attained fit or exponential escape", {
  for(x in list(c(1,1,40,60),c(1,1,1,1,20,20,20,36))) {
    expect_equal(mean(x^2),2*mean(x)^2)
    f <- mllomax(x)
    profile <- function(t) {l<-exp(t); S<-sum(log1p(l*x)); k<-length(x)/S; length(x)*(log(k)+t-1)-S}
    o <- optimize(profile,c(-15,5),maximum=TRUE,tol=1e-12)
    expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-9)
  }
  expect_error(mllomax(c(1,1,4,12)),"ml exp|mlexp|ml.*exp")
})

test_that("a negative Lomax boundary score need not imply global escape", {
  x <- c(1,1,40,59)
  expect_lt(mean(x^2),2*mean(x)^2)
  f <- mllomax(x)
  profile <- function(t) {l<-exp(t); S<-sum(log1p(l*x)); k<-length(x)/S; length(x)*(log(k)+t-1)-S}
  o <- optimize(profile,c(-10,5),maximum=TRUE,tol=1e-12)
  expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-9)
  expect_gt(attr(f,"logLik"),-length(x)*(log(mean(x))+1))
})

test_that("unresolvable Lomax starting hints fall back to the moment start", {
  x<-c(1,1,2,3,100)
  base<-mllomax(x)
  for(start in c(1e-300,1e300)) {
    f<-mllomax(x,lambda0=start)
    expect_equal(as.numeric(f),as.numeric(base),tolerance=1e-10)
  }
})

test_that("Lomax exact criterion resolves nonlocal maxima", {
  samples <- list(c(1,100),c(.001,4,6),c(.001,1,2),c(1,29),c(1,1,1,12,12))
  gaps <- c(.590084,1.338305,.652423,.001924,1.90927e-4)
  rates <- c(.772199,1394.08,1039.29,.236987,.0359623)
  shapes <- c(.405536,.161888,.196081,.878708,6.07162)
  for (j in seq_along(samples)) {
    x <- samples[[j]]; f <- mllomax(x,reltol=1e-12)
    expect_equal(as.numeric(f),c(rates[j],shapes[j]),tolerance=5e-6)
    expect_equal(attr(f,"logLik")+length(x)*(log(mean(x))+1),gaps[j],tolerance=5e-7)
    for (s in c(1e-200,1e200)) {
      g <- mllomax(x*s,reltol=1e-12)
      expect_equal(as.numeric(g)*c(s,1),as.numeric(f),tolerance=1e-6)
    }
    g <- mllomax(rep(x,50),reltol=1e-12)
    expect_equal(as.numeric(g),as.numeric(f),tolerance=1e-6)
    expect_equal(attr(g,"logLik")/50,attr(f,"logLik"),tolerance=1e-10)
  }
  for(x in list(c(1,10),c(1,28),rep(1,5))) expect_error(mllomax(x),"mlexp")
  set.seed(5)
  expect_error(mllomax(rexp(10)),"mlexp")
})

test_that("Lomax agrees with a slow independent psi and density oracle", {
  skip_unless_tier("full")
  set.seed(4801)
  for (j in seq_len(220)) {
    n <- c(2L,3L,5L,10L,50L)[1L+(j-1L)%%5L]
    x <- if(j%%2L) rexp(n) else rlomax(n = n, lambda = 1, kappa = runif(1, 0.3, 5))
    y <- x/mean(x); logubar <- -mean(log(y)); ubar <- exp(logubar)
    # Independent existence criterion, including every grid-local minimum.
    psi <- function(u) mean(log(exp(-u)+u*y))
    ug <- sort(unique(c(exp(seq(-25,logubar,length.out=1201)),seq(0,ubar,length.out=1201)[-1])))
    pv <- vapply(ug,psi,0.)
    ids <- which(pv[-c(1,length(pv))] <= pv[-c(length(pv)-1,length(pv))] &
                 pv[-c(1,length(pv))] <= pv[-c(1,2)])+1L
    minpsi <- min(pv)
    for (i in ids) minpsi <- min(minpsi,optimize(psi,ug[c(i-1,i+1)],tol=1e-12)$objective)
    # Internal density, independently profiled in log(lambda).
    profile <- function(t) {
      q <- exp(t); kappa <- 1/mean(log1p(q*y))
      sum(dlomax(x = y, lambda = q, kappa = kappa, log = TRUE))-n*log(mean(x))
    }
    # The theoretical endpoint can exceed the double exponent range.
    # Beyond log q=600 the profile is already far below the face here.
    upper <- min(logubar+ubar,600)
    tg <- seq(-25,upper,length.out=2401)
    lv <- vapply(tg,profile,0.)
    ids <- which(lv[-c(1,length(lv))] >= lv[-c(length(lv)-1,length(lv))] &
                 lv[-c(1,length(lv))] >= lv[-c(1,2)])+1L
    oracle <- max(lv)
    for(i in ids) oracle <- max(oracle,optimize(profile,tg[c(i-1,i+1)],maximum=TRUE,tol=1e-11)$objective)
    boundary <- -n*(log(mean(x))+1); gap <- oracle-boundary
    f <- tryCatch(mllomax(x,reltol=1e-12),error=identity)
    if(gap > 1.1e-8*n) {
      expect_lt(minpsi,0)
      expect_false(inherits(f,"error"))
      if(!inherits(f,"error")) expect_gte(attr(f,"logLik"),oracle-1e-8*n)
    } else if(gap < .9e-8*n) {
      expect_true(inherits(f,"error"))
      if(inherits(f,"error")) expect_match(conditionMessage(f),"mlexp")
    }
  }
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "lomax"
  pn <- c("lambda","kappa")
  held_default <- c(1,2)
  names(held_default) <- pn
  density <- dlomax
  ref <- function(x,p) {
    suppressWarnings(do.call(density,c(list(x=x),as.list(p),list(log=TRUE))))
  }
  samples <- list(n20=ml_simulate(paste0("ml",id),20),
                  n100=ml_simulate(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)
  samples$limit <- rexp(100)
  subsets <- lapply(1:(2^length(pn)-1),function(mask)
    pn[as.logical(intToBits(mask)[seq_along(pn)])])
  for (x in samples) for (held in subsets) {
    hv <- held_default
    args <- c(list(x=x),as.list(hv[held]))
    f <- do.call(get(paste0("ml",id)),args)
    expect_identical(unname(as.numeric(f[held])),unname(hv[held]))
    expect_setequal(attr(f,"fixed"),held)
    p <- setNames(as.numeric(f),pn)
    expect_equal(as.numeric(logLik(f)),sum(ref(x,p)),tolerance=1e-8)
    free <- setdiff(pn,held)
    decode <- function(z) {
      q <- hv
      q[free] <- exp(z)
      q
    }
    cost <- function(z) {
      if(any(!is.finite(z)) || any(abs(z)>40)) return(1e100)
      v <- suppressWarnings(-sum(ref(x,decode(z))))
      if(is.finite(v)) v else 1e100
    }
    if(length(free)) {
      start <- log(hv[free])
      oracles <- lapply(c(-3,-1,0,1,3),function(d)
        optim(start+d,cost,method="BFGS",control=list(maxit=1000,reltol=1e-12))$value)
      best <- min(unlist(oracles))
    } else best <- -sum(ref(x,hv))
    expect_equal(as.numeric(logLik(f)),-best,tolerance=1e-6)
  }
  for (p in pn) for (bad in c(NA_real_,NaN,Inf,-Inf,if(p=="meanlog") numeric() else c(0,-1))) {
    expect_error(do.call(get(paste0("ml",id)),c(list(x=samples$n20),setNames(list(bad),p))))
  }
})

test_that("P2 held existence boundaries", {
expect_error(mllomax(rep(2,3)),"mlexp")
expect_true(is.finite(logLik(mllomax(rep(2,3),kappa=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mllomax(c(2,3),kappa=2,iterlim=1)))
})

test_that("lomax: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "lomax", "golden.json"))
  x <- g$data
  expect_error(mllomax(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("lomax: P3 compressed profile rejects exhaustion", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "lomax", "golden.json"))
  x <- rep(g$data, ceiling(4097/length(g$data)))
  expect_error(mllomax(x,iterlim=1,reltol=1e-10), "failed to converge")
  fit <- mllomax(x,reltol=1e-10)
  expect_equal(unname(fit["lambda"]),g$estimates$lambda,tolerance=1e-5)
  expect_equal(unname(fit["kappa"]),g$estimates$kappa,tolerance=1e-5)
})

test_that("limit detector describes its numerical comparison", {
  expect_error(mllomax(rep(1, 5)), "numerical profile search.*tolerance")
})

test_that("Lomax help describes the detector as numerical", {
  rd <- paste(readLines(file.path("..", "..", "man", "mllomax.Rd"), warn = FALSE), collapse = " ")
  rd <- gsub("[[:space:]]+", " ", rd)
  expect_match(rd, "numerical search does not establish a finite maximum", fixed = TRUE)
})
