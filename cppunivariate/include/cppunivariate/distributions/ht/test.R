# Independent oracle: ordinary reference density, joint log-parameter optim,
# multiple starts; no production scale profile is used.
ht_oracle <- function(x, sigma = NULL) {
  objective <- function(p) -sum(extraDistr::dht(x, exp(p[1]), if(is.null(sigma)) exp(p[2]) else sigma, log=TRUE))
  starts <- if (is.null(sigma)) expand.grid(log(c(.02,.1,.5,2,10,100)),log(c(.1,1,10)*median(x))) else matrix(log(c(.02,.1,.5,2,10,100)),ncol=1)
  fits <- lapply(seq_len(nrow(starts)),function(i) optim(as.numeric(starts[i,]), objective, method=if(is.null(sigma)) "Nelder-Mead" else "BFGS", control=list(maxit=4000,reltol=1e-12)))
  fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
}
test_that("half-t global profile agrees with independent multistart density oracle (mu held at 0)", {
  set.seed(213)
  samples <- list(c(.12773,12.5979,3.91782,2.58658,.138212), c(.001,.01,.1,1,10,100), extraDistr::rht(80,.5,2), extraDistr::rht(100,3,1), c(.0001,.2,.3,.4,5,1000))
  for(x in samples) {
    f <- mlht(x, mu = 0); o <- ht_oracle(x)
    expect_equal(attr(f,"logLik"), -o$value, tolerance=1e-8)
    expect_equal(log(unname(f[c("nu","sigma")])),o$par,tolerance=2e-4)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
    expect_equal(mlht(x*3, mu = 0)[["nu"]], f[["nu"]], tolerance=1e-5)
    expect_equal(mlht(x*3, mu = 0)[["sigma"]], 3*f[["sigma"]], tolerance=1e-5)
  }
})
test_that("half-t held subsets have their own existence conditions (mu held at 0)", {
  x <- c(0,1,2,10)
  for(nu in c(.5,1,3,100)) {
    f <- mlht(x,mu=0,nu=nu)
    o <- optimize(function(t) -sum(extraDistr::dht(x,nu,exp(t),log=TRUE)),c(-20,20),tol=1e-11)
    expect_equal(log(f[["sigma"]]),o$minimum,tolerance=1e-6)
    expect_setequal(attr(f,"fixed"),c("mu","nu"))
    expect_equal(attr(logLik(f),"df"),1L)
  }
  f <- mlht(c(.001,.01,.1,1,10,100),mu=0,sigma=2)
  o <- ht_oracle(c(.001,.01,.1,1,10,100),sigma=2)
  expect_equal(attr(f,"logLik"),-o$value,tolerance=1e-8)
  expect_equal(f[["sigma"]],2)
  expect_setequal(attr(f,"fixed"),c("mu","sigma"))
  f <- mlht(c(0,0,0),mu=0,nu=1,sigma=2)
  expect_setequal(attr(f,"fixed"),c("mu","nu","sigma"))
  expect_equal(attr(logLik(f),"df"),0L)
  expect_equal(attr(f,"logLik"),sum(extraDistr::dht(c(0,0,0),1,2,log=TRUE)))
  # Zeros are safe when scale is held and df alone is estimated.
  f <- mlht(c(0,.001,100),mu=0,sigma=1)
  expect_true(f[["nu"]]>0)
  expect_error(mlht(c(0,1,2),mu=0),"unbounded.*sigma -> 0")
  expect_error(mlht(c(0,1),mu=0,nu=1),"finite unattained")
  expect_error(mlht(c(0,1),mu=0,nu=.5),"unbounded")
  expect_error(mlht(c(0,0),mu=0,nu=3),"unbounded")
  expect_error(mlht(c(1,1,1),mu=0),"mlhnorm\\(x, mu = 0\\)")
  expect_error(mlht(c(0,0),mu=0,sigma=1),"mlhnorm\\(x, mu = 0, sigma = 1\\)")
  for(v in c(0,-1,Inf,NaN)) {
    expect_error(mlht(c(1,2),mu=0,nu=v),"positive finite")
    expect_error(mlht(c(1,2),mu=0,sigma=v),"positive finite")
  }
  expect_error(mlht(c(-1,2)),"requires `mu`")
  expect_error(mlht(c(-1,2),mu=0),"at least the held mu")
  expect_error(mlht(c(1,Inf),mu=0))
  expect_error(mlht(numeric(),mu=0))
  expect_error(mlht(1,mu=0,nu=1e-300),"failed")
})
test_that("half-t reference tails and seeded RNG", {
  f <- mlht(c(1,2),mu=0,nu=3,sigma=2)
  expect_equal(dml(c(-1,0,1,10),f),extraDistr::dht(c(-1,0,1,10),3,2))
  for(lower in c(TRUE,FALSE)) for(logs in c(TRUE,FALSE)) {
    u <- c(0,.001,.5,.999,1)
    expect_equal(pml(qml(if(logs)log(u)else u,f,lower,logs),f,lower),u,tolerance=1e-10)
  }
  for(lower in c(TRUE,FALSE)) for(lp in if(lower) -100 else c(-100,-1000))
    expect_equal(pml(qml(lp,f,lower,TRUE),f,lower,TRUE),lp,tolerance=1e-10)
  expect_true(is.finite(dml(1e200,f,log=TRUE)))
  set.seed(7); a <- rml(30,f)
  set.seed(7); expect_identical(a,extraDistr::rht(30,3,2))
  set.seed(1); boot <- bootstrapml(f,reps=3,reducer=identity)
  expect_true(all(boot["nu",]==3 & boot["sigma",]==2))
})

test_that("half-t d/p/q/r shift with a nonzero held threshold", {
  f <- mlht(c(1,2)+5, mu = 5, nu = 3, sigma = 2)
  expect_identical(f[["mu"]], 5)
  expect_equal(dml(c(-1,0,1,10)+5,f),extraDistr::dht(c(-1,0,1,10),3,2))
  set.seed(7); a <- rml(30,f)
  set.seed(7); expect_identical(a, 5+extraDistr::rht(30,3,2))
})

test_that("half-t free threshold is the sample minimum", {
  set.seed(777)
  x <- c(5,7,9,20,2)
  f <- mlht(x, mu = NULL)
  expect_identical(unname(f["mu"]), 2)
  o <- optim(c(log(3),log(2)), function(p) -sum(extraDistr::dht(x-2, exp(p[1]), exp(p[2]), log=TRUE)),
             method = "L-BFGS-B", lower=c(0,-30), upper=c(log(1e6),30), control = list(maxit=4000, factr=1))
  expect_lte(-o$value, attr(f,"logLik")+1e-8*length(x))
  expect_equal(sum(dml(x,f,log=TRUE)), attr(f,"logLik"), tolerance=1e-8)
  expect_error(mlht(x, mu = 2.5), "at least the held mu")
})

test_that("half-t free threshold, nu held, reduces to the held-mu model", {
  x <- c(2,2,3,5,12)
  f <- mlht(x, mu = NULL, nu = 3)
  g <- mlht(x-2, mu = 0, nu = 3)
  expect_equal(f[["mu"]], 2)
  expect_equal(f[["sigma"]], g[["sigma"]])
  expect_equal(attr(f,"logLik"), attr(g,"logLik"))
})

test_that("half-t free threshold restricts free nu even with held sigma", {
  x <- c(4,4,4,4,9) # k = 4 of 5 at the minimum.
  f <- mlht(x, mu = NULL, sigma = 2)
  expect_equal(f[["mu"]], 4)
  expect_equal(f[["sigma"]], 2)
  expect_gte(f[["nu"]], 1)
  expect_equal(attr(f, "logLik"), sum(extraDistr::dht(x-min(x), f[["nu"]], 2, log=TRUE)))
})

test_that("half-t free threshold, nu and sigma both free: tie multiplicity vs n", {
  f <- suppressWarnings(mlht(c(5,5,7,9,20), mu = NULL), classes="univariateML_restricted")
  expect_equal(f[["mu"]], 5)
  expect_gte(f[["nu"]], 1)
  o <- optim(c(log(f[["nu"]]),log(f[["sigma"]])),
             function(p) -sum(extraDistr::dht(c(5,5,7,9,20)-5,exp(p[1]),exp(p[2]),log=TRUE)),
             method="L-BFGS-B", lower=c(0,-30), upper=c(log(1e6),30), control=list(maxit=4000,factr=1))
  expect_lte(-o$value, attr(f,"logLik")+1e-8*5)
  expect_error(mlht(c(3,3,8,20), mu = NULL), "finite unattained")
  expect_error(mlht(c(3,3,3,8), mu = NULL), "unbounded")
})

test_that("half-t restricted free-threshold fit warns and carries the restricted attribute", {
  # Independently verified (see notes/evaluations/2026-10-29-p4-report.md): the
  # unconstrained profile over nu peaks strictly between the collapse break-even
  # k/(n-k) = 2/3 and 1, so the nu >= 1 restriction is active here and the
  # restricted fit lands exactly at nu = 1.
  f <- suppressWarnings(mlht(c(5,5,7,9,20), mu = NULL), classes = "univariateML_restricted")
  expect_identical(f[["nu"]], 1)
  expect_warning(mlht(c(5,5,7,9,20), mu = NULL), class = "univariateML_restricted")
  expect_identical(unname(attr(f, "restricted")["nu"]), "lower")
})

test_that("half-t free threshold escapes to the half-normal", {
  x <- 2 + (seq_len(300)-.5)/300
  expect_error(mlht(x, mu = NULL), "mlhnorm\\(x, mu = NULL\\)")
})

test_that("bootstrapml replays a held or estimated ht mu", {
  set.seed(9); x <- 3+extraDistr::rht(50,4,1)
  held <- mlht(x, mu = 3, nu = 4)
  boot <- bootstrapml(held, reps = 5, reducer = identity)
  expect_true(all(boot["mu",] == 3))
  free <- mlht(x, mu = NULL, nu = 4)
  boot2 <- suppressWarnings(bootstrapml(free, reps = 5, reducer = identity), classes="univariateML_restricted")
  expect_true(all(is.finite(boot2["mu",])))
})

test_that("translated half-t restricted fits agree with independent joint multistart oracles", {
  for (x in list(c(2,5,7,9,20),c(5,5,7,9,20),c(-3,-2.99,-2.9,-2,7,97))) {
    f <- suppressWarnings(mlht(x,mu=NULL),classes="univariateML_restricted")
    # Reference density only; optimize mu jointly under its support bound.
    objective <- function(p) -sum(extraDistr::dht(x-p[1],exp(p[2]),exp(p[3]),log=TRUE))
    starts <- expand.grid(mu=c(min(x),min(x)-1),nu=log(c(1,3,20)),sigma=log(c(1,5)))
    fits <- lapply(seq_len(nrow(starts)),function(i)
      optim(as.numeric(starts[i,]),objective,method="L-BFGS-B",
        lower=c(-Inf,0,-30),upper=c(min(x),log(1e6),30),
        control=list(maxit=4000,factr=1)))
    best <- min(vapply(fits,`[[`,0.0,"value"))
    expect_equal(as.numeric(logLik(f)),-best,tolerance=1e-7)
    expect_equal(f[["mu"]],min(x))
    expect_gte(f[["nu"]],1)
  }
  # A held shape below one is evaluated/estimated exactly, as R2 requires.
  x <- c(0,1,3,10,30)
  f <- mlht(x,mu=NULL,nu=.5)
  expect_identical(f[["nu"]],.5)
  expect_length(attr(f,"restricted"),0L)
  expect_equal(as.numeric(logLik(f)),sum(extraDistr::dht(x,.5,f[["sigma"]],log=TRUE)))
})

test_that("half-t compares the half-normal face even at an exact half-minimum tie", {
  # At y=(0,0,1,1), the normal face exceeds the nu=1 scale-zero face.
  y <- c(0,0,1,1)
  normal_ll <- sum(log(2)+dnorm(y,sd=sqrt(mean(y^2)),log=TRUE))
  boundary_ll <- length(y)*log(2/pi)
  expect_gt(normal_ll,boundary_ll)
  expect_error(mlht(y+3,mu=NULL),"mlhnorm")
})
