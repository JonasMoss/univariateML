test_that("nhyper design, fixed subsets and errors are explicit", {
  expect_error(mlnhyper(3, N=20), "requires `r`")
  expect_error(mlnhyper(3, r=2), "requires `N`")
  expect_error(mlnhyper(3, r=2, N=20, m=NA_real_), "not NA")
  for (args in list(list(r=-1,N=20), list(r=21,N=20), list(r=2,N=2^53),
                    list(r=2,N=20,m=1), list(r=2,N=20,m=21)))
    expect_error(do.call(mlnhyper,c(list(c(3,4)),args)), "whole numbers")
  expect_error(mlnhyper(c(1,3),r=2,N=20), "positive probability")
  expect_error(mlnhyper(21,r=2,N=20), "positive probability")
  expect_error(mlnhyper(5,r=2,N=20,m=19), "positive probability")
  expect_error(mlnhyper(1,r=0,N=20), "positive probability")
  expect_error(mlnhyper(3.5,r=2,N=20), "Non-integral")
  a <- mlnhyper(c(3,4,5),r=2,N=20)
  expect_identical(names(a),c("m","N","r"))
  expect_equal(attr(a,"fixed"),c("N","r"))
  expect_equal(attr(logLik(a),"df"),1)
  b <- mlnhyper(c(3,4,5),r=2,N=20,m=8)
  expect_equal(attr(logLik(b),"df"),0)
  expect_equal(attr(b,"fixed"),c("m","N","r"))
  expect_equal(attr(b,"logLik"),sum(extraDistr::dnhyper(c(3,4,5),12,8,2,log=TRUE)))
  expect_warning(z <- mlnhyper(c(0,0),r=0,N=20),"unidentified")
  expect_equal(as.numeric(z),c(0,20,0))
  expect_equal(attr(z,"logLik"),0)
  expect_equal(attr(mlnhyper(0,r=0,N=0),"logLik"),0)
  expect_equal(unname(mlnhyper(c(2,2),r=2,N=20)["m"]),20)
})

test_that("nhyper integer MLE agrees with independent reference enumeration", {
  set.seed(891)
  ties <- 0
  for (i in seq_len(120)) {
    N <- sample(5:45,1); r <- sample(1:N,1); m <- r + sample.int(N-r+1,1)-1
    x <- if (m == N) rep(r,7) else extraDistr::rnhyper(7,N-m,m,r)
    a <- suppressWarnings(mlnhyper(x,r=r,N=N))
    grid <- r:(N-max(x)+r)
    ll <- vapply(grid,function(m) {
      if (m == N) return(if (all(x == r)) 0 else -Inf)
      sum(extraDistr::dnhyper(x,N-m,m,r,log=TRUE))
    },0)
    expect_equal(attr(a,"logLik"),max(ll),tolerance=1e-11)
    expect_true(unname(a["m"]) %in% grid[ll >= max(ll)-1e-10])
    ties <- ties + (sum(abs(ll-max(ll))<1e-10)>1)
  }
  # Single observation: ratio is one at m = r*(N+1)/x.
  expect_warning(a <- mlnhyper(6,r=2,N=20),"tied")
  expect_equal(unname(a["m"]),6)
})

test_that("nhyper reference repairs preserve tails and total-draw endpoints", {
  fit <- mlnhyper(c(3,4,5),r=2,N=20,m=8)
  x <- 2:14
  expect_equal(dml(x,fit),extraDistr::dnhyper(x,12,8,2),tolerance=1e-13)
  expect_equal(pml(x,fit),extraDistr::pnhyper(x,12,8,2),tolerance=1e-13)
  expect_equal(qml(c(.01,.4,.9),fit),extraDistr::qnhyper(c(.01,.4,.9),12,8,2))
  set.seed(42); a <- rml(30,fit)
  set.seed(42); b <- extraDistr::rnhyper(30,12,8,2)
  expect_equal(a,b)
  d <- univariateML_to_function(fit,"d"); p <- univariateML_to_function(fit,"p")
  q <- univariateML_to_function(fit,"q"); rr <- univariateML_to_function(fit,"r")
  expect_equal(d(3,1e12,1e12,3,log=TRUE),log(.5^3),tolerance=1e-11)
  expect_equal(d(1003,1000,1000,3,log=TRUE),dhyper(3,1000,1000,1003,log=TRUE)+log(3/1003))
  expect_equal(p(500,1000,1000,3,lower.tail=FALSE,log.p=TRUE),phyper(2,1000,1000,500,log.p=TRUE))
  expect_equal(q(-1e-100,12,8,2,log.p=TRUE),14)
  expect_equal(q(-1e-100,12,8,2,lower.tail=FALSE,log.p=TRUE),2)
  expect_equal(q(-1000,12,8,2,log.p=TRUE),2)
  expect_equal(q(-1000,12,8,2,lower.tail=FALSE,log.p=TRUE),14)
  expect_equal(p(14,12,8,2,lower.tail=FALSE,log.p=TRUE),-Inf)
  expect_equal(d(2,0,20,2),1)
  expect_equal(p(c(1,2),0,20,2),c(0,1))
  expect_equal(q(c(0,.5,1),0,20,2),c(2,2,2))
  expect_equal(rr(3,0,20,2),c(2,2,2))
  expect_equal(d(c(0,1),0,0,0),c(1,0))
  expect_equal(p(c(-1,0),0,0,0),c(0,1))
  expect_equal(q(c(0,.5,1),0,0,0),c(0,0,0))
  expect_equal(rr(3,0,0,0),c(0,0,0))
  expect_equal(sum(d(x,12,8,2)),1)
  expect_equal(p(x,12,8,2)+p(x,12,8,2,lower.tail=FALSE),rep(1,length(x)))
})


test_that("nhyper growing-urn limit counts black draws after subtracting r", {
  fit <- mlnhyper(c(3,4,5),r=3,N=2e12,m=1e12)
  x <- 3:30
  expect_equal(dml(x,fit,log=TRUE),dnbinom(x-3,size=3,prob=.5,log=TRUE),tolerance=1e-9)
  nb <- mlnbinom(x-3,size=3)
  expect_equal(unname(nb["prob"]),3/mean(x))
  expect_equal(attr(logLik(nb),"df"),1)
})

test_that("nhyper explicit NULL gives a reason and bootstrap replays held N", {
  x <- c(3,4,5)
  expect_error(mlnhyper(x,N=NULL,r=2), "growing-urn limit")
  expect_error(mlnhyper(x,N=NULL,r=2,m=8), "Estimating N with m held")
  expect_error(mlnhyper(x,N=20,r=NULL), "Estimating r")
  for (args in list(list(),list(m=8))) {
    f <- do.call(mlnhyper,c(list(x=x,N=20,r=2),args))
    set.seed(132)
    b <- suppressWarnings(bootstrapml(f,reps=4,reducer=identity))
    expect_equal(unname(b["N",]),rep(20,4))
    expect_equal(unname(b["r",]),rep(2,4))
    if(length(args)) expect_equal(unname(b["m",]),rep(8,4))
  }
})

test_that("free stopping count matches the reference finite grid", {
  for (N in c(12, 25, 50)) for (m in c(3, 6, 9)) {
    x <- c(3, 4, 5, 4)
    grid <- max(1, max(x) - (N - m)):min(m, min(x))
    ll <- vapply(grid, function(r) sum(extraDistr::dnhyper(x, N - m, m, r, log = TRUE)), 0.0)
    fit <- suppressWarnings(mlnhyper(x, m = m, N = N, r = NULL))
    expect_equal(as.numeric(logLik(fit)), max(ll), tolerance = 1e-10)
    expect_true(unname(fit["r"]) %in% grid[abs(ll - max(ll)) < 1e-10])
    expect_setequal(attr(fit, "fixed"), c("m", "N"))
  }
  expect_warning(a <- mlnhyper(3, m = 7, N = 20, r = NULL), "tied")
  expect_equal(unname(a["r"]), 1)
  expect_equal(unname(mlnhyper(0, m = 7, N = 20, r = NULL)["r"]), 0)
  expect_error(mlnhyper(c(0, 1), m = 7, N = 20, r = NULL), "positive probability")
})
