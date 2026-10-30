test_that("nhyper design, fixed subsets and errors are explicit", {
  expect_error(mlnhyper(3, N=20), "requires `r`")
  expect_error(mlnhyper(3, r=2), "known population")
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
  expect_identical(names(a),c("n","m","r"))
  expect_equal(attr(a,"fixed"),c("N","r"))
  expect_equal(attr(logLik(a),"df"),1)
  b <- mlnhyper(c(3,4,5),r=2,N=20,m=8)
  expect_equal(attr(logLik(b),"df"),0)
  expect_equal(attr(b,"fixed"),c("n","m","r"))
  expect_equal(attr(b,"logLik"),sum(dnhyper(x = c(3, 4, 5), n = 12, m = 8, r = 2, log = TRUE)))
  expect_warning(z <- mlnhyper(c(0,0),r=0,N=20),"unidentified")
  expect_equal(as.numeric(z),c(20,0,0))
  expect_equal(attr(z,"logLik"),0)
  expect_equal(attr(mlnhyper(0,r=0,N=0),"logLik"),0)
  expect_equal(unname(mlnhyper(c(2,2),r=2,N=20)["m"]),20)
})

test_that("nhyper integer MLE agrees with independent reference enumeration", {
  set.seed(891)
  ties <- 0
  for (i in seq_len(120)) {
    N <- sample(5:45,1); r <- sample(1:N,1); m <- r + sample.int(N-r+1,1)-1
    x <- if (m == N) rep(r,7) else rnhyper(nn = 7, n = N - m, m = m, r = r)
    a <- suppressWarnings(mlnhyper(x,r=r,N=N))
    grid <- r:(N-max(x)+r)
    ll <- vapply(grid,function(m) {
      if (m == N) return(if (all(x == r)) 0 else -Inf)
      sum(dnhyper(x = x, n = N - m, m = m, r = r, log = TRUE))
    },0)
    expect_equal(attr(a,"logLik"),max(ll),tolerance=1e-11)
    expect_true(unname(a["m"]) %in% grid[ll >= max(ll)-1e-10])
    ties <- ties + (sum(abs(ll-max(ll))<1e-10)>1)
  }
  # Single observation: ratio is one at m = r*(N+1)/x.
  expect_warning(a <- mlnhyper(6,r=2,N=20),"tied")
  expect_equal(unname(a["m"]),6)
})

test_that("nhyper evaluation preserve tails and total-draw endpoints", {
  fit <- mlnhyper(c(3,4,5),r=2,N=20,m=8)
  x <- 2:14



  set.seed(42); a <- rml(30,fit)
  set.seed(42); b <- rnhyper(nn = 30, n = 12, m = 8, r = 2)
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
