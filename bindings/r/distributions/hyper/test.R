test_that("hyper requires its design and preserves fixed constraints", {
  expect_error(mlhyper(c(1, 2), N = 20), "requires `k`")
  expect_error(mlhyper(c(1, 2), k = 4), "requires `N`")
  expect_error(mlhyper(0, k = 2, N = NULL, m = 3), "point mass at zero")
  expect_error(mlhyper(1, k = 2, N = 2^53), "2\\^53")
  expect_error(mlhyper(1.5, k = 2, N = 10), "Non-integral")
  expect_error(mlhyper(1, k = 2, N = 10, m = NA_real_), "not NA")
  x <- c(1, 2, 3, 2)
  a <- mlhyper(x, N = 20, k = 5)
  expect_equal(mlhyper(x, FALSE, 5, 20)[1:3], a[1:3])
  expect_identical(names(a), c("m", "N", "k"))
  expect_equal(unname(a["N"]), 20)
  expect_equal(attr(a, "fixed"), c("N", "k"))
  expect_equal(attr(logLik(a), "df"), 1)
  b <- mlhyper(x, m = 8, k = 5, N = NULL)
  expect_equal(attr(b, "fixed"), c("m", "k"))
  expect_equal(attr(logLik(b), "df"), 1)
  c <- mlhyper(x, N = 20, m = 8, k = 5)
  expect_equal(attr(c, "fixed"), c("m", "N", "k"))
  expect_equal(attr(logLik(c), "df"), 0)
  for (fit in list(a, b, c)) {
    args <- list(m=fit[["m"]], n=fit[["N"]]-fit[["m"]], k=fit[["k"]])
    expect_equal(dml(x, fit, log = TRUE), do.call(dhyper, c(list(x), args, log = TRUE)))
    expect_equal(pml(x, fit), do.call(phyper, c(list(x), args)))
    expect_equal(qml(c(.1, .5, .9), fit), do.call(qhyper, c(list(c(.1, .5, .9)), args)))
    set.seed(42); actual <- rml(10, fit)
    set.seed(42); expected <- do.call(rhyper, c(list(10), args))
    expect_equal(actual, expected)
  }
})

test_that("hyper warns deterministically for ties and unidentified designs", {
  expect_warning(a <- mlhyper(5, k = 10, N = 19), "tied")
  expect_equal(unname(a["m"]), 9)
  expect_warning(b <- mlhyper(5, k = 10, N = NULL, m = 10), "tied")
  expect_equal(unname(b["N"]), 19)
  expect_warning(z <- mlhyper(c(0, 0), k = 0, N = 7), "every `m`")
  expect_equal(unname(z["m"]), 0)
  expect_warning(mlhyper(0, k = 3, N = NULL, m = 0), "Every `N`")
  fit <- mlhyper(c(0, 1, 1), k = 1, N = 10)
  grid <- 0:10
  ll <- vapply(grid, function(m) sum(dhyper(c(0, 1, 1), m, 10-m, 1, log=TRUE)), 0)
  expect_equal(unname(fit["m"]), grid[which.max(ll)])
})

test_that("hyper exact searches agree with independent finite enumeration", {
  set.seed(2901)
  for (i in seq_len(60)) {
    N <- sample(8:30, 1); m <- sample(2:(N-2), 1); k <- sample(2:(N-1), 1)
    x <- rhyper(6, m, N-m, k)
    a <- suppressWarnings(mlhyper(x, k=k, N=N))
    grid <- max(x):(N-k+min(x))
    ll <- vapply(grid, function(m) sum(dhyper(x,m,N-m,k,log=TRUE)), 0)
    expect_equal(attr(a,"logLik"), max(ll), tolerance=1e-12)
    if (sum(x) == 0) next
    b <- suppressWarnings(mlhyper(x, k=k, N=NULL, m=m))
    # Derived bound beyond which every adjacent ratio is below one.
    top <- ceiling(k*(m+k)/mean(x))+2
    grid <- (m+k-min(x)):top
    ll <- vapply(grid, function(N) sum(dhyper(x,m,N-m,k,log=TRUE)), 0)
    expect_equal(attr(b,"logLik"), max(ll), tolerance=1e-12)
  }
})

test_that("hyper explicit NULL and bootstrap retain the population design", {
  x <- c(1,2,3,2)
  expect_true(is.finite(as.numeric(logLik(mlhyper(x,N=NULL,k=5)))))
  expect_equal(attr(suppressWarnings(mlhyper(x,N=20,k=NULL)), "fixed"), "N")
  held <- mlhyper(x,N=20,k=5,m=8)
  set.seed(130)
  b <- bootstrapml(held,reps=4,reducer=identity)
  expect_equal(unname(b["N",]),rep(20,4))
  expect_equal(unname(b["m",]),rep(8,4))
  expect_equal(unname(b["k",]),rep(5,4))
  free <- mlhyper(x,N=NULL,k=5,m=8)
  set.seed(131)
  b <- suppressWarnings(bootstrapml(free,reps=4,reducer=identity))
  expect_true(all(is.finite(b["N",])))
  expect_equal(unname(b["m",]),rep(8,4))
  expect_equal(unname(b["k",]),rep(5,4))
})

test_that("free draws match the reference finite grid", {
  for (N in c(12, 20, 50)) for (m in c(2, 5, 7)) {
    x <- c(1, 2, 1, 0)
    grid <- max(x):(N - m + min(x))
    ll <- vapply(grid, function(k) sum(stats::dhyper(x, m, N - m, k, log = TRUE)), 0.0)
    fit <- suppressWarnings(mlhyper(x, m = m, N = N, k = NULL))
    expect_equal(as.numeric(logLik(fit)), max(ll), tolerance = 1e-10)
    expect_true(unname(fit["k"]) %in% grid[abs(ll - max(ll)) < 1e-10])
    expect_setequal(attr(fit, "fixed"), c("m", "N"))
  }
  expect_warning(a <- mlhyper(5, m = 10, N = 19, k = NULL), "tied")
  expect_equal(unname(a["k"]), 9)
  expect_warning(z <- mlhyper(0, m = 0, N = 7, k = NULL), "Every `k`")
  expect_equal(unname(z["k"]), 0)
  expect_error(mlhyper(4, m = 3, N = 10, k = NULL), "positive probability")
})

test_that("fully free hyper fits and reports boundary outcomes", {
  f <- mlhyper(c(2,2,2,3,3,3), N=NULL, k=NULL, m=NULL)
  expect_equal(as.numeric(logLik(f)), 6*log(.5), tolerance=1e-9)
  expect_length(attr(f, "fixed"), 0)
  expect_error(mlhyper(c(0,1),N=NULL,k=NULL,m=NULL), "matched the binomial face")
  expect_error(mlhyper(c(0,0,0,5,5,5),N=NULL,k=NULL,m=NULL), "Poisson face.*not excluded|not excluded.*Poisson face")
  expect_error(mlhyper(c(0,100001),N=NULL,k=NULL,m=NULL), "numerical failure")
  expect_error(mlhyper(c(8,8,9,9),N=NULL,k=NULL,m=NULL), "numerical failure.*budget")
})

test_that("fully free seeded fits agree with independent population grids", {
  set.seed(12112)
  for (i in 1:3) {
    x <- rhyper(40,2,2,3)
    f <- mlhyper(x,N=NULL,k=NULL,m=NULL)
    count <- tabulate(x+1L, nbins=3)
    best <- -Inf
    for (N in 2:200) for (m in 0:N) {
      k <- 0:N
      ll <- count[2]*dhyper(1,m,N-m,k,log=TRUE) + count[3]*dhyper(2,m,N-m,k,log=TRUE)
      best <- max(best, ll, na.rm=TRUE)
    }
    expect_equal(as.numeric(logLik(f)),best,tolerance=1e-9)
  }
})

 test_that("held N joint hyper grid matches stats enumeration", {
  samples <- c(list(0, rep(0,100), 4, rep(3,80), c(0,6), c(5,6,6)),
    lapply(1:20, function(i) rep((0:6)[(i %% 4 + 1):7], (i %% 5)+1)))
  for (x in samples) {
    N <- 14
    grid <- expand.grid(m=0:N,k=0:N)
    grid <- grid[order(grid$m,grid$k),]
    ll <- apply(grid,1,function(z) sum(stats::dhyper(x,z[1],N-z[1],z[2],log=TRUE)))
    best <- max(ll)
    z <- grid[which(is.finite(ll) & abs(ll-best)<=64*.Machine$double.eps*abs(best))[1],]
    f <- mlhyper(x,N=N,k=NULL)
    expect_equal(as.numeric(f),c(z$m,N,z$k))
    expect_equal(as.numeric(logLik(f)),best,tolerance=1e-10)
    expect_identical(attr(f,"fixed"),"N")
  }
  expect_error(mlhyper(c(0,15),N=14,k=NULL),"positive probability")
  expect_error(mlhyper(c(0,1),N=10000,k=NULL),"numerical failure.*budget")
 })

test_that("joint population fits agree with a finite reference grid", {
  x <- c(2,2,2,3,3,3)
  f <- mlhyper(x, k=5, N=NULL, m=NULL)
  g <- mlhyper(x, m=5, N=NULL, k=NULL)
  ll <- vapply(5:500, function(N) {
    max(vapply(0:N, function(m) sum(dhyper(x,m,N-m,5,log=TRUE)), 0.0))
  }, 0.0)
  expect_equal(as.numeric(logLik(f)), max(ll), tolerance=1e-9)
  expect_equal(as.numeric(logLik(g)), max(ll), tolerance=1e-9)
  expect_equal(as.numeric(logLik(mlhyper(c(0,1),k=1,N=NULL,m=NULL))),2*log(.5))
  expect_error(mlhyper(c(0,1),k=100001,N=NULL,m=NULL),"numerical failure")
})

test_that("seeded joint hyper fits match population grids", {
  set.seed(12101)
  for (i in 1:3) {
    x <- rhyper(100,6,6,6)
    f <- mlhyper(x,k=6,N=NULL,m=NULL)
    g <- mlhyper(x,m=6,N=NULL,k=NULL)
    best <- max(vapply(6:120,function(N)
      max(vapply(0:N,function(m) sum(dhyper(x,m,N-m,6,log=TRUE)),0.0)),0.0))
    expect_equal(as.numeric(logLik(f)),best,tolerance=1e-8)
    expect_equal(as.numeric(logLik(g)),best,tolerance=1e-8)
  }
  expect_error(mlhyper(c(0,0,0,5,5,5),k=5,N=NULL,m=NULL),"not excluded")
})
