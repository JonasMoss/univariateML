sn_oracle <- function(x, held = list()) {
  nll <- function(t) {
    p <- c(xi = t[1], omega = exp(t[2]), alpha = t[3])
    p[names(held)] <- unlist(held)
    -sum(dsn(x, p[1], p[2], p[3], log = TRUE))
  }
  free <- setdiff(1:3, match(names(held), c('xi', 'omega', 'alpha')))
  best <- Inf
  for (a in c(-20, -5, -1, 0, 1, 5, 20)) {
    start <- c(mean(x) - sign(a)*sd(x)*0.8, log(sd(x)*1.5), a)
    f <- function(v) { t <- start; t[free] <- v; nll(t) }
    result <- optim(start[free], f, method = if (length(free)==1L) 'BFGS' else 'Nelder-Mead',
                    control = list(maxit = 3000, reltol = 1e-12))
    result <- optim(result$par, f, method = 'BFGS',
                    control = list(maxit = 1000, reltol = 1e-12))
    best <- min(best, result$value)
  }
  -best
}

test_that('sn density and CDF integrate and reflect, including half-normal regimes', {
  for (a in c(-100, -5, -1, 0, 1, 5, 100)) {
    expect_equal(integrate(function(x) dsn(x, alpha=a), -Inf, Inf)$value, 1, tolerance=1e-8)
    for (q in c(-3,-0.1,0,0.1,3)) {
      ref <- integrate(function(x) dsn(x, alpha=a), -Inf, q, rel.tol=1e-10)$value
      expect_equal(psn(q, alpha=a), ref, tolerance=1e-9)
      expect_equal(psn(q, alpha=a), psn(-q, alpha=-a, lower.tail=FALSE), tolerance=1e-12)
    }
  }
  expect_equal(dsn(c(-1,1), alpha=1e100), c(0,2*dnorm(1)))
  expect_equal(psn(1,alpha=1e100), 2*pnorm(1)-1)
  expect_equal(psn(0,alpha=1e100,log.p=TRUE), -log(pi)-log(1e100), tolerance=1e-10)
  expect_equal(dsn(1,alpha=0,log=TRUE), dnorm(1,log=TRUE))
  expect_equal(dsn(1e308,xi=-1e308,omega=1e308,alpha=0,log=TRUE),dnorm(2,log=TRUE)-log(1e308))
  expect_equal(dsn(1e308,xi=-1e308),0)
  expect_true(is.finite(psn(1e-100,alpha=1e100,log.p=TRUE)))
  expect_equal(psn(10,log.p=TRUE),pnorm(10,log.p=TRUE),tolerance=1e-12)
  expect_lt(psn(10,log.p=TRUE),0)
  expect_lt(psn(0,alpha=1e100,lower.tail=FALSE,log.p=TRUE),0)
  expect_equal(psn(qsn(-200,alpha=1e100,log.p=TRUE),alpha=1e100,log.p=TRUE),-200,tolerance=1e-8)
})

test_that('sn inverse inequalities and logarithmic extreme tails', {
  for (a in c(-1e8,-10,0,10,1e8)) for (lower in c(TRUE,FALSE)) {
    lp <- c(-1000,-100,-20,-2,-0.01)
    q <- qsn(lp,alpha=a,lower.tail=lower,log.p=TRUE)
    expect_equal(psn(q,alpha=a,lower.tail=lower,log.p=TRUE),lp,tolerance=1e-8)
    before <- psn(q-1e-7,alpha=a,lower.tail=lower,log.p=TRUE)
    after <- psn(q+1e-7,alpha=a,lower.tail=lower,log.p=TRUE)
    expect_true(all(if(lower) before<=lp else before>=lp))
    expect_true(all(if(lower) after>=lp else after<=lp))
  }
  expect_equal(qsn(c(0,1)), c(-Inf,Inf))
  expect_equal(qsn(c(0,1),lower.tail=FALSE), c(Inf,-Inf))
})

test_that('sn vectorisation, invalid input, NA and seeded RNG follow stats', {
  expect_equal(dsn(c(0,1,2),xi=c(0,1)), c(dsn(0),dsn(1,1),dsn(2)))
  expect_identical(dsn(numeric()), numeric())
  expect_identical(rsn(3,omega=numeric()), numeric())
  expect_true(is.na(psn(NA_real_)))
  expect_true(is.nan(dsn(NaN)))
  expect_warning(expect_true(is.nan(dsn(0,omega=-1))), 'NaNs')
  expect_warning(expect_true(is.nan(qsn(2))), 'NaNs')
  expect_warning(expect_true(is.nan(rsn(1,alpha=Inf))), 'NaNs')
  set.seed(81); a <- rsn(100,2,3,5)
  set.seed(81); expect_identical(rsn(100,2,3,5),a)
  set.seed(81); u <- rnorm(200); ref <- 2+3*(5*abs(u[seq(1,200,2)])+u[seq(2,200,2)])/sqrt(26)
  expect_equal(a,ref,tolerance=1e-14)
})

test_that('sn all fixed subsets agree with slow multistart exact-density oracle', {
  set.seed(219); x <- rsn(100,alpha=3)
  for (mask in 0:7) {
    held <- list(xi=0,omega=1,alpha=3)[as.logical(intToBits(mask)[1:3])]
    fit <- do.call(mlsn,c(list(x=x),held))
    expect_equal(unname(attr(fit,'logLik')),sum(dsn(x,fit[1],fit[2],fit[3],log=TRUE)),tolerance=1e-8)
    expect_equal(sort(attr(fit,'fixed')),sort(names(held)))
    expect_equal(attr(logLik(fit),'df'),3-length(held))
    if (mask<7) expect_gte(unname(attr(fit,'logLik')),sn_oracle(x,held)-2e-5)
    for (nm in names(held)) expect_equal(unname(fit[nm]),held[[nm]],tolerance=1e-9)
  }
})

test_that('sn symmetry is stationary but does not trap the global search', {
  set.seed(219); x <- rsn(100,alpha=3)
  normal <- mlsn(x,alpha=0)
  expect_equal(sum((x-normal[1])/normal[2]),0,tolerance=1e-9)
  f <- mlsn(x)
  expect_gt(abs(f[3]),1)
  expect_gt(attr(f,'logLik'),attr(normal,'logLik')+1)
  prof <- vapply(c(-0.001,0.001),function(a) attr(mlsn(x,alpha=a),'logLik'),0.0)
  expect_lt(abs(diff(prof)/0.002),1e-4)
})

test_that('sn detects both exact half-normal faces and respects fixed location', {
  x <- c(0,0.1,0.2,0.4,3)
  expect_error(mlsn(x),'use `mlhnorm`')
  expect_error(mlsn(-x),'on `-x`')
  expect_gte(as.numeric(logLik(mlhnorm(x, mu = NULL))),sn_oracle(x)-1e-5)
  expect_gte(as.numeric(logLik(mlhnorm(x, mu = NULL))),sn_oracle(-x)-1e-5)
  expect_error(mlsn(x,xi=-1),'use `mlhnorm`')
  expect_error(mlsn(x,xi=0),'use `mlhnorm`')
  expect_equal(as.numeric(mlsn(rep(1,5),xi=1,omega=2)),c(1,2,0))
  fixed <- mlsn(rep(1,5),xi=0,omega=2,alpha=3)
  expect_equal(attr(fixed,'logLik'),5*dsn(1,0,2,3,log=TRUE))
  expect_error(mlsn(x,omega=2),'use `mlhnorm`')
  f <- mlsn(x,xi=0.2)
  expect_true(all(is.finite(f)))
  f <- mlsn(x,xi=0,omega=2,alpha=1)
  expect_equal(attr(f,'logLik'),sum(dsn(x,0,2,1,log=TRUE)))
  expect_error(mlsn(rep(1,5)),'distinct')
  expect_error(mlsn(x,omega=0),'positive')
})

test_that('sn package density routes through fitted object operations', {
  set.seed(219); x <- rsn(100,alpha=3); f <- mlsn(x)
  expect_equal(dml(x,f),dsn(x,f[1],f[2],f[3]))
  expect_equal(pml(x,f),psn(x,f[1],f[2],f[3]))
  expect_equal(qml(c(0.2,0.8),f),qsn(c(0.2,0.8),f[1],f[2],f[3]))
  set.seed(6); a <- rml(10,f); set.seed(6); expect_equal(a,rsn(10,f[1],f[2],f[3]))
  held <- mlsn(x,alpha=0)
  set.seed(10); b <- bootstrapml(held,reps=3,reducer=identity)
  expect_equal(dim(b),c(3L,3L))
  expect_equal(unname(b['alpha',]),rep(0,3))
  expect_s3_class(model_select(x,models='sn',alpha=0),'univariateML')
  path <- tempfile(fileext='.pdf'); grDevices::pdf(path)
  for (kind in c('d','p','q')) expect_silent(plot(f,kind=kind))
  grDevices::dev.off(); expect_gt(file.info(path)$size,0)
  unlink(path)
})

test_that('sn optional reference density agrees with sn package', {
  skip_if_not_installed('sn')
  for (a in c(-100,-3,0,3,100)) {
    x <- seq(-5,5,length.out=51)
    expect_equal(dsn(x,1,2,a,log=TRUE),sn::dsn(x,xi=1,omega=2,alpha=a,log=TRUE),tolerance=1e-9)
  }
})

test_that("sn: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "sn", "golden.json"))
  x <- g$data
  expect_error(mlsn(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("sn: P3 unfinished fixed-shape inner fit stops", {
  expect_error(mlsn(c(-2,-.3,.1,.8,4),alpha=2,iterlim=1), "did not converge")
})
