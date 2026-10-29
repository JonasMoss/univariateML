context("mltriang")
x <- c(.02, .2, .3, .4, .45, .65, .9)
ll <- function(p, x) sum(extraDistr::dtriang(x, p[1], p[2], p[3], log = TRUE))
for (mask in 0:7) {
  held <- c("a", "b", "c")[as.logical(intToBits(mask)[1:3])]
  values <- c(a = 0, b = 1, c = .4)
  fit <- suppressWarnings(do.call(mltriang, c(list(x), as.list(values[held]))))
  expect_identical(attr(fit, "fixed"), held)
  expect_equal(attr(logLik(fit), "df"), 3L - length(held))
  expect_equal(unname(coef(fit)[held]), unname(values[held]))
  expect_equal(as.numeric(logLik(fit)), ll(fit, x), tolerance = 1e-11)
  free <- setdiff(names(values), held)
  if (length(free)) {
    objective <- function(z) {
      p <- coef(fit); p[free] <- z
      if (p[1] >= min(x) || p[2] <= max(x) || p[3] < p[1] || p[3] > p[2]) return(1e50)
      v <- suppressWarnings(ll(p, x))
      if (is.finite(v)) -v else 1e50
    }
    for (offset in c(-.02, 0, .02)) {
      oracle <- optim(coef(fit)[free] + offset, objective,
        method = if (length(free) == 1) "BFGS" else "Nelder-Mead",
        control = list(maxit = 2000, reltol = 1e-12))
      expect_lte(-oracle$value, as.numeric(logLik(fit)) + 1e-7)
    }
  }
}
expect_warning(tie <- mltriang(c(0, 1)), "not unique")
expect_equal(unname(coef(tie)), c(0, 1.5, 0), tolerance = 1e-11)
expect_error(mltriang(rep(1, 4)), "unbounded")
expect_error(mltriang(x, a = .1), "incompatible")
expect_error(mltriang(c(0, 1), a = 0, b = 1), "incompatible")
expect_error(mltriang(x, a = 2, b = 1), "a < b")
expect_error(mltriang(x, c = Inf), "finite")
expect_error(mltriang(x, c = NA_real_), "finite")
expect_error(mltriang(x, a = 0, c = -1), "a <= c")
expect_error(mltriang(x, iterlim = 0), "iterlim")
expect_equal(pml(0, tie), 0)
expect_equal(pml(0, tie, lower.tail = FALSE), 1)
expect_equal(pml(0, tie, log.p = TRUE), -Inf)
fix <- univariateML:::univariateML_reference_fixes[["extraDistr::ptriang"]]
expect_true(fix$broken())
expect_identical(formals(fix$replacement), formals(extraDistr::ptriang))
for (lower in c(TRUE, FALSE)) for (logged in c(TRUE, FALSE)) {
  q <- c(-1, 0, .1, .4, .9, 1, 2, NA_real_)
  expect_equal(fix$replacement(q, 0, 1, .4, lower, logged),
    extraDistr::ptriang(q, 0, 1, .4, lower, logged))
  expect_equal(fix$replacement(q[q > 0 & !is.na(q)], 0, 1, 0, lower, logged),
    extraDistr::ptriang(q[q > 0 & !is.na(q)], 0, 1, 0, lower, logged))
}
# Independent mode grid with a separate endpoint optim at every grid point.
fit <- mltriang(x)
for (mode in seq(0, 1, length.out = 31)) {
  oracle <- optim(c(-.1, 1.1), function(z) {
    if (z[1] >= min(x) || z[2] <= max(x) || mode < z[1] || mode > z[2]) return(1e50)
    -ll(c(z, mode), x)
  }, control = list(maxit = 1000, reltol = 1e-11))
  expect_lte(-oracle$value, as.numeric(logLik(fit)) + 1e-7)
}
# A held endpoint observed in the data forces its corner mode.
corner <- mltriang(c(0, .01, .1, .5), a = 0)
expect_equal(coef(corner)[["c"]], 0)
for (mode in c(-1, 2)) {
  fixed_mode <- mltriang(x, c = mode)
  expect_equal(coef(fixed_mode)[[if (mode < 0) "a" else "b"]], mode)
}
expect_equal(as.numeric(logLik(mltriang(rep(.4, 3), a = 0, b = 1))),
  3 * log(2))
corner_upper <- mltriang(1 - c(0, .01, .1, .5), b = 1)
expect_equal(coef(corner_upper)[["c"]], 1)
expect_warning(known_tie <- mltriang(c(.2, .8), a = 0, b = 1), "not unique")
expect_equal(coef(known_tie)[["c"]], .2)

test_that("constant triangular samples with exterior held mode", {
  for (v in c(.1,.4,1)) for (b in c(NA,3)) {
    x <- rep(v,5); args <- list(x=x,c=-1)
    if (!is.na(b)) args$b <- b
    fit <- do.call(mltriang,args)
    expect_equal(unname(fit["a"]), -1)
    expect_equal(unname(fit["b"]), if (is.na(b)) 2*v+1 else b)
    expect_equal(as.numeric(logLik(fit)), sum(extraDistr::dtriang(x,fit[1],fit[2],fit[3],log=TRUE)))
  }
})

test_that("P2 reflected constant exterior-mode maxima", {
  for (held in list(list(c=2),list(a=-1,c=2),list(a=-1,b=3,c=2))) {
    fit <- do.call(mltriang,c(list(x=rep(.4,5)),held))
    expect_equal(as.numeric(logLik(fit)),ll(fit,rep(.4,5)),tolerance=1e-12)
    if (is.null(held$b)) expect_equal(unname(fit["b"]),2)
    if (is.null(held$a)) expect_equal(unname(fit["a"]),-1.2)
  }
})

test_that("P2 triangular subsets dominate independent multistart endpoint searches", {
  set.seed(8131)
  samples <- list(extraDistr::rtriang(20,0,1,.4),extraDistr::rtriang(100,0,1,.4),
                  runif(20),c(.2,.2,.6,.8),c(.2,.8),rep(.4,5))
  values <- c(a=0,b=1,c=.4)
  for (x in samples) for (mask in 0:7) {
    held <- names(values)[as.logical(intToBits(mask)[1:3])]
    if (length(unique(x))==1 && all(values[held]==x[1])) next
    fit <- suppressWarnings(do.call(mltriang,c(list(x=x),as.list(values[held]))))
    # Conditional mode maxima occur at observations or a held mode.
    modes <- if ("c" %in% held) values["c"] else unique(x)
    best <- -Inf
    for (mode in modes) {
      free <- setdiff(c("a","b"),held)
      objective <- function(z) {
        p <- c(a=-.1,b=1.1,c=mode); p[held] <- values[held]
        for (i in seq_along(free)) p[free[i]] <- if (free[i]=="a")
          min(x,mode)-exp(z[i]) else max(x,mode)+exp(z[i])
        if (p[1]>=p[2] || p[3]<p[1] || p[3]>p[2]) return(1e100)
        v <- -sum(extraDistr::dtriang(x,p[1],p[2],p[3],log=TRUE))
        if (is.finite(v)) v else 1e100
      }
      if (!length(free)) best <- max(best,-objective(numeric())) else
        for (start in c(-3,0,2)) best <- max(best,-nlminb(rep(start,length(free)),
          objective,lower=rep(-25,length(free)),upper=rep(10,length(free)),
          control=list(iter.max=500,rel.tol=1e-11))$objective)
    }
    expect_lte(best,as.numeric(logLik(fit))+1e-6*length(x))
    # Interior slack searches only approach endpoint modes; compare against
    # the fit from below, with a small numerical allowance.
    expect_lt(as.numeric(logLik(fit))-best,1e-5*length(x))
  }
})
