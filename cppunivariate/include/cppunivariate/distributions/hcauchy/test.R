test_that("half-Cauchy scale agrees with independent density optimization (mu held at 0)", {
  samples <- list(c(1), c(1,4), c(0,1,2), c(.001,1,2,1000), c(1,1,1,20))
  for (x in samples) {
    f <- mlhcauchy(x, mu = 0)
    o <- optimize(function(t) -sum(dhcauchy(x = x, mu = 0, sigma = exp(t), log = TRUE)), c(-25,25), tol = 1e-11)
    expect_equal(log(f[["sigma"]]), o$minimum, tolerance = 1e-6)
    expect_equal(attr(f,"logLik"), -o$objective, tolerance = 1e-10)
    expect_equal(sum(dml(x,f,log=TRUE)), attr(f,"logLik"), tolerance = 1e-10)
  }
  expect_equal(mlhcauchy(c(1,4), mu = 0)[["sigma"]], 2)
  expect_error(mlhcauchy(c(0,1), mu = 0), "finite unattained")
  expect_error(mlhcauchy(c(0,0,1), mu = 0), "unbounded")
  expect_error(mlhcauchy(rep(0,4), mu = 0), "unbounded")
  expect_error(mlhcauchy(-1), "requires `mu`")
  expect_error(mlhcauchy(-1, mu = 0), "at least the held mu")
  expect_error(mlhcauchy(Inf, mu = 0))
  expect_equal(mlhcauchy(c(1,4)*1e200, mu = 0)[["sigma"]]/1e200, 2, tolerance=1e-11)
  expect_equal(mlhcauchy(c(1,4)*1e-200, mu = 0)[["sigma"]]/1e-200, 2, tolerance=1e-11)
})

test_that("half-Cauchy evaluation preserves ordinary values and tails", {
  f <- mlhcauchy(c(1,4), mu = 0); s <- f[["sigma"]]
  expect_equal(dml(c(-1,0,1,10),f), dhcauchy(x = c(-1,0,1,10), mu = 0, sigma = s))
  expect_equal(dml(1e200,f,log=TRUE), log(2/pi)+log(s)-2*log(1e200), tolerance=1e-12)
  for (lower in c(TRUE,FALSE)) for (logs in c(TRUE,FALSE)) {
    u <- c(0,.001,.5,.999,1)
    expect_equal(pml(qml(if(logs)log(u)else u,f,lower,logs),f,lower),u,tolerance=1e-11)
  }
  expect_equal(pml(qml(-100,f,FALSE,TRUE),f,FALSE,TRUE), -100, tolerance=1e-12)
  expect_equal(pml(qml(-100,f,TRUE,TRUE),f,TRUE,TRUE), -100, tolerance=1e-12)
  set.seed(7); a <- rml(30,f)
  set.seed(7); expect_identical(a, rhcauchy(n = 30, mu = 0, sigma = s))
})

test_that("half-Cauchy d/p/q/r shift with a nonzero threshold", {
  f <- mlhcauchy(c(5,8), mu = 3); s <- f[["sigma"]]
  expect_identical(f[["mu"]], 3)
  o <- optimize(function(t) -sum(dhcauchy(x = c(5,8)-3, mu = 0, sigma = exp(t), log = TRUE)), c(-10,10), tol = 1e-13)
  expect_equal(s, sqrt(10), tolerance = 1e-12)
  expect_equal(s, exp(o$minimum), tolerance = 1e-7)
  expect_equal(attr(f,"logLik"), -o$objective, tolerance = 1e-12)
  expect_equal(dml(c(2,3,4,13),f), dhcauchy(x = c(2,3,4,13)-3, mu = 0, sigma = s))
  expect_equal(sum(dml(c(5,8),f,log=TRUE)), attr(f,"logLik"))
  set.seed(4); a <- rml(20,f)
  set.seed(4); expect_identical(a, 3+rhcauchy(n = 20, mu = 0, sigma = s))
  u <- c(.1,.5,.9)
  expect_equal(qml(u,f), 3+qhcauchy(p = u, mu = 0, sigma = s))
})

test_that("held half-Cauchy scale evaluates every sample without scale collapse (mu held at 0)", {
  set.seed(8102)
  for(x in list(rhcauchy(n = 20, mu = 0, sigma = 2),rhcauchy(n = 100, mu = 0, sigma = 2),abs(rnorm(20)),c(0,0,1),c(0,1),0,rep(0,4))) {
    f <- mlhcauchy(x,mu=0,sigma=1.7)
    expect_identical(unname(f["sigma"]),1.7)
    expect_identical(unname(f["mu"]),0)
    expect_setequal(attr(f,"fixed"),c("mu","sigma"))
    expect_equal(attr(f,"logLik"),sum(dhcauchy(x = x, mu = 0, sigma = 1.7, log=TRUE)),tolerance=1e-12)
    expect_equal(attr(logLik(f),"df"),0L)
  }
  for(bad in c(0,-1,Inf,NaN))expect_error(mlhcauchy(1:3,mu=0,sigma=bad),"sigma.*positive finite")
})

test_that("half-Cauchy free threshold is the sample minimum, independent of a multistart mu/sigma oracle", {
  set.seed(91017)
  samples <- list(c(5,5,7,9,20), c(.3,1,4,9), abs(rnorm(30))+2)
  for (x in samples) {
    f <- mlhcauchy(x, mu = NULL)
    expect_identical(unname(f["mu"]), min(x))
    objective <- function(p) -sum(dhcauchy(x = x-p[1], mu = 0, sigma = exp(p[2]), log = TRUE))
    starts <- expand.grid(c(min(x)-1, min(x)-.1, min(x)), log(c(.1,1,10)))
    fits <- lapply(seq_len(nrow(starts)), function(i)
      optim(as.numeric(starts[i,]), objective, method = "Nelder-Mead",
            control = list(maxit=4000, reltol=1e-13)))
    best <- fits[[which.min(vapply(fits,`[[`,numeric(1),"value"))]]
    expect_lte(-best$value, attr(f,"logLik")+1e-8*length(x))
    expect_equal(attr(f,"logLik"), sum(dml(x,f,log=TRUE)), tolerance = 1e-10)
  }
})

test_that("half-Cauchy free threshold: tie multiplicity vs n (2k<n, 2k=n, 2k>n)", {
  expect_true(is.finite(logLik(mlhcauchy(c(5,5,7,9,20), mu = NULL))))
  expect_error(mlhcauchy(c(3,3,8,20), mu = NULL), "finite unattained")
  expect_error(mlhcauchy(c(3,3,3,8), mu = NULL), "unbounded")
  # Held sigma removes the collapse at any tie multiplicity, mu free or held.
  f <- mlhcauchy(c(3,3,3,8), mu = NULL, sigma = 2)
  expect_identical(unname(f["mu"]), 3)
  expect_identical(unname(f["sigma"]), 2)
})

test_that("half-Cauchy held mu above an observation is invalid", {
  expect_error(mlhcauchy(c(1,2,3), mu = 1.5), "at least the held mu")
  expect_s3_class(mlhcauchy(c(1,2,3), mu = 1), "univariateML")
  expect_s3_class(mlhcauchy(c(1,2,3), mu = -5), "univariateML")
})

test_that("bootstrapml replays a held or estimated hcauchy mu", {
  set.seed(3); x <- 2+rhcauchy(n = 40, mu = 0, sigma = 1.5)
  held <- mlhcauchy(x, mu = 2)
  boot <- bootstrapml(held, reps = 5, reducer = identity)
  expect_true(all(boot["mu",] == 2))
  free <- mlhcauchy(x, mu = NULL)
  boot2 <- bootstrapml(free, reps = 5, reducer = identity)
  expect_true(all(is.finite(boot2["mu",])))
})
