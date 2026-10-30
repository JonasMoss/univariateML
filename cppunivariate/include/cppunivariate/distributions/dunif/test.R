test_that("dunif agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mldunif(x)
  expect_equal(unname(coef(fit)), c(min(x), max(x)))
  expect_equal(attr(fit, "logLik"), sum(ddunif(x = x, min = fit[[1]], max = fit[[2]], log = TRUE)))
})

test_that("discrete uniform width cannot overflow an int", {
  x <- c(-.Machine$integer.max, .Machine$integer.max)
  expect_equal(attr(mldunif(x), "logLik"), -2 * log(2*.Machine$integer.max + 1))
})

test_that("discrete uniform holds maximize enumerated reference likelihoods", {
  set.seed(812)
  for(x in list(sample(-2:7,20,TRUE),sample(-2:7,100,TRUE),c(0,2),rep(2,4),c(0,0,1,3))) {
    for(mask in 0:3) {
      args <- list(x=x)
      if(bitwAnd(mask,1)>0)args$min <- -5
      if(bitwAnd(mask,2)>0)args$max <- 10
      f <- do.call(mldunif,args)
      lower <- if(is.null(args$min)) (min(x)-3):min(x) else args$min
      upper <- if(is.null(args$max)) max(x):(max(x)+3) else args$max
      oracle <- max(vapply(lower,function(a) max(vapply(upper,function(b)
        sum(ddunif(x = x, min = a, max = b, log = TRUE)),0.)),0.))
      expect_equal(as.numeric(logLik(f)),oracle,tolerance=1e-12)
      expect_setequal(attr(f,"fixed"),names(args)[-1])
      if(!is.null(args$min))expect_identical(f[["min"]],args$min)
      if(!is.null(args$max))expect_identical(f[["max"]],args$max)
    }
  }
  for(args in list(list(min=1),list(max=2),list(min=.5),list(max=Inf),list(min=NA_real_),list(max=NaN)))
    expect_error(do.call(mldunif,c(list(x=0:3),args)))
})

test_that("dunif singleton fitted law is constant", {
  fit <- mldunif(rep(3,4))
  expect_equal(dml(c(2,3),fit),c(0,1))
  expect_equal(pml(c(2,3),fit),c(0,1))
  expect_equal(qml(c(0,.5,1),fit),rep(3,3))
  expect_equal(rml(10,fit),rep(3,10))
})
