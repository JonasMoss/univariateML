context("mlpig")
test_that("Poisson and zero faces are explicit", {
  for (x in list(rep(1,10), c(0,1,1,2), c(0,2), rep(2,10)))
    expect_error(mlpig(x), "Use `mlpois`")
  poisson_sample <- qpois((seq_len(100)-.5)/100,2)
  expect_lte(mean((poisson_sample-mean(poisson_sample))^2),mean(poisson_sample))
  expect_error(mlpig(poisson_sample), "Use `mlpois`")
  expect_error(mlpig(rep(0,3)), "point-mass")
  expect_error(mlpig(rep(0,3), mean=2), "point-mass")
  expect_error(mlpig(rep(0,3), shape=2), "point-mass")
  fit <- mlpig(rep(0,3), mean=2,shape=3)
  expect_equal(attr(fit,"logLik"),3*actuar::dpoisinvgauss(0,2,3,log=TRUE))
})
test_that("all fixed subsets agree with accurate actuar oracle", {
  x <- c(0,0,0,0,1,2,3,5,10)
  for (held in list(list(),list(mean=3),list(shape=3),list(mean=3,shape=3))) {
    fit <- do.call(mlpig,c(list(x=x),held))
    expect_equal(sort(attr(fit,"fixed")),sort(if (is.null(names(held))) character() else names(held)))
    expect_equal(attr(fit,"logLik"),sum(actuar::dpoisinvgauss(x,fit[["mean"]],fit[["shape"]],log=TRUE)),tolerance=1e-9)
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    free <- setdiff(c("mean","shape"),names(held))
    if (length(free)) {
      objective <- function(z) {
        pars <- as.list(setNames(as.numeric(fit),names(fit)))
        pars[free] <- exp(z)
        -sum(do.call(actuar::dpoisinvgauss,c(list(x=x,log=TRUE),pars)))
      }
      for (mult in c(.5,1,2)) {
        opt <- optim(log(as.numeric(fit[free])*mult),objective,method="BFGS",control=list(reltol=1e-12))
        expect_lte(-opt$value,attr(fit,"logLik")+1e-7)
      }
    }
  }
})
test_that("fixed controls reject invalid values", {
  for (v in list(0,-1,Inf,NA_real_)) {
    expect_error(mlpig(c(0,1,10),mean=v))
    expect_error(mlpig(c(0,1,10),shape=v))
  }
})

test_that("a tiny finite overdispersion gain loses the mandated face tie", {
  x <- c(rep(0,10002),rep(2,10000),3)
  expect_gt(mean((x-mean(x))^2),mean(x))
  expect_error(mlpig(x),"Use `mlpois`")
})
