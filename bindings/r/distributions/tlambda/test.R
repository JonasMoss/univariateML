test_that("Tukey lambda tails, endpoints and analytic limits", {
  for (l in c(-1, -.5, 0, 1e-14, -1e-14)) {
    lp <- c(-700, -100, -30, -5, -1)
    for (lower in c(TRUE, FALSE)) {
      q <- qtlambda(p = lp, mu = 0, sigma = 1, lambda = l, lower.tail = lower, log.p = TRUE)
      expect_equal(ptlambda(q = q, mu = 0, sigma = 1, lambda = l, lower.tail = lower, log.p = TRUE), lp, tolerance=1e-10)
    }
  }
  expect_equal(dtlambda(x = c(-1000, 0, 1000), mu = 0, sigma = 1, lambda = 0, log = TRUE), dlogis(c(-1000, 0, 1000), log=TRUE))
  expect_equal(qtlambda(p = c(-1000, -1), mu = 0, sigma = 1, lambda = 0, lower.tail = FALSE, log.p = TRUE), qlogis(c(-1000,-1), lower.tail=FALSE, log.p=TRUE))
  expect_equal(dtlambda(x = c(-1, 0, 1), mu = 0, sigma = 1, lambda = 1), rep(.5,3))
  expect_equal(dtlambda(x = c(-0.5, 0, 0.5), mu = 0, sigma = 1, lambda = 2), rep(1,3))
  expect_equal(dtlambda(x = c(-2, 2), mu = 0, sigma = 1, lambda = 0.5), c(0,0))
  expect_equal(ptlambda(q = c(-Inf, Inf), mu = 0, sigma = 1, lambda = 0), c(0,1))
  expect_equal(qtlambda(p = c(0, 1), mu = 0, sigma = 1, lambda = 0.5), c(-2,2))
  expect_equal(qtlambda(p = c(0.01, 0.8), mu = 0, sigma = 1, lambda = 1e-14), qlogis(c(.01,.8)), tolerance=1e-12)
})
test_that("Tukey lambda vector and RNG conventions", {
  expect_equal(length(dtlambda(x = 1:5, mu = 0:1, sigma = 1, lambda = 0)), 5L)
  expect_length(qtlambda(p = numeric(), mu = 0, sigma = 1, lambda = 0), 0)
  expect_true(is.na(dtlambda(x = NA_real_, mu = 0, sigma = 1, lambda = 0)))
  expect_true(is.nan(dtlambda(x = NaN, mu = 0, sigma = 1, lambda = 0)))
  expect_warning(expect_true(is.nan(dtlambda(x = 1, mu = 0, sigma = -1, lambda = 0))), "NaNs")
  expect_warning(expect_true(is.nan(qtlambda(p = 2, mu = 0, sigma = 1, lambda = 0))), "NaNs")
  set.seed(31); a <- rtlambda(n = 20, mu = 2, sigma = 3, lambda = 0.14)
  set.seed(31); expect_equal(a,qtlambda(p = runif(20), mu = 2, sigma = 3, lambda = 0.14))
  expect_length(rtlambda(n = c(3, 5), mu = 0, sigma = 1, lambda = 0),2)
  expect_error(rtlambda(n = -1, mu = 0, sigma = 1, lambda = 0),"invalid")
})
test_that("Tukey lambda existence guards and exact uniform faces", {
  x <- c(-2,-1,0,1,2)
  for(l in c(1,2)) {
    f <- suppressWarnings(mltlambda(x,lambda=l), classes = "univariateML_restricted")
    expect_equal(as.numeric(f),c(0,2*l,l))
    expect_equal(as.numeric(logLik(f)),as.numeric(logLik(mlunif(x))))
    expect_equal(attr(f,"fixed"),"lambda")
    expect_equal(attr(logLik(f),"df"),2L)
    expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)))
  }
  expect_error(suppressWarnings(mltlambda(c(0,0,1,2)), classes = "univariateML_restricted"),"tie multiplicity")
  expect_error(suppressWarnings(mltlambda(1:2), classes = "univariateML_restricted"),"three")
  expect_error(suppressWarnings(mltlambda(x,lambda=Inf), classes = "univariateML_restricted"),"Fixed lambda")
  expect_error(suppressWarnings(mltlambda(x,lambda=NA_real_), classes = "univariateML_restricted"),"finite")
  expect_error(suppressWarnings(mltlambda(x,reltol=0), classes = "univariateML_restricted"),"positive")
  expect_error(suppressWarnings(mltlambda(x,lambda=0,iterlim=1), classes = "univariateML_restricted"),"converge")
  expect_equal(unname(suppressWarnings(mltlambda(x,mu=.1), classes = "univariateML_restricted")["mu"]),.1)
})
# Independent likelihood: invert the defining quantile with uniroot,
# using log probabilities and expm1, independently of the C++ inversion.
tlambda_oracle_ld <- function(x,mu,sigma,l) {
  z <- (x-mu)/sigma
  if(l==1 || l==2) return(ifelse(abs(z)<=1/l,-log(2*sigma/l),-Inf))
  if(l>0 && any(abs(z)>=1/l)) return(rep(-Inf,length(x)))
  vapply(z,function(y) {
    if(l==0) return(dlogis(y,log=TRUE)-log(sigma))
    qabs <- function(t) (-expm1(-l*t)+expm1(l*log1p(-exp(-t))))/l
    upper <- max(log(2)+1,log1p(abs(y)))
    while(qabs(upper)<abs(y))upper<-2*upper
    t <- if(y==0) log(2) else uniroot(function(t) qabs(t)-abs(y),
                                    c(log(2),upper),tol=1e-11)$root
    a <- (l-1)*(-t); b <- (l-1)*log1p(-exp(-t)); m <- max(a,b)
    -log(sigma)-m-log(exp(a-m)+exp(b-m))
  },numeric(1))
}
test_that("Tukey lambda independent oracle on varied fixed and free samples", {
  for(x in list(c(-4,-1,-.2,0,.5,1,9),c(-3,-2,-1,0,1,2,3),c(-10,-1,-.1,0,.1,1,10))) {
    for(l in c(-1,0,.14)) {
      f <- suppressWarnings(mltlambda(x,lambda=l), classes = "univariateML_restricted")
      objective <- function(t) {
        ll <- sum(tlambda_oracle_ld(x,t[1],exp(t[2]),l))
        if(is.finite(ll)) -ll else 1e100
      }
      fits <- lapply(c(-1,0,1),function(m) optim(c(m,log(max(1,1.1*l*max(abs(x-m))))),objective,method="Nelder-Mead",control=list(maxit=1000,reltol=1e-10)))
      expect_gte(as.numeric(logLik(f)), -min(vapply(fits,`[[`,numeric(1),"value"))-1e-6)
      expect_equal(sum(tlambda_oracle_ld(x,f[1],f[2],l)),as.numeric(logLik(f)),tolerance=1e-8)
    }
    f <- suppressWarnings(mltlambda(x), classes = "univariateML_restricted")
    free_objective <- function(t) {
      ll <- tryCatch(sum(tlambda_oracle_ld(x,t[1],exp(t[2]),tanh(t[3]))),error=function(e) -Inf)
      if(is.finite(ll)) -ll else 1e100
    }
    for(start in c(-.7,0,.6)) {
      o <- optim(c(median(x),log(IQR(x)/2),atanh(start)),free_objective,
                 method="Nelder-Mead",control=list(maxit=1500,reltol=1e-10))
      expect_gte(as.numeric(logLik(f)),-o$value-1e-6)
    }
    for(l in seq(-1,.9,length.out=11)) {
      g <- suppressWarnings(mltlambda(x,lambda=l), classes = "univariateML_restricted")
      expect_gte(as.numeric(logLik(f)),as.numeric(logLik(g))-1e-6)
    }
    expect_equal(sum(tlambda_oracle_ld(x,f[1],f[2],f[3])),as.numeric(logLik(f)),tolerance=1e-8)
  }
})
test_that("Tukey lambda dispatch, affine equivariance and attained bounds", {
  x <- qlogis((1:31-.5)/31)
  f <- suppressWarnings(mltlambda(x), classes = "univariateML_restricted")
  g <- suppressWarnings(mltlambda(5+7*x), classes = "univariateML_restricted")
  expect_equal(g[1],5+7*f[1],ignore_attr=TRUE,tolerance=2e-5)
  expect_equal(g[2],7*f[2],ignore_attr=TRUE,tolerance=2e-5)
  expect_equal(g[3],f[3],ignore_attr=TRUE,tolerance=2e-5)
  expect_equal(as.numeric(logLik(g)),as.numeric(logLik(f))-length(x)*log(7),tolerance=1e-8)
  p <- c(.1,.5,.9)
  expect_equal(pml(qml(p,f),f),p,tolerance=1e-8)
  set.seed(12); a <- rml(10,f)
  set.seed(12); expect_equal(a,rtlambda(n = 10, mu = f[1], sigma = f[2], lambda = f[3]))
  u <- suppressWarnings(mltlambda(seq(-1,1,length.out=31)), classes = "univariateML_restricted")
  expect_equal(unname(u[3]),1,ignore_attr=TRUE)
  expect_equal(as.numeric(logLik(u)),as.numeric(logLik(mlunif(seq(-1,1,length.out=31)))))
  h <- suppressWarnings(mltlambda(c(-1e6,-2,-.1,0,.1,2,1e6)), classes = "univariateML_restricted")
  expect_equal(unname(h[3]),-1,ignore_attr=TRUE)
})
test_that("Tukey lambda avoids standardized overflow in heavy tails", {
  expect_true(is.finite(dtlambda(x = 1e+300, mu = 0, sigma = 1e-300, lambda = -1, log = TRUE)))
  expect_true(is.finite(ptlambda(q = 1e+300, mu = 0, sigma = 1e-300, lambda = -1, lower.tail = FALSE, log.p = TRUE)))
  expect_equal(dtlambda(x = 1e+300, mu = 0, sigma = 1e-300, lambda = -1, log = TRUE),-900*log(10),tolerance=1e-12)
  q <- qtlambda(p = -1000, mu = 0, sigma = 1e-300, lambda = -1, lower.tail = FALSE, log.p = TRUE)
  expect_true(is.finite(q))
  expect_equal(ptlambda(q = q, mu = 0, sigma = 1e-300, lambda = -1, lower.tail = FALSE, log.p = TRUE),-1000,tolerance=1e-12)
})
test_that("Tukey lambda bootstrap, selection and plots use owned evaluation", {
  x <- qlogis((1:31-.5)/31)
  f <- suppressWarnings(mltlambda(x,lambda=0), classes = "univariateML_restricted")
  set.seed(10); b <- bootstrapml(f,reps=3,reducer=identity)
  expect_equal(dim(b),c(3L,3L))
  expect_equal(unname(b['lambda',]),rep(0,3))
  expect_s3_class(model_select(x,models='tlambda',lambda=0),'univariateML')
  path <- tempfile(fileext='.pdf'); grDevices::pdf(path)
  for(kind in c('d','p','q')) expect_silent(plot(f,kind=kind))
  grDevices::dev.off(); expect_gt(file.info(path)$size,0)
  unlink(path)
})
test_that("Tukey lambda quantiles keep log tails and the logistic limit", {
  expect_equal(qtlambda(p = c(-1000, -100), mu = 0, sigma = 1, lambda = 0, lower.tail = FALSE, log.p = TRUE),c(1000,100),tolerance=1e-12)
  expect_equal(qtlambda(p = 0.2, mu = 0, sigma = 1, lambda = 1e-12),qlogis(.2),tolerance=1e-10)
})
test_that("Tukey lambda bounded-tail inversion resolves endpoint deficits", {
  for(l in c(.14,.5,.9,.99)) {
    x <- (1-1e-12)/l
    lp <- ptlambda(q = x, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)
    u <- exp(lp)
    deficit <- -expm1(l*log1p(-u))+exp(l*lp)
    expect_equal(deficit/(1-l*x),1,tolerance=1e-3)
  }
  for(l in c(1,2,5)) expect_equal(dtlambda(qtlambda(p = 1, mu = 0, sigma = 1, lambda = l),mu=0,sigma=1,lambda=l),if(l==1) .5 else 1)
})
test_that("Tukey lambda sampler is inversion with R's RNG", {
  set.seed(1); a <- rtlambda(n = 10, mu = 0, sigma = 1, lambda = 1e-12)
  set.seed(1); expect_equal(a,qlogis(runif(10)),tolerance=1e-10)
})
test_that("Tukey lambda accepts ties strictly below half the sample", {
  for(l in c(-1,0,1)) expect_true(is.finite(as.numeric(logLik(suppressWarnings(mltlambda(c(0,0,1,2,3),lambda=l), classes = "univariateML_restricted")))))
  expect_error(suppressWarnings(mltlambda(rep(1,5)), classes = "univariateML_restricted"))
})

test_that("Tukey lambda handles finite log tails beyond 1e307", {
  l <- 1e-308
  expect_equal(ptlambda(q = 5e+307, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)/(-log(2)/l),1,tolerance=1e-12)
  q <- qtlambda(p = -1e+308, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)
  expect_true(is.finite(q))
  expect_equal(ptlambda(q = q, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)/(-1e308),1,tolerance=1e-12)
})

test_that("Tukey lambda large-tail logarithms retain tiny nonzero values", {
  expect_equal(ptlambda(q = 100, mu = 0, sigma = 1, lambda = 0, log.p = TRUE)/plogis(100,log.p=TRUE),1,tolerance=1e-12)
  for(l in c(-1,0)) {
    large <- qtlambda(p = -1e-100, mu = 0, sigma = 1, lambda = l, log.p = TRUE)
    expect_equal(ptlambda(q = large, mu = 0, sigma = 1, lambda = l, log.p = TRUE)/(-1e-100),1,tolerance=1e-10)
    x <- qtlambda(p = -100, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)
    expect_equal(ptlambda(q = x, mu = 0, sigma = 1, lambda = l, log.p = TRUE)/(-exp(-100)),1,tolerance=1e-10)
    expect_equal(ptlambda(q = -x, mu = 0, sigma = 1, lambda = l, lower.tail = FALSE, log.p = TRUE)/(-exp(-100)),1,tolerance=1e-10)
  }
})

test_that("Tukey lambda keeps shifted finite results after intermediate overflow", {
  expect_equal(ptlambda(1e308, -1e308, 1e308, .1), ptlambda(2, 0, 1, .1), tolerance=1e-12)

  expect_equal(dtlambda(x = 1e+308, mu = -1e+308, sigma = 1e+308, lambda = 0.1, log = TRUE),dtlambda(x = 2, mu = 0, sigma = 1, lambda = 0.1, log = TRUE)-log(1e308),tolerance=1e-12)
  expect_equal(qtlambda(p = 1, mu = -1e+308, sigma = 1e+308, lambda = 0.5)/1e308,1,tolerance=1e-12)
  q <- qtlambda(p = 0.999, mu = -1e+308, sigma = 1e+308, lambda = 0.5)
  expect_true(is.finite(q))
  expect_equal(ptlambda(q = q, mu = -1e+308, sigma = 1e+308, lambda = 0.5),.999,tolerance=1e-12)
})

test_that("Tukey lambda rejects numerical merging of distinct data", {
  expect_error(suppressWarnings(mltlambda(c(0,1e-300,2e-300,1e300)), classes = "univariateML_restricted"),"standardization")
})


test_that("tlambda new holds agree with independent multistart likelihoods", {
  set.seed(8100+9190)
  held_values <- c(mu=0.2,sigma=1.3,lambda=0)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) tlambda_oracle_ld(x,p[1],p[2],p[3])
  for(x in samples)for(mask in 1:7) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mltlambda,c(list(x=x),args))),error=identity)
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"[Uu]se `ml|unbounded|collapse|support cannot contain|does not exist|positive")
      next
    }
    expect_identical(unname(as.numeric(f[held])),unname(held_values[held]))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(attr(logLik(f),"df"),length(held_values)-length(held))
    expect_equal(as.numeric(logLik(f)),sum(ld(x,as.numeric(f))),tolerance=1e-7)
    free <- setdiff(names(held_values),held)
    if(!length(free))next
    encode <- function(p) {z<-c(p[1],log(p[2]));z<-c(z,atanh(p[3]));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else tanh(z[j])
      }
      p
    }
    objective <- function(z) {
      if(any(!is.finite(z))||any(abs(z)>30))return(1e100)
      v<-suppressWarnings(sum(ld(x,decode(z))))
      if(is.finite(v)) -v else 1e100
    }
    # Starts depend on the sample and held values, not on the fitted result.
    values <- c()
    for(shift in c(-1,0,1)) {
      start<-held_values;start[1]<-median(x)+shift;start[2]<-max(.2,sd(x))*exp(shift/2)
      start[held]<-held_values[held]
      o<-suppressWarnings(optim(encode(start),objective,method="Nelder-Mead",
           control=list(maxit=1800,reltol=1e-11)))
      values<-c(values,o$value)
    }
    expect_gte(as.numeric(logLik(f)),-min(values)-2e-6*length(x))
  }
})


test_that("tlambda conditional collapse, feasibility and invalid holds", {
  expect_error(suppressWarnings(mltlambda(c(0,0,1),mu=0), classes = "univariateML_restricted"),"unbounded")
  expect_error(suppressWarnings(mltlambda(c(0,1),mu=0,lambda=-1), classes = "univariateML_restricted"),"collapse")
  expect_error(suppressWarnings(mltlambda(c(-2,2),sigma=1,lambda=.5), classes = "univariateML_restricted"),"support cannot contain")
  expect_error(suppressWarnings(mltlambda(c(-2,2),mu=0,sigma=1,lambda=1), classes = "univariateML_restricted"),"support cannot contain")
  expect_equal(as.numeric(suppressWarnings(mltlambda(c(-1,1),mu=0,sigma=1,lambda=1), classes = "univariateML_restricted")),c(0,1,1))
  expect_equal(as.numeric(suppressWarnings(mltlambda(rep(2,4),sigma=1,lambda=0), classes = "univariateML_restricted"))[1],2,tolerance=1e-6)
  expect_error(suppressWarnings(mltlambda(1:5,mu=Inf), classes = "univariateML_restricted"),"finite")
  expect_error(suppressWarnings(mltlambda(1:5,sigma=0), classes = "univariateML_restricted"),"positive")
})


test_that("Tukey lambda held-location half ties can have an attained maximum", {
  expect_error(suppressWarnings(mltlambda(c(0,1),mu=0), classes = "univariateML_restricted"), "unbounded")
})
