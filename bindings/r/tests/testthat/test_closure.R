test_that("closed endpoints give the exact fitted likelihood", {
  for (x in list(c(-3,-3,0,1), 1e15+c(0,1,2,3))) {
    f <- mlhnorm(x, mu=NULL)
    expect_identical(f[["mu"]],min(x))
    expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-12)
    expect_equal(dml(min(x),f),sqrt(2/pi)/f[["sigma"]])
  }
  for (held in list(list(),list(beta=2),list(alpha=3),list(alpha=3,beta=2))) {
    x <- c(1,2,3)
    f <- do.call(mlpower,c(list(x=x),held))
    expect_identical(f[["alpha"]],3)
    expect_equal(dml(3,f),f[["beta"]]/3)
    expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)))
  }
  expect_error(mlpower(rep(2,3)),"unbounded")
  expect_equal(as.numeric(logLik(mlpower(rep(2,3),beta=2))),0)
})

test_that("truncation bounds profile to sample extremes and retain closed density", {
  x <- c(-2,-.3,0,.1,.2,2)
  for (a in list(NULL,-2)) for (b in list(NULL,2)) {
    f <- .modifier_tnorm(x,a=a,b=b,mean=0,sd=1)
    expect_equal(as.numeric(f[c("a","b")]),c(-2,2))
    expect_setequal(attr(f,"fixed"),c("mean","sd",if(!is.null(a))"a",if(!is.null(b))"b"))
    expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-10)
    expect_equal(dml(c(-2,2),f),dnorm(c(-2,2))/(pnorm(2)-pnorm(-2)),tolerance=1e-10)
  }
  x <- c(-2,rep(0,15),2)
  f <- .modifier_tnorm(x,a=NULL,b=NULL)
  expect_equal(as.numeric(f[c("a","b")]),range(x))
  expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-9)
  expect_error(.modifier_tnorm(c(0,0,10),a=NULL,b=Inf),"exponential")
  expect_error(.modifier_tnorm(c(0,1),a=NULL,b=NULL),"truncated exponential")
})

test_that("resolved endpoints agree in both probability tails and log modes", {
  fits <- list(mlhnorm(c(-2,0,1),mu=NULL), mlpower(c(1,2,3)),
               .modifier_tnorm(c(-2,0,2),a=NULL,b=NULL,mean=0,sd=1),
               mlunif(c(-2,0,2)))
  for (f in fits) {
    bounds <- univariateML:::univariateML_support(f)$bounds
    for (lower in c(TRUE,FALSE)) for (lp in c(TRUE,FALSE)) {
      p <- if (lp) c(-Inf,0) else c(0,1)
      expect_equal(qml(p,f,lower.tail=lower,log.p=lp),if(lower) bounds else rev(bounds))
      expected <- if (lower) c(0,1) else c(1,0)
      expect_equal(pml(bounds,f,lower.tail=lower,log.p=lp),if(lp) log(expected) else expected)
    }
    expect_equal(dml(bounds[1]-1,f),0)
  }
})

test_that("GPD and GEV endpoint limits include the uniform xi=-1 member", {
  for (id in c("gpd","gev")) {
    f <- univariateML:::univariateML_construct(c(0,2,-1),paste0("ml",id),list())
    expect_equal(dml(2,f),.5)
    expect_equal(dml(2,f,log=TRUE),-log(2))
    expect_equal(pml(2,f),1)
    expect_equal(qml(1,f),2)
    expect_equal(dml(2.01,f),0)
  }
})

test_that("tpois support preserves its strict lower truncation", {
  f <- .modifier_tpois(c(2,3,4),a=NULL,b=NULL,lambda=3)
  expect_equal(as.numeric(f[c("a","b")]),c(1,4))
  expect_equal(univariateML:::univariateML_support(f)$bounds,c(2,4))
  expect_equal(dml(1,f),0)
  expect_gt(dml(2,f),0)
  expect_equal(sum(dml(c(2,3,4),f,log=TRUE)),as.numeric(logLik(f)))
  expect_equal(qml(c(0,1),f),c(2,4))
  expect_equal(pml(c(1,4),f),c(0,1))
})

test_that("declared endpoint limits cover zero, finite and infinite densities", {
  construct <- univariateML:::univariateML_construct
  cases <- list(
    list("pareto",c(a=2,b=3),3,2/3),
    list("pareto2",c(shape=2,scale=3,min=-1),-1,2/3),
    list("pareto3",c(shape=.5,scale=3,min=-1),-1,Inf),
    list("pareto3",c(shape=1,scale=3,min=-1),-1,1/3),
    list("pareto3",c(shape=2,scale=3,min=-1),-1,0),
    list("pareto4",c(shape1=2,shape2=1,scale=3,min=-1),-1,2/3),
    list("fpareto",c(shape1=2,shape2=2,shape3=.5,scale=3,min=-1),-1,2/(3*beta(2,.5))),
    list("nsbeta",c(shape1=1,shape2=2,min=-1,max=3),-1,.5),
    list("nsbeta",c(shape1=2,shape2=.5,min=-1,max=3),3,Inf),
    list("genbeta",c(shape1=2,shape2=1,shape3=3,scale=4),4,1.5),
    list("frechet",c(lambda=2,mu=-1,sigma=3),-1,0),
    list("fatigue",c(alpha=2,beta=3,mu=-1),-1,0),
    list("triang",c(a=0,b=2,c=0),0,1),
    list("triang",c(a=0,b=2,c=2),2,1))
  for (case in cases) {
    f <- construct(case[[2]],paste0("ml",case[[1]]),list())
    expect_equal(dml(case[[3]],f),case[[4]],info=case[[1]])
    expect_equal(dml(case[[3]],f,log=TRUE),log(case[[4]]),info=case[[1]])
  }
})

test_that("closed reference functions retain positional coefficient overrides", {
  f <- univariateML:::univariateML_construct(c(8,20,2),"mlnhyper",list())
  d <- univariateML_to_function(f,"d")
  p <- univariateML_to_function(f,"p")
  q <- univariateML_to_function(f,"q")
  expect_equal(d(1003,1000,2000,3,log=TRUE),dhyper(3,1000,1000,1003,log=TRUE)+log(3/1003))
  expect_equal(p(500,1000,2000,3,lower.tail=FALSE,log.p=TRUE),phyper(2,1000,1000,500,log.p=TRUE))
  expect_equal(q(c(0,1),20,20,2),c(2,2))
})
