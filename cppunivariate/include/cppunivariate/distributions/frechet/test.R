test_that("frechet: held location, attained shape boundary and escape", {
 x <- extraDistr::qfrechet((1:100-.5)/100,lambda=.7,mu=2,sigma=3)
 f <- mlfrechet(x,mu=2)
 b <- mlinvweibull(x-2,reltol=1e-10)
 expect_equal(unname(f["lambda"]),unname(b["shape"]),tolerance=1e-10)
 expect_equal(unname(f["sigma"]),1/unname(b["rate"]),tolerance=1e-10)
 expect_equal(attr(f,"logLik"),attr(b,"logLik"),tolerance=1e-10)
 free <- mlfrechet(x, mu = NULL)
 expect_equal(unname(free["lambda"]),1)
 expect_error(mlfrechet(c(1,1,2), mu = NULL),"tie")
 expect_error(mlfrechet(1:5,mu=1),"strictly")
 expect_error(mlfrechet(1:5,mu=Inf),"finite")
 expect_error(mlfrechet(rep(1,5),mu=0),"nonconstant")
 expect_error(mlfrechet(x,iterlim=1, mu = NULL),"converge")
 expect_error(mlfrechet(x,mu=2,iterlim=1),"converge")
 expect_error(mlfrechet(extraDistr::qgumbel((1:100-.5)/100), mu = NULL),"mlgumbel")
 expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
 set.seed(12); a<-rml(20,f);set.seed(12);expect_identical(rml(20,f),a)
 for(shape in c(.5,1,3)) {
   y <- extraDistr::qfrechet((1:80-.5)/80,lambda=shape)
   fit <- mlfrechet(y, mu = NULL)
   objective <- function(z) -sum(extraDistr::dfrechet(y,lambda=1+exp(z[1]),mu=min(y)-exp(z[2]),sigma=exp(z[3]),log=TRUE))
   for(delta in c(-.3,0,.3)) {
     start <- c(log(max(fit["lambda"]-1,1e-6)),log(min(y)-fit["mu"]),log(fit["sigma"]))
     o<-optim(start+delta,objective,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))
     expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(y))
   }
 }
})

test_that("frechet and fpareto log-tail repairs remain finite", {
 source_fun <- univariateML:::univariateML_reference_fixes
 q <- source_fun[["extraDistr::qfrechet"]]$replacement(-1000,2,lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),500)
 p <- source_fun[["extraDistr::pfrechet"]]$replacement(q,2,lower.tail=FALSE,log.p=TRUE)
 expect_equal(p,-1000)
 q <- source_fun[["actuar::qfpareto"]]$replacement(-40,0,.3,.7,.4,lower.tail=FALSE,log.p=TRUE)
 expect_true(is.finite(q))
 expect_equal(actuar::pfpareto(q,0,.3,.7,.4,lower.tail=FALSE,log.p=TRUE),-40,tolerance=1e-12)
})

test_that("frechet: reference mapping and unbounded unrestricted path", {
 x <- c(.1,.5,1,2,10,100)
 expect_lt(max(abs((extraDistr::dfrechet(x,2,0,2,log=TRUE))-(actuar::dinvweibull(x,2,scale=2,log=TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(extraDistr::dfrechet(y,.1,1-e,e,log=TRUE)))
 expect_true(all(diff(ll)>0))
})

test_that("frechet: an independent Gumbel-face oracle finds no better member", {
 y <- extraDistr::qgumbel((1:80-.5)/80)
 g <- mlgumbel(y)
 nll <- function(z) {
   if(any(!is.finite(z)) || any(abs(z)>500) || z[1]>log(1e5)) return(1e100)
   ll <- sum(extraDistr::dfrechet(y,lambda=1+exp(z[1]),
     mu=min(y)-exp(z[2]),sigma=exp(z[3]),log=TRUE))
   if(is.finite(ll)) -ll else 1e100
 }
 for(lambda in c(3,30,300)) {
   sf <- as.numeric(g["sigma"])*lambda
   start <- c(log(lambda-1),log(min(y)-as.numeric(g["mu"])+sf),log(sf))
   o <- optim(start,nll,method="Nelder-Mead",control=list(maxit=2000,reltol=1e-12))
   expect_lte(-o$value,attr(g,"logLik")+1e-6*length(y))
 }
 expect_error(mlfrechet(y, mu = NULL),"mlgumbel")
})

test_that("threshold tail repairs retain finite answers under extreme rescaling", {
 fixes <- univariateML:::univariateML_reference_fixes
 q <- fixes[["extraDistr::qfrechet"]]$replacement(-100,.1,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),log(1e-300)+1000,tolerance=1e-12)
 expect_equal(fixes[["extraDistr::pfrechet"]]$replacement(q,.1,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE),-100,tolerance=1e-12)
 q <- fixes[["actuar::qfpareto"]]$replacement(-40,0,.1,.4,.4,scale=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(pbeta(plogis(-.4*(log(q)-log(1e-300))),.1,.4,log.p=TRUE),
   -40,tolerance=1e-12)
})

test_that("Frechet free location accepts ties above the minimum", {
  x <- c(0,1,2,2,2)
  f <- tryCatch(mlfrechet(x), error=identity)
  if (inherits(f,"error")) expect_match(conditionMessage(f), "mlgumbel")
  else expect_equal(as.numeric(logLik(f)), sum(extraDistr::dfrechet(x,f[1],f[2],f[3],log=TRUE)), tolerance=1e-8)
})
