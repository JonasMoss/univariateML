# Exact log-density paths, independently of package fitter; run from repo root.
library(univariateML,lib.loc='/tmp/umvl-p0lib')
x <- c(0,1,2,4,5); n <- length(x)
cat('Truncated normal: free lower bound approaches minimum; held upper=6, mean=2, sd=2\n')
for(e in c(1,.1,.01,.001,0)) {
 a <- min(x)-e
 cat(sprintf('gap=%g logLik=%g\n',e,sum(extraDistr::dtnorm(x,mean=2,sd=2,a=a,b=6,log=TRUE))))
}
cat('Symmetric GED, mode at observation, profiled exponential-power scale, nu to zero\n')
for(p in c(1,.3,.1,.05,.02,.01,.005)) {
 y <- abs(x-x[1]); loga <- log(p*mean(y^p))/p
 ll <- n*(log(p)-log(2)-lgamma(1/p)-loga)-sum(exp(p*(log(y)-loga)))
 cat(sprintf('nu=%g logLik=%.8f\n',p,ll))
}
cat('SGED density accepts nu=.5: ',dsged(0,nu=.5,xi=1,log=TRUE),'\n')
cat('SSTD with sd held at 1, xi held at 1, mean held at 0: four tied zeros and one outlier\n')
x2 <- c(0,0,0,0,1)
for(e in 10^-(1:8)) cat(sprintf('nu-2=%g logLik=%g\n',e,sum(dsstd(x2,mean=0,sd=1,nu=2+e,xi=1,log=TRUE))))
cat('Truncated-normal exponential boundary: x=(.01,.99), held a=0,b=1\n')
print(tryCatch(mltnorm(c(.01,.99),a=0,b=1),error=function(e)conditionMessage(e)))
