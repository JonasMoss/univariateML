# Small independent two-shape/scale and half-family checks; run from repo root.
library(univariateML,lib.loc='/tmp/umvl-p0lib'); library(extraDistr)
set.seed(20261029)
checks=list(
 hnorm=list(x=rhnorm(100), p=c(sigma=1), d=function(x,p) dhnorm(x,p[1],log=TRUE), args=list(mu=0)),
 hcauchy=list(x=rhcauchy(100),p=c(sigma=1),d=function(x,p) dhcauchy(x,p[1],log=TRUE),args=list()),
 ht=list(x=rht(100,3),p=c(nu=3,sigma=1),d=function(x,p) dht(x,p[1],p[2],log=TRUE),args=list()),
 foldnorm=list(x=abs(rnorm(100,2)),p=c(mu=2,sigma=1),d=function(x,p) dfoldnorm(x,p[1],p[2],log=TRUE),args=list()),
 fatigue=list(x=rfatigue(100,3,2),p=c(alpha=3,beta=2),d=function(x,p) dfatigue(x,p[1],p[2],log=TRUE),args=list()),
 betapr=list(x=rbetapr(100,2,3,1),p=c(shape1=2,shape2=3),d=function(x,p) dbetapr(x,p[1],p[2],1,log=TRUE),args=list(scale=1)))
for(id in names(checks)) { z=checks[[id]]; f=function(u) -sum(z$d(z$x,exp(u))); starts=list(log(z$p),rep(0,length(z$p)),rep(1,length(z$p))); opt=lapply(starts,function(a) optim(a,f,method='BFGS',control=list(reltol=1e-12,maxit=300))); best=min(vapply(opt,`[[`,0.0,'value')); fit=tryCatch(do.call(get(paste0('ml',id)),c(list(x=z$x),z$args)),error=function(e) e); if(inherits(fit,'error')) cat(id,'package:',conditionMessage(fit),'\n') else cat(id,'oracle',-best,'package',as.numeric(logLik(fit)),'difference',as.numeric(logLik(fit))+best,'\n') }
