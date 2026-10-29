# Reference-density paths. Run with Rscript from the repository root.
library(univariateML,lib.loc="/tmp/umvl-p0lib")
t<-c(2,5,10,20); x<-rep(1,3)
f<-list(
 exp_all_zero=function(t) sum(dexp(rep(0,3),rate=t,log=TRUE)),
 gamma_constant=function(t) sum(dgamma(x,shape=t^2,rate=t^2,log=TRUE)),
 weibull_constant=function(t) sum(dweibull(x,shape=t,scale=1,log=TRUE)),
 lnorm_constant=function(t) sum(dlnorm(x,meanlog=0,sdlog=exp(-t),log=TRUE)),
 invgamma_constant=function(t) sum(extraDistr::dinvgamma(x,alpha=t^2,beta=t^2,log=TRUE)),
 invgauss_constant=function(t) sum(actuar::dinvgauss(x,mean=1,shape=t^2,log=TRUE)),
 invweibull_constant=function(t) sum(actuar::dinvweibull(x,shape=t,rate=1,log=TRUE)),
 llogis_constant=function(t) sum(actuar::dllogis(x,shape=t,rate=1,log=TRUE)),
 naka_constant=function(t) sum(dnaka(x,shape=t^2,scale=1,log=TRUE)),
 gompertz_constant=function(t) sum(extraDistr::dgompertz(x,a=t/expm1(t),b=t,log=TRUE)),
 sgomp_constant=function(t) sum(dsgomp(x,b=t,eta=exp(t),log=TRUE)),
 sgomp_all_zero=function(t) sum(dsgomp(rep(0,3),b=t,eta=3,log=TRUE)),
 lgamma_constant=function(t) sum(actuar::dlgamma(rep(exp(1),3),shapelog=t^2,ratelog=t^2,log=TRUE)),
 pareto_constant=function(t) sum(extraDistr::dpareto(x,a=t,b=1,log=TRUE)),
 power_fixed_shape=function(t) sum(extraDistr::dpower(c(.4,1),alpha=1+exp(-t),beta=2,log=TRUE)),
 power_constant=function(t) sum(extraDistr::dpower(x,alpha=1+exp(-t),beta=t,log=TRUE)),
 lomax_exp=function(t) sum(extraDistr::dlomax(c(.2,1,2),lambda=1/t,kappa=t,log=TRUE)),
 invpareto_invexp=function(t) sum(actuar::dinvpareto(c(.2,1,2),scale=1/t,shape=t,log=TRUE)),
 gompertz_exp=function(t) sum(extraDistr::dgompertz(c(.2,1,2),a=1,b=1/t,log=TRUE)))
out<-do.call(rbind,lapply(names(f),function(n) data.frame(path=n,t=t,logLik=vapply(t,f[[n]],0.0))))
write.csv(out,"notes/evaluations/2026-10-29-p0-positive/paths.csv",row.names=FALSE)
print(out,row.names=FALSE)
