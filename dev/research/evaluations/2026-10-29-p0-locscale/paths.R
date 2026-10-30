# Rscript dev/research/evaluations/2026-10-29-p0-locscale/paths.R
library(univariateML,lib.loc='/tmp/umvl-p0lib')
x <- c(0,1,2,4,7); s <- 10^-(1:6)
print(data.frame(s, lst_df_0.1=vapply(s,function(z) sum(extraDistr::dlst(x,mu=0,sigma=z,df=.1,log=TRUE)),0), lst_df_1=vapply(s,function(z) sum(extraDistr::dlst(x,mu=0,sigma=z,df=1,log=TRUE)),0), tlambda_minus8=vapply(s,function(z) sum(dtlambda(x,mu=0,sigma=z,lambda=-8,log=TRUE)),0)))
# Exact t2 face: sd must diverge at the indicated rate to retain ordinary scale 1.
nu <- 2+10^-(1:7); print(data.frame(nu,sd=sqrt(nu/(nu-2)),ll=vapply(nu,function(v) sum(dstd(x,mean=0,sd=sqrt(v/(v-2)),nu=v,log=TRUE)),0),t2=sum(dt(x,2,log=TRUE))))
# GED unrestricted spike after exact scale profiling, alpha is exponential-power scale.
p <- c(.1,.05,.02,.01,.005); print(data.frame(p,ll=vapply(p,function(v) {-length(x)*log(2)-length(x)*lgamma(1+1/v)-length(x)/v*log(v*mean(abs(x)^v))-length(x)/v},0)))
print(c(bhatt_a0=extraDistr::dbhatt(0,mu=0,sigma=1,a=0),normal=dnorm(0),lst_df_half=extraDistr::dlst(0,df=.5),ged_nu_half=dged(0,nu=.5),tlambda_minus2=dtlambda(0,lambda=-2)))
# GED nu -> 0 with sd held at 1. Stabilized README formula.
ged_fixed_sd <- function(x,v) {b<-exp(.5*(-2*log(2)/v+lgamma(1/v)-lgamma(3/v)));sum(-abs(x/b)^v/2+log(v)-1.5*lgamma(1/v)+.5*lgamma(3/v)-log(2))}
print(data.frame(nu=c(.1,.05,.02,.01),held_sd_1_tie=vapply(c(.1,.05,.02,.01),function(v)ged_fixed_sd(c(0,1,2,4,7),v),0),held_sd_1_no_tie=vapply(c(.1,.05,.02,.01),function(v)ged_fixed_sd(c(1,2,4,7,8),v),0)))
