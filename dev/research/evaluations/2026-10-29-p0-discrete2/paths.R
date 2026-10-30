# Formula checks and explicit sequences approaching reference-family limits.
# Run from repo root: Rscript dev/research/evaluations/2026-10-29-p0-discrete2/paths.R
library(univariateML, lib.loc='/tmp/umvl-p0lib')
library(extraDistr)
outdir='dev/research/evaluations/2026-10-29-p0-discrete2/'
zsum=function(s) {n=2048;k=1:n;sum(k^(-s))+n^(1-s)/(s-1)-.5*n^(-s)+s/12*n^(-s-1)-s*(s+1)*(s+2)/720*n^(-s-3)}
cmp=function(x,lambda,nu) {if(lambda==0)return(ifelse(x==0,0,-Inf)); k=0:2000; w=k*log(lambda)-nu*lgamma(k+1); m=max(w); x*log(lambda)-nu*lgamma(x+1)-m-log(sum(exp(w-m)))}
f=list(cmp=function(x)cmp(x,4,1.5),yulesimon=function(x)log(2)+lbeta(x,3),zeta=function(x)-2*log(x)-log(zsum(2)),zipf=function(x)-.5*log(x)-log(sum((1:8)^(-.5))),skellam=function(x) -(sqrt(4)-sqrt(3))^2+x/2*log(4/3)+log(besselI(2*sqrt(12),abs(x),expon.scaled=TRUE)))
g=list(cmp=function(x)univariateML::dcmp(x,4,1.5,log=TRUE),yulesimon=function(x)univariateML::dyulesimon(x,2,log=TRUE),zeta=function(x)univariateML::dzeta(x,2,log=TRUE),zipf=function(x)univariateML::dzipf(x,8,.5,log=TRUE),skellam=function(x)univariateML::dskellam(x,4,3,log=TRUE))
checks=do.call(rbind,lapply(names(f),function(id){x=if(id=='skellam') -6:6 else if(id=='zipf') 1:8 else if(id %in% c('zeta','yulesimon')) 1:12 else 0:12;data.frame(id=id,n=length(x),max_abs_log_error=max(abs(f[[id]](x)-g[[id]](x))))}));write.csv(checks,paste0(outdir,'formula-checks.csv'),row.names=FALSE)
rows=list(); add=function(id,case,t,ll,limit) {rows[[length(rows)+1]] <<- data.frame(id=id,case=case,t=t,logLik=ll,limit=limit)}
for(t in c(2,4,8,16,32)) {
 add('zipf','N->inf fixed s zeta',t,-sum(2*log(c(1,2,3)))-3*log(sum((1:(10*t))^(-2))), -sum(2*log(c(1,2,3)))-3*log(zsum(2)))
 add('yulesimon','ones rho->inf',t,sum(log(t)+lbeta(rep(1,5),t+1)),0)
 add('zeta','ones s->inf',t,-5*log(zsum(t)),0)
 add('zipf','held N, ones s->inf',t,-5*log(sum((1:3)^(-t))),0)
 add('cmp','nu->inf adjacent 1/2',t,sum(cmp(c(1,1,1,2,2),(2/3)*2^t,t)),3*log(.6)+2*log(.4))
 add('cmp','held lambda nu->inf on 0/1',t,sum(cmp(c(0,0,1,1,1),.5,t)),2*log(2/3)+3*log(1/3))
 add('cmp','held nu lambda->0 zeros',t,sum(cmp(rep(0,5),exp(-t),1.5)),0)
 add('tpois','fixed bounds lower positive',t,sum(extraDistr::dtpois(rep(1,5),exp(-t),0,5,log=TRUE)),0)
 add('tpois','fixed bounds upper',t,sum(extraDistr::dtpois(rep(5,5),t,0,5,log=TRUE)),0)
 add('dlaplace','scale->0 at held centre',t,sum(extraDistr::ddlaplace(rep(1,5),1,exp(-t),log=TRUE)),0)
 sd=1/t; mu=2-sd*qnorm(.6); x=c(1,1,1,2,2)
 add('dnorm','adjacent fitted Bernoulli',t,sum(log(pnorm((x+1-mu)/sd)-pnorm((x-mu)/sd))),3*log(.6)+2*log(.4))
 add('dnorm','held mean at common edge',t,sum(log(pnorm((x+1-2)/sd)-pnorm((x-2)/sd))),5*log(.5))
 shape=(2/sd)^2; rate=2/sd^2; x=c(1,1,1,2,2)
 add('dgamma','adjacent via concentrating gamma',t,sum(extraDistr::ddgamma(x,shape,rate,log=TRUE)),5*log(.5))
 add('dgamma','held shape all zero',t,5*extraDistr::ddgamma(0,2,t,log=TRUE),0)
 add('dgamma','held rate all zero',t,5*extraDistr::ddgamma(0,1/t,1,log=TRUE),0)
 q=exp(-(-log(.4))/2^t); add('dweibull','adjacent 1/2 free shapes',t,sum(extraDistr::ddweibull(c(1,1,1,2,2),q,t,log=TRUE)),3*log(.6)+2*log(.4))
 q=.4; add('dweibull','held shape1 Bernoulli',t,sum(extraDistr::ddweibull(c(0,0,0,1,1),q,t,log=TRUE)),3*log(1-q)+2*log(q))
 add('dweibull','held shape2 all zero',t,5*extraDistr::ddweibull(0,exp(-t),2,log=TRUE),0)
 add('bnbinom','held size NB face',t,sum(extraDistr::dbnbinom(c(0,0,1,1,2),2,2*t,3*t,log=TRUE)),sum(dnbinom(c(0,0,1,1,2),size=2,prob=.4,log=TRUE)))
 add('bnbinom','all free Poisson face',t,sum(extraDistr::dbnbinom(c(0,0,1,1,2),t,t^2,t,log=TRUE)),sum(dpois(c(0,0,1,1,2),1,log=TRUE)))
 add('bnbinom','held alpha beta size->0 zero',t,5*extraDistr::dbnbinom(0,1/t,2,5,log=TRUE),0)
}
write.csv(do.call(rbind,rows),paste0(outdir,'paths.csv'),row.names=FALSE)
print(checks)
