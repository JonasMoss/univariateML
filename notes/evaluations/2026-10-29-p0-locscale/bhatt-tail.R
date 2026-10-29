# Stable normal-CDF difference for the two cases where extraDistr loses tails.
library(univariateML,lib.loc='/tmp/umvl-p0lib')
s<-readRDS('notes/evaluations/2026-10-29-p0-locscale/samples-bhatt.rds')
ld<-function(x,mu,sigma,a){lo<-(x-mu-a)/sigma;hi<-(x-mu+a)/sigma; upper<-lo>0; big<-ifelse(upper,pnorm(lo,lower.tail=FALSE,log.p=TRUE),pnorm(hi,log.p=TRUE));small<-ifelse(upper,pnorm(hi,lower.tail=FALSE,log.p=TRUE),pnorm(lo,log.p=TRUE));big+log1p(-exp(small-big))-log(2*a)}
results<-list()
for(case in c('cauchy','t2')) {x<-s[[case]]; objective<-function(z){mu<-z[1];sig<-if(case=='cauchy')1 else exp(z[2]);v<-sum(ld(x,mu,sig,2));if(is.finite(v))-v else 1e100}; starts<-expand.grid(mu=c(median(x),mean(x),0),ls=c(-1,0,1,2)); cand<-lapply(seq_len(nrow(starts)),function(i)optim(if(case=='cauchy')starts$mu[i] else c(starts$mu[i],starts$ls[i]),objective,method='Nelder-Mead',control=list(reltol=1e-12,maxit=2000)));z<-cand[[which.min(vapply(cand,`[[`,0,'value'))]];fit<-if(case=='cauchy')mlbhatt(x,sigma=1,a=2) else mlbhatt(x,a=2);results[[case]]<-data.frame(case,oracle=-z$value,package=as.numeric(logLik(fit)),delta= -z$value-as.numeric(logLik(fit)),oracle_parameters=paste(z$par,collapse=','))}
out<-do.call(rbind,results);print(out);write.csv(out,'notes/evaluations/2026-10-29-p0-locscale/bhatt-tail.csv',row.names=FALSE)
