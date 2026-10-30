#!/usr/bin/env Rscript
# Escaping sequences; the likelihood is bounded because these are discrete pmfs.
Sys.setenv(P0_FUNCTIONS_ONLY="1")
source("dev/research/evaluations/2026-10-29-p0-binomial/oracle.R")
out<-list();i<-0L
record<-function(label,t,ll) {i<<-i+1L;out[[i]]<<-data.frame(path=label,t=t,logLik=ll)}
x<-c(0,1,2,4)
for(N in c(10,100,1000,10000)) {
 record("binom to Poisson",N,sum(dbinom(x,N,mean(x)/N,log=TRUE)))
 record("zib to ZIP",N,sum(extraDistr::dzib(x,N,2/N,.2,log=TRUE)))
 record("zmbinom to ZMPoisson",N,sum(actuar::dzmbinom(x,N,2/N,.2,log=TRUE)))
 record("ztbinom to ZTPoisson",N,sum(actuar::dztbinom(x+1,N,2/N,log=TRUE)))
 record("tbinom to truncated Poisson",N,sum(extraDistr::dtbinom(x+1,N,2/N,0,8,log=TRUE)))
 record("hyper to binomial",N,sum(dhyper(c(1,2,3),round(.4*N),N-round(.4*N),5,log=TRUE)))
 record("nhyper to negative binomial",N,sum(extraDistr::dnhyper(c(3,4,6),N-round(.4*N),round(.4*N),3,log=TRUE)))
}
for(t in c(2,5,10,100,1000,10000)) {
 record("bbinom to binomial",t,sum(extraDistr::dbbinom(c(2,3,4),10,.4*t,.6*t,log=TRUE)))
 record("bbinom to endpoint mixture",t,sum(extraDistr::dbbinom(c(0,10),10,1/t,1/t,log=TRUE)))
 record("tbinom to lower point mass",t,sum(extraDistr::dtbinom(rep(1,3),10,1/t,0,10,log=TRUE)))
}
for(t in c(2,5,10,20,40)) {
 # eta chooses equal limiting weights at adjacent points 2 and 3.
 eta <- -t*(lchoose(6,3)-lchoose(6,2))
 record("cmb to adjacent mixture",t,sum(dens("cmb",c(2,3),6,c(plogis(eta),t))))
 record("cmb to endpoint mixture",t,sum(dens("cmb",c(0,6),6,c(.5,-t))))
}
write.table(do.call(rbind,out),"dev/research/evaluations/2026-10-29-p0-binomial/limits.tsv",sep="\t",row.names=FALSE,quote=TRUE)
