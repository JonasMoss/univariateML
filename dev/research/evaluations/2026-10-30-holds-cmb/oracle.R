library(univariateML)
library(extraDistr)
set.seed(20261030)
# Reproduce the RNG consumption and samples in P0b exactly.
invisible(rbbinom(20,10,2,3)); invisible(rbbinom(100,10,2,3))
invisible(rbbinom(1000,10,2,3)); invisible(rbbinom(5,10,2,3))
invisible(rnbinom(100,size=2,prob=.4)); invisible(rbinom(100,10,.4))
draw <- function(n) { k<-0:10; z<-1.5*lchoose(10,k)+k*qlogis(.3); sample(k,n,TRUE,exp(z-max(z))) }
samples <- list(default_20=draw(20),default_100=draw(100),
  tied=c(0,0,1,1,2,2), adjacent=rep(c(4,5),25),
  endpoint=rep(c(0,10),25), over_20=rcmb(20,10,.4,-.6),
  over_100=rcmb(100,10,.4,-.6), poisson_20=rpois(20,3),
  poisson_100=rpois(100,3))
profile <- function(x,N,hold) {
  obj <- function(t) {
    nu <- if(hold=='nu') 1.5 else if(hold=='nu_negative') -.6 else t[length(t)]
    eta <- if(hold=='prob') qlogis(.3) else t[1]-nu*log(N)
    p <- plogis(eta)
    if(p<=0 || p>=1) return(1e100)
    v <- -sum(dcmb(x,N,p,nu,log=TRUE))
    if(is.finite(v)) v else 1e100
  }
  starts <- if(hold=='none') expand.grid(a=c(-1,1,3),nu=c(-1,0,1,3)) else if(hold=='prob') data.frame(nu=c(-3,-1,0,1,3)) else data.frame(a=c(-2,0,2,4))
  fits <- lapply(seq_len(nrow(starts)),function(i) tryCatch(optim(as.numeric(starts[i,]),obj,method='BFGS',control=list(maxit=300,reltol=1e-11)),error=function(e) NULL))
  vals <- vapply(fits,function(f) if(is.null(f)) Inf else f$value,0.)
  j<-which.min(vals); f<-fits[[j]]
  c(ll=-vals[j],convergence=f$convergence)
}
face <- function(x,heldnu=FALSE) {
  f <- function(t) {
    nu<-if(heldnu) 1.5 else exp(t[2]); lam<-exp(t[1])
    v<-tryCatch(-sum(dcmp(x,lam,nu,log=TRUE)),error=function(e) Inf)
    if(is.finite(v)) v else 1e100
  }
  starts<-if(heldnu) list(log(mean(x))) else list(c(log(mean(x)),0),c(log(mean(x)),log(1.5)),c(0,-1))
  vals<-vapply(starts,function(s) tryCatch(optim(s,f,method='BFGS',control=list(maxit=200))$value,error=function(e) Inf),0.)
  -min(vals)
}
out<-list(); j<-0L
for(nm in names(samples)) {
 x<-samples[[nm]]; Ns<-sort(unique(c(seq(max(x),max(x)+15),50,100,300,1000,2000))); Ns<-Ns[Ns>=max(x)]
 cmp<-face(x); cmp15<-face(x,TRUE); geom<-sum(dgeom(x,1/(1+mean(x)),log=TRUE)); pois<-sum(dpois(x,mean(x),log=TRUE))
 for(hold in c('none','prob','nu','nu_negative')) for(N in Ns) {
   v<-profile(x,N,hold); j<-j+1L
   binom<-sum(dbinom(x,N,mean(x)/N,log=TRUE))
   out[[j]]<-data.frame(sample=nm,n=length(x),hold=hold,N=N,ll=v[1],convergence=v[2],cmp=cmp,cmp15=cmp15,geom=geom,pois=pois,binom=binom)
 }
 cat(nm,'completed\n'); flush.console()
}
write.csv(do.call(rbind,out),'dev/research/evaluations/2026-10-30-holds-cmb/oracle.csv',row.names=FALSE)
saveRDS(samples,'dev/research/evaluations/2026-10-30-holds-cmb/samples.rds')
