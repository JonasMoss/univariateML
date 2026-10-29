library(univariateML,lib.loc='/tmp/umvl-p0lib')
set.seed(20261029)
x20<-extraDistr::rtnorm(20,mean=0,sd=1,a=-2,b=2)
samples<-list(n20=x20,n100=extraDistr::rtnorm(100,a=-2,b=2),n1000=extraDistr::rtnorm(1000,a=-2,b=2),n2=x20[1:2],n3=x20[1:3],n5=x20[1:5],ties=round(x20,1),uniform=runif(20,-1,1),exponential=rexp(20))
rows<-list()
for(cas in names(samples)) {
 x<-samples[[cas]]; p<-c(mean=0,sd=1,a=min(x)-1,b=max(x)+1); ps<-names(p)
 for(mask in 0:14) {
  held<-ps[as.logical(intToBits(mask)[1:4])]; free<-setdiff(ps,held)
  enc<-function(p) c(mean=p['mean'],sd=log(p['sd']),a=log(min(x)-p['a']),b=log(p['b']-max(x))) |> setNames(ps)
  dec<-function(q) c(mean=q['mean'],sd=exp(q['sd']),a=min(x)-exp(q['a']),b=max(x)+exp(q['b'])) |> setNames(ps)
  init<-enc(p)
  f<-function(z){q<-init;q[free]<-z; p0<-dec(q); if(any(abs(q)>25) || abs(p0['mean']/p0['sd'])>7 || p0['sd']<.01) return(1e100); v<-tryCatch(do.call(extraDistr::dtnorm,c(list(x=x),as.list(dec(q)),list(log=TRUE))),error=function(e) -Inf);if(any(!is.finite(v))) 1e100 else -sum(v)}
  opts<-lapply(c(0,-.5,.5),function(o)tryCatch(optim(init[free]+o,f,method='BFGS',control=list(maxit=250)),error=function(e)NULL))
  best<-min(vapply(opts,function(z)if(is.null(z))Inf else z$value,0.0))
  pkg<-if(all(c('a','b') %in% held)) tryCatch(do.call(mltnorm,c(list(x=x,a=p['a'],b=p['b']),as.list(p[intersect(held,c('mean','sd'))]))),error=function(e)e) else simpleError('bounds not implemented')
  ll<-if(inherits(pkg,'error'))NA_real_ else as.numeric(logLik(pkg)); msg<-if(inherits(pkg,'error'))conditionMessage(pkg) else 'returned'
  rows[[length(rows)+1]]<-data.frame(case=cas,n=length(x),held=paste(held,collapse='+'),oracle=-best,package=ll,difference=ll+best,status=msg)
 }
}
out<-do.call(rbind,rows);write.csv(out,'notes/evaluations/2026-10-29-p0-skewreal/tnorm-results.csv',row.names=FALSE); print(out[is.finite(out$difference)&abs(out$difference)>1e-6*out$n,])
