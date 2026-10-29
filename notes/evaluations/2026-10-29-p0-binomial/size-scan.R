#!/usr/bin/env Rscript
# Diagnostic integer profiles with other coefficients held/free in all subsets.
Sys.setenv(P0_FUNCTIONS_ONLY="1")
source("notes/evaluations/2026-10-29-p0-binomial/oracle.R")
set.seed(20261031)
base_dens <- dens
ids<-c("binom","bbinom","cmb","tbinom","zib","zmbinom","ztbinom")
values<-list(binom=list(prob=.5), bbinom=list(alpha=2,beta=3),cmb=list(prob=.3,nu=1.5),tbinom=list(prob=.4,a=0,b=10),zib=list(prob=.4,pi=.4),zmbinom=list(prob=.4,p0=.3),ztbinom=list(prob=.4))
subsets<-function(v) lapply(0:(2^length(v)-1),function(i) v[as.logical(intToBits(i)[seq_along(v)])])
out<-list();j<-0L
for (id in ids) {
  dat<-samples(id)
  for (sn in names(dat)) {
    x<-dat[[sn]]
    for (hs in subsets(names(values[[id]]))) {
      held<-values[[id]][hs]
      dens<-function(f,x,N,v) {
        if(f!="tbinom")return(base_dens(f,x,N,v))
        a<-if("a"%in%hs)held$a else min(x)-1
        b<-if("b"%in%hs)held$b else max(x)
        extraDistr::dtbinom(x,N,v[1],a,b,log=TRUE)
      }
      hshape<-held[intersect(names(held),c("prob","pi","p0","alpha","beta","nu"))]
      lo<-max(max(x),if(id %in% c("zmbinom","ztbinom")) 1 else 0)
      grid<-unique(c(lo+0:5,20,50,200,1000));grid<-grid[grid>=lo]
      ll<-vapply(grid,function(N)tryCatch(oracle(id,x,N,hshape),error=function(e) NA_real_),0.0)
      j<-j+1L;out[[j]]<-data.frame(id=id,sample=sn,held=paste(hs,collapse=","),n=length(x),best_size=grid[which.max(replace(ll,is.na(ll),-Inf))],best=max(ll,na.rm=TRUE),last=tail(ll,1),large_minus_best=tail(ll,1)-max(ll,na.rm=TRUE))
    }
  }
}
write.table(do.call(rbind,out),"notes/evaluations/2026-10-29-p0-binomial/size-scan.tsv",sep="\t",row.names=FALSE,quote=TRUE)
