#!/usr/bin/env Rscript
# Finite-grid C6 likelihood profiles with independently evaluated densities.
suppressPackageStartupMessages(library(univariateML, lib.loc="/tmp/umvl-p0lib"))
set.seed(20261101)
subsets<-function(v) lapply(0:(2^length(v)-1),function(i)v[as.logical(intToBits(i)[seq_along(v)])])
results<-list(); index<-0L
for (id in c("hyper","nhyper")) {
 defaults<-if(id=="hyper") list(m=20,N=50,k=10) else list(m=20,N=50,r=3)
 v<-names(defaults);s<-lapply(c(20,100,1000,2,3,5),function(n) simulate_default(paste0("ml",id),n));names(s)<-paste0("default_",c(20,100,1000,2,3,5))
 s$tied<-if(id=="hyper")c(0,0,1,1,2,2) else c(3,3,4,4,5,5)
 s$boundary<-if(id=="hyper")rep(0,5) else rep(3,5)
 s$limit<-if(id=="hyper")rbinom(100,10,.4) else 3+rnbinom(100,3,.4)
 for(sn in names(s)) for(h in subsets(v)) {
  if(length(h)==3) next
  x<-s[[sn]]; xt<-table(x); xv<-as.integer(names(xt)); counts<-as.numeric(xt)
  Ns<-if("N"%in%h) 50 else unique(c(max(max(x),if("m"%in%h)20 else 1),50,100,200))
  Ns<-Ns[Ns>=max(x)]; bests<-rep(-Inf,length(Ns))
  for(i in seq_along(Ns)) {
   N<-Ns[i]; mm<-if("m"%in%h)20 else 0:N; mm<-mm[mm<=N]
   if(id=="hyper") {
    kk<-if("k"%in%h)10 else seq.int(max(x),N)
    gr<-expand.grid(m=mm,k=kk); gr<-gr[gr$m>=max(x) & gr$k<=N & N-gr$m>=gr$k-min(x),,drop=FALSE]
    if(nrow(gr)) { ll<-numeric(nrow(gr)); for(t in seq_along(xv))ll<-ll+counts[t]*dhyper(xv[t],gr$m,N-gr$m,gr$k,log=TRUE);bests[i]<-max(ll) }
   } else {
    rr<-if("r"%in%h)3 else 0:min(x);gr<-expand.grid(m=mm,r=rr);gr<-gr[gr$m>=gr$r & max(x)<=N-gr$m+gr$r,,drop=FALSE]
    if(nrow(gr)) {
      ll<-rep(-Inf,nrow(gr));deg<-gr$r==0;ll[deg]<-if(all(x==0))0 else -Inf
      deg<-gr$m==N & gr$r>0;ll[deg]<-ifelse(min(x)==max(x) & gr$r[deg]==min(x),0,-Inf)
      ix<-which(gr$m<N & gr$r>0)
      if(length(ix)) {z<-numeric(length(ix)); for(t in seq_along(xv))z<-z+counts[t]*extraDistr::dnhyper(xv[t],N-gr$m[ix],gr$m[ix],gr$r[ix],log=TRUE);ll[ix]<-z}
      bests[i]<-max(ll)
    }
   }
  }
  index<-index+1L;results[[index]]<-data.frame(id=id,sample=sn,held=paste(h,collapse=","),n=length(x),best=max(bests),last=tail(bests,1),best_N=Ns[which.max(bests)])
 }
}
write.table(do.call(rbind,results),"notes/evaluations/2026-10-29-p0-binomial/hyper-scan.tsv",sep="\t",quote=TRUE,row.names=FALSE)
