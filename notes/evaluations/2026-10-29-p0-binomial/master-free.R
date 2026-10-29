#!/usr/bin/env Rscript
# Compare free-size master fits with independently profiled finite-size candidates.
Sys.setenv(P0_FUNCTIONS_ONLY="1")
source("notes/evaluations/2026-10-29-p0-binomial/oracle.R")
set.seed(20261031)
scan<-read.delim("notes/evaluations/2026-10-29-p0-binomial/size-scan.tsv")
ids<-c("binom","bbinom","cmb","tbinom","zib","zmbinom","ztbinom")
rows<-list();j<-0L
for(id in ids) {
 dat<-samples(id)
 if(!(id %in% c("binom","zib","zmbinom","ztbinom")))next
 for(sn in names(dat)) {
  x<-dat[[sn]]; ref<-scan[scan$id==id & scan$sample==sn & scan$held=="",,drop=FALSE]
  fit<-tryCatch(suppressWarnings(do.call(get(paste0("ml",id)),list(x=x))),error=function(e)e)
  ll<-if(inherits(fit,"error")) NA_real_ else as.numeric(logLik(fit))
  sz<-if(inherits(fit,"error")) NA_real_ else unname(coef(fit)["size"])
  atfit<-if(is.finite(sz))oracle(id,x,sz)else NA_real_
  freq<-table(x); sample_counts<-paste(names(freq),as.integer(freq),sep=":",collapse=",")
  j<-j+1L;rows[[j]]<-data.frame(id=id,sample=sn,n=length(x),sample_counts=sample_counts,master=ll,size=sz,oracle_at_size=atfit,grid_size=ref$best_size,grid_best=ref$best,delta=ll-atfit,message=if(inherits(fit,"error"))conditionMessage(fit)else "")
 }
}
# Hyper with m and k held: enumerate N to a finite diagnostic cap.
set.seed(20261101)
for(id in c("hyper","nhyper")) {
 dat<-lapply(c(20,100,1000,2,3,5),function(n)simulate_default(paste0("ml",id),n));names(dat)<-paste0("default_",c(20,100,1000,2,3,5))
 if(id=="nhyper") next
 dat$tied<-c(0,0,1,1,2,2);dat$boundary<-rep(0,5);dat$limit<-rbinom(100,10,.4)
 for(sn in names(dat)) {
  x<-dat[[sn]]; grid<-seq.int(max(20+10-min(x),20,10),2000)
  v<-vapply(grid,function(N)sum(dhyper(x,20,N-20,10,log=TRUE)),0.0)
  fit<-tryCatch(suppressWarnings(mlhyper(x,m=20,k=10)),error=function(e)e)
  ll<-if(inherits(fit,"error"))NA_real_ else as.numeric(logLik(fit))
  freq<-table(x); sample_counts<-paste(names(freq),as.integer(freq),sep=":",collapse=",")
  j<-j+1L;rows[[j]]<-data.frame(id="hyper_m_k",sample=sn,n=length(x),sample_counts=sample_counts,master=ll,size=NA_real_,oracle_at_size=max(v),grid_size=grid[which.max(v)],grid_best=max(v),delta=ll-max(v),message=if(inherits(fit,"error"))conditionMessage(fit)else "")
 }
}
write.table(do.call(rbind,rows),"notes/evaluations/2026-10-29-p0-binomial/master-free.tsv",sep="\t",row.names=FALSE,quote=TRUE)
