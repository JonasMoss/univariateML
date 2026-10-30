#!/usr/bin/env Rscript
# Reference-density multistart versus every implemented fixed-design shape hold.
Sys.setenv(P0_FUNCTIONS_ONLY="1")
source("dev/research/evaluations/2026-10-29-p0-binomial/oracle.R")
set.seed(20261030)
configs <- list(
 binom=list(list()), bbinom=list(list(),list(alpha=2),list(beta=3)),
 cmb=list(list(),list(prob=.3),list(nu=1.5)), tbinom=list(list(),list(prob=.4)),
 zib=list(list()), zmbinom=list(list()), ztbinom=list(list()))
rows <- list(); j<-0L
for(id in names(configs)) {
  dat <- samples(id)
  for (sample in names(dat)) for (held in configs[[id]]) {
    x <- dat[[sample]]; N<-unname(size[id]); base<-args_for(id)
    oracle_ll<-tryCatch(oracle(id,x,N,held),error=function(e) NA_real_)
    fit<-tryCatch(suppressWarnings(do.call(get(paste0("ml",id)),c(list(x=x),base,held))),error=function(e)e)
    fit_ll<-if(inherits(fit,"error")) NA_real_ else as.numeric(logLik(fit))
    j<-j+1L;rows[[j]]<-data.frame(id=id,sample=sample,held=paste(names(held),collapse=","),n=length(x),oracle=oracle_ll,master=fit_ll,delta=fit_ll-oracle_ll,message=if(inherits(fit,"error"))conditionMessage(fit)else "")
  }
}
# The independent CMB formula must also agree with the package's density.
chk<-expand.grid(N=c(0,2,5,10,20),p=c(0,.1,.3,.8,1),nu=c(-2,0,1,3))
chk$max_error<-vapply(seq_len(nrow(chk)),function(i){a<-chk[i,]; xx<-0:a$N; d1<-dens("cmb",xx,a$N,c(a$p,a$nu)); d2<-univariateML::dcmb(xx,a$N,a$p,a$nu,log=TRUE); if(!identical(is.infinite(d1),is.infinite(d2))) return(Inf); max(abs(d1[is.finite(d1)]-d2[is.finite(d2)]))},0.0)
write.table(chk,"dev/research/evaluations/2026-10-29-p0-binomial/cmb-density.tsv",sep="\t",row.names=FALSE,quote=TRUE)
write.table(do.call(rbind,rows),"dev/research/evaluations/2026-10-29-p0-binomial/held-checks.tsv",sep="\t",row.names=FALSE,quote=TRUE)
cat("CMB largest density discrepancy:",max(chk$max_error),"\n")
