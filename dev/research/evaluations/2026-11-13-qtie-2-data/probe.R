options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r',quiet=TRUE)
evidence<-Sys.getenv('QTIE2_EVIDENCE_DIR','dev/research/evaluations/2026-11-13-qtie-2-data')
cases<-jsonlite::fromJSON(file.path(evidence,'cases.json'),simplifyVector=FALSE)
reached<-function(v,p,l,g) {
 e<-64*.Machine$double.eps;t<-p
 if(g){if(l)t<-ifelse(p > -.Machine$double.xmax/(1+e),p*(1+e),p) else t<-p*(1-e)}else if(l)t<-p*(1-e) else t<-ifelse(1-p>4*e,p*(1+e),p)
 if(l)v>=t else v<=t
}
rows<-list();draws<-list();set.seed(215)
for(c in cases) {
 message(c$id," point ",c$point)
 if(!is.null(c$config$truncate))c$config$truncate<-as.numeric(unlist(c$config$truncate))
 fit<-tryCatch(do.call(get(paste0('ml',c$base)),c(c$args,c$config)),error=function(e)e)
 if(inherits(fit,'error')){rows[[length(rows)+1]]<-data.frame(id=c$id,point=c$point,lower=NA,log=NA,n=0,up=0,earlier=0,invalid=0,crossing_failure=0,error=conditionMessage(fit));next}
 draws[[length(draws)+1]]<-tryCatch(rml(if(grepl("-window-",c$id,fixed=TRUE))16 else 512,fit),error=function(e)conditionMessage(e))
 for(l in c(TRUE,FALSE))for(g in c(FALSE,TRUE)) {
 k<-c$centre+(if(grepl("-window-",c$id,fixed=TRUE))(-2:2) else (-100:100))
 tails<-tryCatch(list(p=pml(k,fit,lower.tail=l,log.p=g),prev=pml(k-1,fit,lower.tail=l,log.p=g)),error=function(e)e)
 if(inherits(tails,'error')) {
 rows[[length(rows)+1]]<-data.frame(id=c$id,point=c$point,lower=l,log=g,n=0,up=0,earlier=0,invalid=0,crossing_failure=0,error=paste('pml:',conditionMessage(tails)));next
 }
 p<-tails$p;prev<-tails$prev
 ok<-if(g)is.finite(p)&p<0 else p>0&p<1;ok<-ok & !is.na(p!=prev)&p!=prev;k<-k[ok];p<-p[ok]
 qerror<-'';q<-tryCatch(qml(p,fit,lower.tail=l,log.p=g),error=function(e){qerror<<-conditionMessage(e);rep(NaN,length(p))})
 cross<-reached(pml(q,fit,lower.tail=l,log.p=g),p,l,g)&!reached(pml(q-1,fit,lower.tail=l,log.p=g),p,l,g)
 rows[[length(rows)+1]]<-data.frame(id=c$id,point=c$point,lower=l,log=g,n=length(p),up=sum(q>k,na.rm=TRUE),earlier=sum(q<k,na.rm=TRUE),invalid=sum(!is.finite(q)),crossing_failure=sum(!cross,na.rm=TRUE),error=qerror)
 }
}
args<-commandArgs(TRUE);write.csv(do.call(rbind,rows),args[1],row.names=FALSE);saveRDS(draws,args[2],version=2)
