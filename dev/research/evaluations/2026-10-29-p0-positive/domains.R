# Empirical reference acceptance at coordinate boundaries; warnings preserved.
library(univariateML,lib.loc="/tmp/umvl-p0lib")
ids<-c("exp","gamma","weibull","lnorm","invgamma","invgauss","invweibull","invexp","llogis","rayleigh","naka","lomax","gompertz","sgomp","lgamma","invpareto","pareto","power")
out<-list()
for(id in ids) {
 m<-univariateML:::univariateML_metadata[[paste0("ml",id)]]; p<-setNames(m$default,m$names)
 s<-strsplit(m$density,"::",fixed=TRUE)[[1]]; f<-if(length(s)==2) getExportedValue(s[1],s[2]) else getExportedValue("univariateML",s[1])
 x<-switch(id,lgamma=c(1.1,2,3),pareto=c(2,3,4),power=c(.1,.5,.9),c(.5,1,2))
 for(name in names(p)) for(v in c(-Inf,-1,0,Inf)) {
  pp<-p;pp[name]<-v;w<-character()
  ans<-tryCatch(withCallingHandlers(do.call(f,c(list(x=x),as.list(pp),list(log=TRUE))),warning=function(c){w<<-c(w,conditionMessage(c));invokeRestart("muffleWarning")}),error=function(e) paste("ERROR",conditionMessage(e)))
  out[[length(out)+1]]<-data.frame(id=id,param=name,value=v,result=if(is.numeric(ans)) paste(signif(ans,5),collapse=";") else paste(ans,collapse=";"),warning=paste(w,collapse=";"))
 }
}
write.csv(do.call(rbind,out),"dev/research/evaluations/2026-10-29-p0-positive/domains.csv",row.names=FALSE)
