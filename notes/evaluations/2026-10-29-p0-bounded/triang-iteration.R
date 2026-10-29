library(univariateML,lib.loc='/tmp/umvl-p0lib')
outdir<-'notes/evaluations/2026-10-29-p0-bounded'; x<-rep(.4,5); z<-list();i<-0
for(held in list(list(c=-1),list(b=2,c=-1))) for(limit in c(100,1000,10000)) for(tol in c(1e-12,1e-9,1e-6)){f<-tryCatch(suppressWarnings(do.call(mltriang,c(list(x=x,iterlim=limit,reltol=tol),held))),error=function(e)e);i<-i+1;z[[i]]<-data.frame(held=paste(names(held),collapse='+'),iterlim=limit,reltol=tol,result=if(inherits(f,'error'))conditionMessage(f) else paste('fit',paste(format(coef(f),digits=8),collapse='+')),stringsAsFactors=FALSE)}
write.csv(do.call(rbind,z),file.path(outdir,'triang-iteration.csv'),row.names=FALSE);print(do.call(rbind,z),row.names=FALSE)
