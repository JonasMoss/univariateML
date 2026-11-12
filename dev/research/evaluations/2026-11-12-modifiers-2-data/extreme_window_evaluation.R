# Exercise the fitted-object evaluation bridge on the controlled-size edge.
pkgload::load_all(Sys.getenv('UMVL_AUDIT_PKG','build/scratch/modifiers-2-audit-package'),quiet=TRUE)
library(jsonlite)
f<-mlbinom(0:3,size=1e308,truncate=c(0,Inf),iterlim=512)
run<-function(fun)tryCatch(list(status='Ok',values=as.numeric(fun())),error=function(e)list(status='stop',message=conditionMessage(e)))
rows<-list(d=run(function()dml(0:3,f,log=TRUE)),p=run(function()pml(0:3,f)),q=run(function()qml(c(.01,.5,.99),f)))
reference<-list(d=dpois(0:3,1.5,log=TRUE),p=ppois(0:3,1.5),q=qpois(c(.01,.5,.99),1.5))
write_json(list(results=rows,reference=reference),'dev/research/evaluations/2026-11-12-modifiers-2-data/extreme-window-evaluation.json',auto_unbox=TRUE,null='null',pretty=TRUE,digits=17)
print(rows)
for(type in names(rows)) {
 stopifnot(rows[[type]]$status=='Ok')
 stopifnot(isTRUE(all.equal(rows[[type]]$values,reference[[type]],tolerance=1e-10)))
}
