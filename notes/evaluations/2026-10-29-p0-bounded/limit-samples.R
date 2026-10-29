# Exact point-mass limits fall outside the continuous fitters' sample support.
library(univariateML,lib.loc='/tmp/umvl-p0lib')
outdir<-'notes/evaluations/2026-10-29-p0-bounded'
params<-list(beta=c(shape1=2,shape2=3),kumar=c(a=2,b=3),logitnorm=c(mu=2,sigma=3))
samples<-list(delta0=rep(0,5),delta1=rep(1,5),endpoint_mixture=c(0,0,1,1,1),delta_half=rep(.5,5))
rows<-list();i<-0
for(id in names(params)) for(sn in names(samples)) for(mask in 0:2) {
 h<-params[[id]][as.logical(intToBits(mask)[1:2])];x<-samples[[sn]]
 f<-tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),as.list(h)))),error=function(e)e)
 i<-i+1;rows[[i]]<-data.frame(id=id,sample=sn,held=paste(names(h),collapse='+'),result=if(inherits(f,'error'))conditionMessage(f) else paste0('fit; fixed=',paste(attr(f,'fixed'),collapse='+')),stringsAsFactors=FALSE)
}
write.csv(do.call(rbind,rows),file.path(outdir,'limit-samples.csv'),row.names=FALSE)
cat('Exact point-mass sample calls:',length(rows),'\n')
cat('dbeta boundaries, both shapes .5:',dbeta(c(0,1),.5,.5),'\n')
cat('dkumar boundaries, both shapes .5:',extraDistr::dkumar(c(0,1),.5,.5),'\n')
