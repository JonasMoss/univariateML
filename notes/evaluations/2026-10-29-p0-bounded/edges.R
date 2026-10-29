library(univariateML,lib.loc='/tmp/umvl-p0lib')
outdir <- 'notes/evaluations/2026-10-29-p0-bounded';rows<-list();i<-0
probe<-function(id,label,x,h){f<-tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),as.list(h)))),error=function(e)e);err<-if(inherits(f,'error'))conditionMessage(f) else '';p<-if(inherits(f,'error')) '' else paste(format(coef(f),digits=12),collapse='+');lk<-if(inherits(f,'error')) NA_real_ else as.numeric(logLik(f));fixed<-if(inherits(f,'error')) '' else paste(sort(attr(f,'fixed')),collapse='+');data.frame(id=id,case=label,n=length(x),held=paste(names(h),collapse='+'),values=paste(format(h,digits=7),collapse='+'),coef=p,loglik=lk,fixed=fixed,error=err)}
add<-function(id,label,x,h){i<<-i+1;rows[[i]]<<-probe(id,label,x,h)}
x<-c(0,.25,1)
for(a in c(-1,0,.25,1.2)) for(b in c(2,1,.25,-.2)) for(c in c(-1,0,.4,1,2)) for(mask in 0:7){v<-c(a=a,b=b,c=c);h<-v[as.logical(intToBits(mask)[1:3])];add('triang','nonconstant',x,h)}
for(z in c(0,.4)) for(a in c(-1,z))for(b in c(z,2))for(c in c(-1,z,2))for(mask in 0:7){v<-c(a=a,b=b,c=c);h<-v[as.logical(intToBits(mask)[1:3])];add('triang',paste0('constant-',z),rep(z,3),h)}
for(a in c(-1,0,.25,1.2)) for(b in c(2,1,.25,-.2)) for(mask in 0:3){v<-c(min=a,max=b);h<-v[as.logical(intToBits(mask)[1:2])];add('unif','nonconstant',x,h)}
for(a in c(-1,.4)) for(b in c(.4,2)) for(mask in 0:3){v<-c(min=a,max=b);h<-v[as.logical(intToBits(mask)[1:2])];add('unif','constant',rep(.4,3),h)}
write.csv(do.call(rbind,rows),file.path(outdir,'edges.csv'),row.names=FALSE,na='NA')
sink(file.path(outdir,'domains.txt'))
for(id in c('beta','kumar','logitnorm','unif','triang')) {
 cat('\n',id,'\n');vv<-switch(id,beta=list(c(0,2),c(2,0),c(0,0),c(Inf,2),c(2,Inf),c(Inf,Inf),c(-1,2)),kumar=list(c(0,2),c(2,0),c(Inf,2),c(2,Inf),c(-1,2)),logitnorm=list(c(0,0),c(0,Inf),c(Inf,1),c(-Inf,1),c(0,-1)),unif=list(c(0,0),c(-Inf,1),c(0,Inf),c(1,0)),triang=list(c(0,1,0),c(0,1,1),c(0,0,0),c(-Inf,1,0),c(0,Inf,1),c(0,1,-1)))
 for(p in vv) {z<-tryCatch(suppressWarnings(switch(id,beta=dbeta(.4,p[1],p[2],log=TRUE),kumar=extraDistr::dkumar(.4,p[1],p[2],log=TRUE),logitnorm=dlogitnorm(.4,p[1],p[2],log=TRUE),unif=dunif(.4,p[1],p[2],log=TRUE),triang=extraDistr::dtriang(.4,p[1],p[2],p[3],log=TRUE))),error=function(e)NA_real_);cat(p,'=>',z,'\n')}
}
sink()
cat('wrote',length(rows),'edge rows\n')
