# Boundary, restrictions and domain probes, run from the repository root.
suppressPackageStartupMessages(library(univariateML,lib.loc='/tmp/umvl-p0lib'))
suppressPackageStartupMessages(library(extraDistr))
outdir='notes/evaluations/2026-10-29-p0-halfthreshold'
source_text=readLines(file.path(outdir,'oracle.R')); eval(parse(text=source_text[1:(grep('^rows=list',source_text)-1)]))
seqe=10^-(1:6); rows=list(); nr=0
add=function(name,e,v) {nr<<-nr+1; rows[[nr]]<<-data.frame(path=name,e=e,ll=v)}
x=c(1,2,3,4,5); y=c(1,1,2,3,4)
for(e in seqe) {
 add('hnorm constant, mu=min',e,sum(dhnorm(rep(0,5),e,log=TRUE)))
 add('hcauchy k=3 n=5',e,sum(dhcauchy(c(0,0,0,1,2),e,log=TRUE)))
 add('hcauchy k=2 n=4 equality',e,sum(dhcauchy(c(0,0,1,2),e,log=TRUE)))
 add('ht nu=.2 k=1 n=5',e,sum(dht(x-min(x),.2,e,log=TRUE)))
 add('ht nu=.25 k=1 n=5 equality',e,sum(dht(x-min(x),.25,e,log=TRUE)))
 add('foldnorm constant',e,sum(lfold(rep(1,5),1,e)))
 add('fatigue fixed mu gamma-half face',e,sum(dfatigue(x,1/sqrt(e),e,0,log=TRUE)))
 add('fatigue fixed mu invgamma-half face',e,sum(dfatigue(x,1/sqrt(e),1/e,0,log=TRUE)))
 add('fatigue free',e,sum(dfatigue(x,sqrt(1/e),e,min(x)-e^2,log=TRUE)))
 add('fatigue beta held k=2 n=5',e,sum(dfatigue(y,sqrt(1/e),1,min(y)-e,log=TRUE)))
 add('fatigue beta held k=1 n=3 equality',e,sum(dfatigue(c(1,2,3),sqrt(1/(3*e)),1,1-e,log=TRUE)))
 add('fatigue alpha held constant',e,sum(dfatigue(rep(1,5),1,e,1-e,log=TRUE)))
 add('fatigue mu held constant',e,sum(dfatigue(rep(1,5),e,1,0,log=TRUE)))
 add('fatigue beta mu held constant',e,sum(dfatigue(rep(1,5),e,1,0,log=TRUE)))
 add('betapr constant fixed scale',e,sum(dbetapr(rep(1,5),1/e,1/e,1,log=TRUE)))
 # alpha*beta=1 and beta+mu=2; the shifted law tends to N(2,1).
 add('fatigue normal limit',e,sum(dfatigue(x,e,1/e,2-1/e,log=TRUE)))
 add('fatigue normal reference',e,sum(dnorm(x,2,1,log=TRUE)))
 add('half-t normal limit',e,sum(dht(x,1/e,1,log=TRUE)))
 add('half-normal reference',e,sum(dhnorm(x,1,log=TRUE)))
 add('betapr gamma limit',e,sum(dbetapr(x,2,1/e,1/e,log=TRUE)))
 add('gamma reference',e,sum(dgamma(x,2,scale=1,log=TRUE)))
 add('betapr invgamma limit',e,sum(dbetapr(x,1/e,3,e,log=TRUE)))
 add('invgamma reference',e,sum(dgamma(1/x,3,scale=1,log=TRUE)-2*log(x)))
}
write.csv(do.call(rbind,rows),file.path(outdir,'path-results.csv'),row.names=FALSE)
# Explicit cases for restricted nu and held threshold placement.
cases=list(unique=c(0,1,2,3,4), belowhalf=c(0,0,1,2,3), equalhalf=c(0,0,1,2), abovehalf=c(0,0,0,1,2), strict=c(1,2,3,4,5))
ans=list(); j=0
for(nm in names(cases)) for(h in list(c(),c(mu=0),c(nu=1),c(mu=0,nu=1),c(sigma=1))) {x=cases[[nm]]; a=oracle('ht',x,h,nu_lower=if(!'nu'%in%names(h)) 1 else 0); f=faces('ht',x,h); j=j+1; ans[[j]]=data.frame(sample=nm,held=paste(names(h),collapse=','),ll=a$ll,par=paste(names(a$par),signif(a$par,6),collapse=';'),normal=if(length(f)) f[1] else NA)}
write.csv(do.call(rbind,ans),file.path(outdir,'restricted-results.csv'),row.names=FALSE)
# Inspect each reference argument at endpoints; NaN and improper all-zero densities distinguished.
refs=list(hnorm=list(fun=dhnorm,p=list(sigma=1)),hcauchy=list(fun=dhcauchy,p=list(sigma=1)),ht=list(fun=dht,p=list(nu=1,sigma=1)),foldnorm=list(fun=dfoldnorm,p=list(mu=1,sigma=1)),fatigue=list(fun=dfatigue,p=list(alpha=1,beta=1,mu=0)),betapr=list(fun=dbetapr,p=list(shape1=1,shape2=1,scale=1)))
res=list(); j=0; for(id in names(refs)) for(p in names(refs[[id]]$p)) for(v in c(-1,0,Inf,-Inf)) {args=refs[[id]]$p;args[[p]]=v; d=suppressWarnings(do.call(refs[[id]]$fun,c(list(x=1),args)));j=j+1;res[[j]]=data.frame(id=id,param=p,value=v,density=d)}
write.csv(do.call(rbind,res),file.path(outdir,'domain-results.csv'),row.names=FALSE)
cat('wrote',nr,'path observations and',length(ans),'restricted cases\n')
# Held location at, below, and outside support, including a coincident-only sample.
heldrows=list(); j=0
for(id in c('hnorm','hcauchy','ht')) for(x in list(c(1,2,3,4,5),rep(1,5),c(1,1,2,3,4))) for(m in c(0,1,1.5)) {
 h=c(mu=m); o=oracle(id,x,h); j=j+1; heldrows[[j]]=data.frame(id=id,sample=paste(x,collapse=';'),mu=m,ll=o$ll,par=paste(names(o$par),signif(o$par,6),collapse=';'))
}
write.csv(do.call(rbind,heldrows),file.path(outdir,'held-threshold-results.csv'),row.names=FALSE)
