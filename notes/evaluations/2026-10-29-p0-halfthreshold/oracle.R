# Rscript from repository root; independent reference likelihood and multistart.
suppressPackageStartupMessages(library(univariateML,lib.loc='/tmp/umvl-p0lib'))
suppressPackageStartupMessages(library(extraDistr))
set.seed(20261029)
outdir='notes/evaluations/2026-10-29-p0-halfthreshold'
defs=list(hnorm=c(mu=0,sigma=1),hcauchy=c(mu=0,sigma=1),ht=c(mu=0,nu=3,sigma=1),foldnorm=c(mu=2,sigma=1),fatigue=c(alpha=3,beta=2,mu=0),betapr=c(shape1=2,shape2=3,scale=1))
lfold=function(x,mu,sigma) -log(sigma)-log(2*pi)/2-(x-mu)^2/(2*sigma^2)+log1p(exp(-2*x*mu/sigma^2))
ll=function(id,x,p) {
 if(id %in% c('hnorm','hcauchy','ht')) {z=x-p['mu']; if(any(z<0)) return(-Inf); f=switch(id,hnorm=dhnorm,hcauchy=dhcauchy,ht=dht); return(sum(do.call(f,c(list(x=z,log=TRUE),as.list(p[names(p)!='mu'])))))}
 if(id=='foldnorm') return(sum(lfold(x,p['mu'],p['sigma'])))
 if(id=='fatigue' && any(x<=p['mu'])) return(-Inf)
 f=switch(id,fatigue=dfatigue,betapr=dbetapr); sum(do.call(f,c(list(x=x,log=TRUE),as.list(p))))
}
sets=function(nm) unlist(lapply(0:(length(nm)-1),function(k) combn(nm,k,simplify=FALSE)),recursive=FALSE)
oracle=function(id,x,held,nu_lower=0) {
 p0=defs[[id]]; p0[names(held)]=held; free=setdiff(names(p0),names(held)); shift=id%in%c('hnorm','hcauchy','ht','fatigue'); edgeable=id%in%c('hnorm','hcauchy','ht','foldnorm') && 'mu'%in%free
 fitone=function(edge=FALSE) {
  active=if(edge) setdiff(free,'mu') else free; m=length(active)
  decode=function(v) {p=p0; if(edge) p['mu']=if(id=='foldnorm') 0 else min(x); for(i in seq_along(active)) {nm=active[i]; p[nm]=if(nm=='mu'&&shift) min(x)-exp(v[i]) else if(nm=='nu'&&nu_lower>0) nu_lower+exp(v[i]) else exp(v[i])}; p}
  obj=function(v) {z=tryCatch(suppressWarnings(ll(id,x,decode(v))),error=function(e) -Inf); if(is.finite(z)) -z else 1e100}
  if(!m) return(list(ll=-obj(numeric()),par=decode(numeric()),conv=0))
  span=max(diff(range(x)), median(abs(x)),.01); init=sapply(active,function(nm) if(nm=='mu'&&shift) log(span) else if(nm=='sigma'||nm=='beta'||nm=='scale') log(span) else log(max(p0[nm] - if(nm=='nu') nu_lower else 0,.1)))
  starts=list(init,init+0.8,init-0.8,init+c(-1,1,0)[seq_len(m)])
  fits=lapply(starts,function(start) tryCatch(nlminb(start,obj,lower=rep(-20,m),upper=rep(15,m),control=list(iter.max=400,eval.max=800,rel.tol=1e-11)),error=function(e) NULL))
  fits=Filter(Negate(is.null),fits); if(!length(fits)) return(list(ll=-Inf,par=p0,conv=99)); best=fits[[which.min(vapply(fits,`[[`,0.,'objective'))]]; list(ll=-best$objective,par=decode(best$par),conv=best$convergence)
 }
 a=fitone(); if(edgeable) {b=fitone(TRUE); if(b$ll>a$ll) a=b}
 if(id=='ht' && nu_lower>0 && 'nu'%in%free) {b=oracle(id,x,c(held,nu=nu_lower)); if(b$ll>a$ll) a=b}
 a
}
faces=function(id,x,held) {
 p=defs[[id]]; p[names(held)]=held; z=x-if('mu'%in%names(held)) held['mu'] else min(x)
 if(id=='ht' && !('nu'%in%names(held))) {if(any(z<0)) return(c(normal=-Inf)); s=if('sigma'%in%names(held)) held['sigma'] else sqrt(mean(z^2)); return(c(normal=if(s>0) sum(dhnorm(z,s,log=TRUE)) else Inf))}
 if(id!='betapr'||'scale'%in%names(held)) return(c())
 a=if('shape1'%in%names(held)) held['shape1'] else NA_real_; b=if('shape2'%in%names(held)) held['shape2'] else NA_real_
 # Independent gamma/inverse-gamma fits via reference densities in log coordinates.
 gammaface=function(inv,shape) {y=if(inv) 1/x else x; f=function(u) {sh=if(is.na(shape)) exp(u[1]) else shape; sc=exp(u[length(u)]); sum(dgamma(y,shape=sh,scale=sc,log=TRUE)) - if(inv) 2*sum(log(x)) else 0}; init=if(is.na(shape)) c(0,log(mean(y))) else log(mean(y)/shape); -nlminb(init,function(u) -f(u),lower=rep(-15,length(init)),upper=rep(15,length(init)))$objective}
 v=c(); if(is.na(b)) v=c(v,gamma=gammaface(FALSE,a)); if(is.na(a)) v=c(v,invgamma=gammaface(TRUE,b)); v
}
sim=function(id,n) switch(id,hnorm=rhnorm(n,1),hcauchy=rhcauchy(n,1),ht=rht(n,3,1),foldnorm=abs(rnorm(n,2,1)),fatigue=rfatigue(n,3,2,0),betapr=rbetapr(n,2,3,1))
implemented=function(id,h) switch(id,hnorm=identical(h,character(0))||identical(h,'mu'),hcauchy=identical(h,'mu'),ht='mu'%in%h,foldnorm=TRUE,fatigue=identical(h,'mu'),betapr=identical(h,character(0))||identical(h,'scale'))
rows=list(); nr=0L; foldmax=0
for(id in names(defs)) {
 cat('family',id,'\n'); flush.console()
 samples=lapply(c(2,3,5,20,100,1000),function(n) sim(id,n)); names(samples)=paste0('default',c(2,3,5,20,100,1000))
 samples$tie=c(1,1,1,2,3,4,5); samples$constant=rep(1,5)
 samples$endpoint=c(0,1,2,3,4); samples$endpoint_equal=c(0,0,1,2)
 if(id %in% c('fatigue','betapr')) {samples$endpoint=NULL; samples$endpoint_equal=NULL}
 if(id %in% c('hnorm','ht','foldnorm')) samples$normal=abs(rnorm(40))
 if(id=='hcauchy') samples$hcauchy=abs(rcauchy(40))
 if(id=='fatigue') {samples$gammahalf=rgamma(40,.5,scale=2); samples$invgammahalf=1/rgamma(40,.5,scale=2); samples$normal=rnorm(40,10,1)}
 if(id=='betapr') {samples$gamma=rgamma(40,2); samples$invgamma=1/rgamma(40,3)}
 for(sn in names(samples)) for(h in sets(names(defs[[id]]))) {
  x=samples[[sn]]; held=defs[[id]][h]; if('mu'%in%h && id!='foldnorm') held['mu']=0
  a=oracle(id,x,held); face=faces(id,x,held); fit=NULL; pkgstatus='not implemented'; pl=NA_real_; cp=''
  if(implemented(id,h)) {args=as.list(held); if(id%in%c('hcauchy','ht','fatigue')) args$mu=NULL; fit=tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),args))),error=function(e) conditionMessage(e)); if(is.character(fit)) pkgstatus=fit else {pkgstatus='ok'; pl=as.numeric(logLik(fit)); cp=paste(names(coef(fit)),signif(coef(fit),8),collapse=';')}}
  if(id=='foldnorm') {ours=lfold(x,2,1); reference=dfoldnorm(x,2,1,log=TRUE); foldmax=max(foldmax,max(abs(ours-reference)))}
  nr=nr+1L; rows[[nr]]=data.frame(id=id,sample=sn,n=length(x),held=paste(h,collapse=','),oracle_ll=a$ll,oracle_par=paste(names(a$par),signif(a$par,7),collapse=';'),convergence=a$conv,face=if(length(face)) paste(names(face),signif(face,9),collapse=';') else '',face_ll=if(length(face)) max(face) else NA_real_,package_ll=pl,package_par=cp,package_status=pkgstatus)
 }
 write.csv(do.call(rbind,rows),file.path(outdir,'oracle-results.csv'),row.names=FALSE)
}
cat('cases',nr,'folded-density max difference',foldmax,'\n')
