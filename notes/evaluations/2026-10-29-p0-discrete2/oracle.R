# Independent likelihood searches; run from repository root. Results are scratch, not goldens.
library(univariateML, lib.loc='/tmp/umvl-p0lib')
library(extraDistr)
set.seed(791029)
D <- list(cmp=c(lambda=4,nu=1.5),yulesimon=c(rho=2),zeta=c(s=2),zipf=c(N=3,s=.5),tpois=c(lambda=3,a=0,b=10),skellam=c(mu1=4,mu2=3),dlaplace=c(location=0,scale=.4),dnorm=c(mean=0,sd=1),dgamma=c(shape=2,rate=1),dweibull=c(shape1=.8,shape2=1.5),dunif=c(min=2,max=10),bnbinom=c(size=1,alpha=2,beta=5))
# reference PMFs; explicit convergent sums for own families
ll <- function(id,x,p) {
  z <- switch(id,
    cmp={if(p['lambda']==0) return(if(max(x)==0) 0 else -Inf); k=0:2000; w=k*log(p['lambda'])-p['nu']*lgamma(k+1); if(w[2001]>max(w)-30) return(-Inf); m=max(w); x*log(p['lambda'])-p['nu']*lgamma(x+1)-m-log(sum(exp(w-m)))},
    yulesimon=log(p['rho'])+lbeta(x,p['rho']+1),
    zeta={k=1:2048; n=2048; t=p['s']; z=sum(k^(-t))+n^(1-t)/(t-1)-.5*n^(-t)+t/12*n^(-t-1)-t*(t+1)*(t+2)/720*n^(-t-3); -p['s']*log(x)-log(z)},
    zipf={if(p['N']<max(x)) return(-Inf); -p['s']*log(x)-log(sum((1:p['N'])^(-p['s'])))},
    tpois=dtpois(x,p['lambda'],p['a'],p['b'],log=TRUE),
    skellam={a=p['mu1'];b=p['mu2']; if(a==0)return(sum(dpois(-x,b,log=TRUE))); if(b==0)return(sum(dpois(x,a,log=TRUE))); if(a<0||b<0)return(-Inf); -(sqrt(a)-sqrt(b))^2+x/2*log(a/b)+log(besselI(2*sqrt(a*b),abs(x),expon.scaled=TRUE))},
    dlaplace=ddlaplace(x,p['location'],p['scale'],log=TRUE),
    dnorm=ddnorm(x,p['mean'],p['sd'],log=TRUE),
    dgamma=ddgamma(x,p['shape'],p['rate'],log=TRUE),
    dweibull=ddweibull(x,p['shape1'],p['shape2'],log=TRUE),
    dunif=ddunif(x,p['min'],p['max'],log=TRUE),
    bnbinom=dbnbinom(x,p['size'],p['alpha'],p['beta'],log=TRUE))
  if(any(!is.finite(z))) -Inf else sum(z)
}
encode <- function(id,p,nm) {v=p[nm]; if(id=='dlaplace' && nm=='scale'||id=='dweibull' && nm=='shape1') qlogis(v) else if(id=='dnorm' && nm=='mean') v else if(id=='zeta') log(v-1) else log(v)}
decode <- function(id,u,nm) {if(id=='dlaplace' && nm=='scale'||id=='dweibull' && nm=='shape1') plogis(u) else if(id=='dnorm' && nm=='mean') u else if(id=='zeta') 1+exp(u) else exp(u)}
fit <- function(id,x,p,held) {
 free=setdiff(names(p),held); discrete=intersect(free,switch(id,zipf='N',dunif=c('min','max'),tpois=c('a','b'),dlaplace='location',character()))
 grids=lapply(discrete,function(v) switch(v,N=max(x),min=min(x),max=max(x),a=min(x)-1,b=max(x),location=sort(x)[ceiling(length(x)/2)])); names(grids)=discrete
 candidates=if(length(grids)) expand.grid(grids) else data.frame(.dummy=1)
 continuous=setdiff(free,discrete); best=-Inf
 for(i in seq_len(nrow(candidates))) { q=p; if(length(discrete)) q[discrete]=unlist(candidates[i,discrete,drop=FALSE]); if(!length(continuous)) {best=max(best,ll(id,x,q));next}
  start=sapply(continuous,function(v) encode(id,q,v)); fn=function(u){if(any(abs(u[continuous!='mean'])>9))return(1e100); for(j in seq_along(u)) q[continuous[j]]=decode(id,u[j],continuous[j]); z=ll(id,x,q);if(is.finite(z)) -z else 1e100}
  if(length(continuous)==1) {lo=if(continuous=='mean') min(x)-8 else -9; hi=if(continuous=='mean') max(x)+8 else 9; o=try(optimize(fn,c(lo,hi),tol=1e-9),silent=TRUE);if(!inherits(o,'try-error') && o$objective<1e90) best=max(best,-o$objective)} else for(offset in c(-1,0,1)) {o=try(optim(start+offset,fn,method='BFGS',control=list(maxit=350,reltol=1e-10)),silent=TRUE);if(!inherits(o,'try-error') && o$value<1e90) best=max(best,-o$value)}
 }; if(id=='zipf' && 's' %in% free) {q=p;q['N']=if('N' %in% free) max(x) else p['N'];q['s']=0;best=max(best,ll(id,x,q))}; if(id=='cmp' && 'lambda' %in% free && max(x)==0) best=max(best,0); if(id=='cmp' && 'nu' %in% free && (('lambda' %in% free && mean(x)>0) || p['lambda']<1)) {q=p;q['nu']=0;if('lambda' %in% free) q['lambda']=mean(x)/(1+mean(x));best=max(best,ll(id,x,q))}; if(id=='skellam') {if(!('mu2' %in% held) && min(x)>=0) best=max(best,sum(dpois(x,if('mu1' %in% held) p['mu1'] else mean(x),log=TRUE))); if(!('mu1' %in% held) && max(x)<=0) best=max(best,sum(dpois(-x,if('mu2' %in% held) p['mu2'] else -mean(x),log=TRUE)))}; best
}
out=list(); ids=commandArgs(trailingOnly=TRUE); if(!length(ids)) ids=names(D); for(id in ids) { set.seed(791029+match(id,names(D))); p=D[[id]]; samples=c(lapply(c(20,100,1000),function(n)simulate_default(paste0('ml',id),n)),list(rep(1L,5),c(0L,0L,1L,1L,2L),c(1L,2L),c(1L,1L,2L),c(0L,1L,2L,2L,3L), rpois(20,2), rnbinom(20,size=2,mu=3), rgeom(20,.5), rbinom(20,1,.5), rep(0L,5),3L+rbinom(20,1,.4))); if(id %in% c('zipf','zeta','yulesimon')) samples=lapply(samples,function(x) pmax(1L,x)); if(id=='tpois') samples=lapply(samples,function(x) pmax(1L,pmin(10L,x))); if(id=='dunif') samples=lapply(samples,function(x) pmax(2L,pmin(10L,x))); if(id=='zipf') samples=c(samples,list(univariateML::rzeta(20,2))); saveRDS(samples,paste0('notes/evaluations/2026-10-29-p0-discrete2/samples-',id,'.rds'))
 for(si in seq_along(samples)) {x=samples[[si]]; for(mask in 0:(2^length(p)-2)) { held=names(p)[as.logical(intToBits(mask)[seq_along(p)])]; args=as.list(p[held]); if(id=='tpois') {args$a=p['a'];args$b=p['b']; if(!all(c('a','b') %in% held)) args=NULL}; o=try(fit(id,x,p,held),silent=TRUE); m=if(is.null(args)) structure('unsupported bounds',class='try-error') else try(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),args))),silent=TRUE); ml=if(inherits(m,'try-error')) NA_real_ else as.numeric(logLik(m)); ref=if(inherits(m,'try-error')) NA_real_ else tryCatch(ll(id,x,setNames(as.numeric(m[names(p)]),names(p))),error=function(e) NA_real_); out[[length(out)+1]]=data.frame(id=id,sample=si,n=length(x),held=paste(held,collapse=','),oracle=if(inherits(o,'try-error')) NA_real_ else o,master=ml,reference_at_fit=ref,status=if(inherits(m,'try-error')) substr(as.character(m),1,160) else 'ok') }
 }; write.csv(do.call(rbind,out),paste0('notes/evaluations/2026-10-29-p0-discrete2/oracle-',id,'.csv'),row.names=FALSE); out=list(); cat(id,'done\n'); flush.console() }
