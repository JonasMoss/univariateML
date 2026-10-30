# Multistart likelihood on external reference densities and explicit endpoints.
# Run with Rscript from repository root. PIG additionally uses the Bessel formula,
# independently of the package recurrence, to identify actuar numerical failures.
suppressPackageStartupMessages(library(univariateML,lib.loc='/tmp/umvl-p0lib'))
ids <- c('pois','geom','nbinom','zip','zinb','zmpois','ztpois','zmgeom','ztgeom','zmnbinom','ztnbinom','zmlogarithmic','lgser','pig')
pars <- list(pois=c(lambda=2),geom=c(prob=.5),nbinom=c(size=10,prob=.3),zip=c(lambda=1,pi=.5),zinb=c(size=2,prob=.4,pi=.3),zmpois=c(lambda=2,p0=.3),ztpois=c(lambda=2),zmgeom=c(prob=.4,p0=.3),ztgeom=c(prob=.4),zmnbinom=c(size=2,prob=.4,p0=.3),ztnbinom=c(size=2,prob=.4),zmlogarithmic=c(prob=.4,p0=.3),lgser=c(theta=.9),pig=c(mean=2,shape=3))
dens <- list(pois=stats::dpois,geom=stats::dgeom,nbinom=stats::dnbinom,zip=extraDistr::dzip,zinb=extraDistr::dzinb,zmpois=actuar::dzmpois,ztpois=actuar::dztpois,zmgeom=actuar::dzmgeom,ztgeom=actuar::dztgeom,zmnbinom=actuar::dzmnbinom,ztnbinom=actuar::dztnbinom,zmlogarithmic=actuar::dzmlogarithmic,lgser=extraDistr::dlgser,pig=actuar::dpoisinvgauss)
endpoints <- function(id,p) {
 if(p %in% c('p0','pi')) return(c(0,1))
 if(p=='prob') return(if(id=='zmlogarithmic') 0 else 1)
 if(p=='size' && id %in% c('nbinom','zinb','zmnbinom','ztnbinom')) return(0)
 if(p=='lambda' && id %in% c('pois','ztpois','zmpois')) return(0)
 if(p=='shape' && id=='pig') return(0)
 if(p=='mean' && id=='pig') return(Inf)
 numeric()
}
pig_bessel <- function(x,mean,shape) {
 if(shape==0) return(ifelse(x==0,0,-Inf))
 A<-2+shape/mean^2; z<-sqrt(shape*A)
 bk<-besselK(z,abs(x-.5),expon.scaled=TRUE)
 .5*log(2*shape/pi)+shape/mean+(x-.5)/2*log(shape/A)+log(bk)-z-lgamma(x+1)
}
fit_one <- function(id,x,held,base) {
 nm<-names(base); free<-setdiff(nm,held); ux<-sort(unique(x)); freq<-tabulate(match(x,ux));
 ll <- function(q,stable=FALSE) {
  v<-suppressWarnings(tryCatch(if(id=='pig' && stable) pig_bessel(ux,q['mean'],q['shape']) else do.call(dens[[id]],c(list(x=ux),as.list(q),list(log=TRUE))),error=function(e) rep(NaN,length(ux))))
  if(length(v)!=length(ux)||anyNA(v)||any(v>1e-8)||any(v==Inf)) return(-Inf)
  sum(v*freq)
 }
 # Every combination of free parameter endpoint and interior; size/lambda endpoints
 # checked explicitly against the density, not inferred from a transformation.
 options<-lapply(free,function(p)c(NA_real_,endpoints(id,p)))
 grid<-if(length(free)) expand.grid(options,KEEP.OUT.ATTRS=FALSE) else data.frame(dummy=1)
 best<--Inf; bestq<-base; bref<--Inf
 for(i in seq_len(nrow(grid))) {
  q<-base; v<-as.numeric(grid[i,seq_along(free),drop=TRUE]); ep<-!is.na(v)
  if(any(ep)) q[free[ep]]<-v[ep]
  live<-free[!ep]; pr<-live %in% c('prob','p0','pi','theta')
  calc<-function(z) { qq<-q; qq[live]<-ifelse(pr,plogis(z),exp(z)); qq }
  if(!length(live)) {qq<-q; val<-ll(qq,id=='pig'); ref<-ll(qq); if(val>best){best<-val;bestq<-qq;bref<-ref}; next}
  z0<-ifelse(pr,qlogis(base[live]),log(base[live])); lo<-ifelse(pr,-14,-12); hi<-ifelse(pr,14,if(id=='pig') 12 else 14)
  for(s in c(-2,0,2)) {
   o<-tryCatch(optim(pmax(lo,pmin(hi,z0+s)),function(z) {v<-ll(calc(z),id=='pig'); if(is.finite(v)) -v else 1e100},method='L-BFGS-B',lower=lo,upper=hi,control=list(maxit=250,factr=1e6)),error=function(e) NULL)
   if(!is.null(o)) {qq<-calc(o$par); val<-ll(qq,id=='pig'); if(val>best){best<-val;bestq<-qq;bref<-ll(qq)}}
  }
 }
 fit<-tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),as.list(base[held])))),error=function(e)e)
 fitll<-if(inherits(fit,'error')) NA_real_ else as.numeric(logLik(fit))
 list(oracle=if(is.finite(best))best else NA_real_,reference=if(is.finite(bref))bref else NA_real_,q=paste(sprintf('%s=%.9g',names(bestq),bestq),collapse=';'),fit=fitll,error=if(inherits(fit,'error')) conditionMessage(fit) else '')
}
set.seed(791030); rows<-list(); j<-0L
for(id in ids) {
 base<-pars[[id]]; nm<-names(base); pos<-id %in% c('ztpois','ztgeom','ztnbinom','lgser')
 samples<-setNames(lapply(c(20,100,1000),function(n) simulate_default(paste0('ml',id),n)),c('default20','default100','default1000'))
 samples<-c(samples,list(ties=if(pos)c(1,1,1,2,2,4) else c(0,0,0,1,1,4), n2=if(pos)c(1,2) else c(0,2), n3=if(pos)c(1,1,4) else c(0,0,4), n5=if(pos)c(1,2,2,3,5) else c(0,1,1,2,4), all1=rep(1L,5), poisson=if(pos) actuar::rztpois(100,2) else rpois(100,2)))
 if(!pos) samples$all0<-rep(0L,5)
 if(id %in% c('zinb','zip')) samples$ziplimit<-extraDistr::rzip(100,2,.35)
 if(id=='zinb') samples$nblimit<-rnbinom(100,2,.4)
 if(id=='zmnbinom') { samples$zmpoislimit<-actuar::rzmpois(100,2,.3); samples$loglimit<-actuar::rzmlogarithmic(100,.6,.3) }
 if(id=='ztnbinom') samples$loglimit<-actuar::rlogarithmic(100,.6)
 if(id=='pig') { samples$overdispersed<-actuar::rpoisinvgauss(100,2,.3); oldseed<-.Random.seed;set.seed(791033);samples$levylimit<-actuar::rpoisinvgauss(5,Inf,3);.Random.seed<-oldseed }
 for(case in names(samples)) for(mask in 0:(2^length(nm)-2)) {
  held<-nm[as.logical(intToBits(mask)[seq_along(nm)])]; configs<-list(default=base)
  if(length(held) && case %in% c('ties','all0','all1','n2')) for(p in held) for(e in endpoints(id,p)) {b<-base;b[p]<-e;configs[[paste0(p,'=',e)]]<-b}
  if('p0' %in% held && case=='ties') {b<-base;b['p0']<-mean(samples[[case]]==0);configs[['p0=zero-share']]<-b}
  for(config in names(configs)) {
   x<-samples[[case]]; ans<-fit_one(id,x,held,configs[[config]])
   j<-j+1L;rows[[j]]<-data.frame(id=id,case=case,n=length(x),held=paste(held,collapse='+'),holdcase=config,oracle=ans$oracle,reference=ans$reference,fit=ans$fit,diff=ans$fit-ans$oracle,oracle_parameters=ans$q,error=ans$error)
  }
 }
 cat(id, 'complete;',j,'rows\n'); flush.console()
}
dir.create('dev/research/evaluations/2026-10-29-p0-counts',showWarnings=FALSE,recursive=TRUE)
write.csv(do.call(rbind,rows),'dev/research/evaluations/2026-10-29-p0-counts/full-oracle.csv',row.names=FALSE,na='')
