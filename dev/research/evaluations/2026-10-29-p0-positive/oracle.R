# Reference-density multistart oracle and master comparison; run from repo root.
# Uses only master metadata for density names/defaults, never master fit results
# as optimizer starts. Outputs one CSV per family for resumability.
library(univariateML, lib.loc = "/tmp/umvl-p0lib")
outdir <- "dev/research/evaluations/2026-10-29-p0-positive"
ids <- c("exp","gamma","weibull","lnorm","invgamma","invgauss","invweibull",
         "invexp","llogis","rayleigh","naka","lomax","gompertz","sgomp",
         "lgamma","invpareto","pareto","power")
formula_ll <- function(id,x,p) {
 if(id=="naka") {m<-p[["shape"]]; o<-p[["scale"]]; return(log(2)+m*log(m)-lgamma(m)-m*log(o)+(2*m-1)*log(x)-m*x*x/o)}
 if(id=="sgomp") {b<-p[["b"]]; e<-p[["eta"]]; return(log(b)-b*x-e*exp(-b*x)+log1p(e*(-expm1(-b*x))))}
 stop("no own formula")
}
set.seed(20261029)
for(id in ids) {
 model<-paste0("ml",id); meta<-univariateML:::univariateML_metadata[[model]]
 pn<-meta$names; def<-setNames(meta$default,pn)
 bits<-strsplit(meta$density,"::",fixed=TRUE)[[1]]
 dens<-if(length(bits)==2) getExportedValue(bits[1],bits[2]) else getExportedValue("univariateML",bits[1])
 dlog<-function(x,p) suppressWarnings(do.call(dens,c(list(x=x),as.list(p),list(log=TRUE))))
 samples<-setNames(lapply(c(2,3,5,20,100,1000),function(n) simulate_default(model,n)),paste0("n",c(2,3,5,20,100,1000)))
 samples$ties<-rep(samples$n5,each=2)
 samples$constant<-rep(samples$n5[1],5)
 matched<-switch(id,weibull=4,lnorm=1,invgauss=3,invweibull=1/3,
                 llogis=1/2,naka=sqrt(3),NULL)
 if(!is.null(matched)) samples$held_match<-rep(matched,5)
 if(id %in% c("exp","rayleigh","gompertz")) {samples$all_zero<-rep(0,5);samples$zero_mix<-c(0,1,2,3)}
 # Named limit laws and relevant endpoint/collapse face samples.
 if(id %in% c("lomax","gompertz","sgomp")) samples$limit_exp<-rexp(100,1)
 if(id=="invpareto") samples$limit_invexp<-1/rexp(100,1)
 if(id=="invgauss") samples$limit_levy<-1/rnorm(100)^2
 if(id=="naka") samples$limit_halfnormal<-abs(rnorm(100))
 if(id=="gompertz") samples$held_a_escape<-c(2,3,4)
 if(id=="sgomp") {samples$all_zero<-rep(0,5);samples$zero_mix<-c(0,1,2,3)}
 if(id=="lomax") samples$heavy_pair<-c(1,100)
 if(id=="invpareto") samples$heavy_pair<-c(1,.01)
 holds<-if(length(pn)==1) list(character()) else c(list(character()),as.list(pn))
 records<-list()
 for(sn in names(samples)) { x<-samples[[sn]]
  for(h in holds) {
   free<-setdiff(pn,h); hv<-def
   if(id=="pareto") hv["b"]<-min(x)*.8
   if(id=="power") hv["alpha"]<-max(x)*1.2
   # Treat Pareto endpoint as an attained analytic boundary; power endpoint
   # is approached strictly from above. Other positive parameters use exp.
   boundary<-id=="pareto" && "b" %in% free
   optfree<-if(boundary) setdiff(free,"b") else free
   encode<-function(v) {
    if(id=="power" && "alpha" %in% optfree) v["alpha"]<-max(1e-4,v["alpha"]-max(x))
    z<-log(v[optfree]);if("meanlog" %in% optfree) z["meanlog"]<-v["meanlog"];z
   }
   decode<-function(z,zeroeta=FALSE) {
    v<-hv
    if(length(optfree)) v[optfree]<-exp(pmin(setNames(z,optfree),600))
    if("meanlog" %in% optfree) v["meanlog"]<-z[match("meanlog",optfree)]
    if(id=="power" && "alpha" %in% optfree) v["alpha"]<-max(x)+exp(z[match("alpha",optfree)])
    if(boundary) v["b"]<-min(x)
    if(zeroeta) v["eta"]<-0
    v
   }
   objective<-function(z,zeroeta=FALSE) {
    bounds<-rep(40,length(z)); if(id=="sgomp" && "eta" %in% optfree) bounds[match("eta",optfree)]<-300
    if(any(!is.finite(z)) || any(abs(z)>bounds)) return(1e100)
    p<-decode(z,zeroeta); ll<-tryCatch(dlog(x,p),error=function(e) NA_real_)
    if(id=="gompertz" && p[["b"]]*max(x)>500) return(1e100)
    # extraDistr loses exp(b*x)-1 when b is tiny, spuriously returning a
    # very high likelihood. Reject evaluations that disagree with expm1.
    if(id=="gompertz" && all(is.finite(ll))) {
      stable<-log(p[["a"]])+p[["b"]]*x-p[["a"]]/p[["b"]]*expm1(p[["b"]]*x)
      if(any(!is.finite(stable)) || max(abs(ll-stable))>1e-7) return(1e100)
    }
    if(length(ll)!=length(x)||any(!is.finite(ll))) return(1e100)
    -sum(ll)
   }
   start<-encode(hv)
   if("mean" %in% optfree) start["mean"]<-log(mean(x))
   if("meanlog" %in% optfree) start["meanlog"]<-mean(log(x))
   if(id=="sgomp" && "b" %in% optfree && sum(x)>0) start["b"]<-log(length(x)/sum(x))
   starts<-lapply(c(-3,-1,0,1,3),function(d) start+d)
   if(id=="sgomp" && all(c("b","eta") %in% optfree) && length(unique(x))>1 && min(x)>0) {
     b0<-min(1000,2/(max(x)-min(x))); starts[[length(starts)+1]]<-c(b=log(b0),eta=min(250,b0*min(x)))
   }
   if(id=="power" && "alpha" %in% optfree) {p<-start;p["alpha"]<-log(max(x))-25;starts[[length(starts)+1]]<-p}
   opts<-if(!length(optfree)) list(list(value=objective(numeric()),par=numeric(),convergence=0)) else lapply(starts,function(st) tryCatch(optim(st,objective,method="BFGS",control=list(maxit=1000,reltol=1e-12)),error=function(e) NULL))
   # In tightly clustered shifted Gompertz samples, log(eta) and b*min(x)
   # are large and nearly equal. Optimize their difference to avoid a long
   # curved ridge, then compare on the reference density as above.
   if(id=="sgomp" && all(c("b","eta") %in% optfree) && length(unique(x))>1 && min(x)>0) {
    qobj<-function(q) objective(c(q[1],q[2]+exp(q[1])*min(x)))
    for(b0 in c(1/mean(x), 1/(max(x)-min(x)),2/(max(x)-min(x)))) {
     z<-tryCatch(optim(c(log(b0),0),qobj,method="BFGS",control=list(maxit=1000,reltol=1e-12)),error=function(e) NULL)
     if(!is.null(z)) opts[[length(opts)+1]]<-z
    }
   }
   opts<-Filter(Negate(is.null),opts)
   # Include the attained eta=0 face explicitly.
   if(id=="sgomp" && "eta" %in% free) {
    if("b" %in% free && sum(x)>0) p0<-c(b=length(x)/sum(x),eta=0) else p0<-c(b=hv[["b"]],eta=0)
    ll0<-tryCatch(sum(dlog(x,p0)),error=function(e) -Inf)
    opts[[length(opts)+1]]<-list(value=-ll0,par=NA,convergence=0,face=TRUE)
   }
   values<-if(length(opts)) vapply(opts,`[[`,0.0,"value") else Inf
   ib<-which.min(values); best<-if(length(ib)&&is.finite(values[ib])&&values[ib]<1e90) -values[ib] else NA_real_
   args<-c(list(x=x),as.list(hv[h]))
   fit<-tryCatch(suppressWarnings(do.call(get(model),args)),error=function(e)e)
   if(inherits(fit,"error")) {pkgll<-pkgrell<-NA_real_; err<-conditionMessage(fit)} else {
    pkgll<-as.numeric(logLik(fit)); err<-""; pfit<-setNames(as.numeric(fit[pn]),pn)
    pkgrell<-tryCatch(sum(dlog(x,pfit)),error=function(e) NA_real_)
   }
   fixed<-if(inherits(fit,"error")) "" else paste(attr(fit,"fixed"),collapse="+")
   ownerr<-if(id %in% c("naka","sgomp")) max(abs(dlog(x,def)-formula_ll(id,x,def))) else NA_real_
   records[[length(records)+1]]<-data.frame(id=id,sample=sn,n=length(x),held=paste(h,collapse="+"),fixed=fixed,oracle=best,pkg=pkgll,pkg_ref=pkgrell,delta=pkgll-best,own_formula_error=ownerr,error=err)
  }
 }
 write.csv(do.call(rbind,records),file.path(outdir,paste0("oracle-",id,".csv")),row.names=FALSE)
 cat(id,":",length(records),"rows\n");flush.console()
}
