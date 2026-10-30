# Independent likelihood search. Run from repo root: Rscript .../oracle.R [group]
library(univariateML, lib.loc='/tmp/umvl-p0lib')
args <- commandArgs(trailingOnly=TRUE); group <- if(length(args)) args[1] else 'all'
ids <- switch(group, shapes=c('beta','kumar','logitnorm'), endpoints=c('unif','triang'), all=c('beta','kumar','logitnorm','unif','triang'),stop('group'))
outdir <- 'dev/research/evaluations/2026-10-29-p0-bounded'
set.seed(791029)
params <- list(beta=c(shape1=2,shape2=3),kumar=c(a=2,b=3),logitnorm=c(mu=2,sigma=3),unif=c(min=-1,max=2),triang=c(a=-1,b=2,c=.4))
ll <- function(id,x,p) suppressWarnings(switch(id,
 beta=sum(dbeta(x,p[1],p[2],log=TRUE)),
 kumar=sum(extraDistr::dkumar(x,p[1],p[2],log=TRUE)),
 logitnorm=sum(-log(p[2])-.5*log(2*pi)-log(x)-log1p(-x)-(qlogis(x)-p[1])^2/(2*p[2]^2)),
 unif=sum(dunif(x,p[1],p[2],log=TRUE)),
 triang=sum(extraDistr::dtriang(x,p[1],p[2],p[3],log=TRUE))))
# Search triangular likelihood with mode candidates and optimized endpoints.
# Conditional on endpoints, the likelihood is convex in c between data
# values; maxima occur at observations (or forced endpoint corners).
tri <- function(x,h) {
 m<-min(x); M<-max(x); sc<-max(M-m,1); const<-m==M
 if(('a'%in%names(h)&&h['a']>m)||('b'%in%names(h)&&h['b']<M)||
    (all(c('a','b')%in%names(h))&&h['a']>=h['b'])||
    (all(c('a','c')%in%names(h))&&h['a']>h['c'])||
    (all(c('b','c')%in%names(h))&&h['b']<h['c'])) return(list(ll=NA_real_,why='infeasible'))
 if(const && all(h==m)) return(list(ll=Inf,why='collapse'))
 modes<-if('c'%in%names(h)) unname(h['c']) else sort(unique(c(x,unname(h[intersect(c('a','b'),names(h))]))))
 if('a'%in%names(h)&&h['a']==m) modes<-intersect(modes,unname(h['a']))
 if('b'%in%names(h)&&h['b']==M) modes<-intersect(modes,unname(h['b']))
 if(!length(modes)) return(list(ll=NA_real_,why='infeasible'))
 getbest <- function(mode) {
  L<-min(m,mode); U<-max(M,mode); free<-setdiff(c('a','b'),names(h))
  choices <- list(character())
  if('a'%in%free && mode<=m) choices<-c(choices,list('a'))
  if('b'%in%free && mode>=M) choices<-c(choices,list('b'))
  best <- -Inf
  for(corner in choices) {
   f<-setdiff(free,corner)
   make<-function(z) {p<-c(a=L-sc,b=U+sc,c=mode);p[names(h)]<-h;p[corner]<-mode;for(i in seq_along(f))p[f[i]]<-if(f[i]=='a') L-sc*exp(z[i]) else U+sc*exp(z[i]);p}
   fun<-function(z){p<-make(z);if(p[1]>=p[2]||p[1]>m||p[2]<M||p[3]<p[1]||p[3]>p[2])return(1e50);v<-ll('triang',x,p);if(is.finite(v)) -v else 1e50}
   if(!length(f)) best<-max(best,-fun(numeric())) else for(s in c(-2,0,1)) {o<-tryCatch(optim(rep(s,length(f)),fun,method='L-BFGS-B',lower=rep(-20,length(f)),upper=rep(8,length(f)),control=list(maxit=250,factr=1e5)),error=function(e)NULL);if(!is.null(o))best<-max(best,-o$value)}
  };best
 }
 # Exhaust small/medium samples; for n>100 use evenly spaced modes, then
 # inspect neighborhoods around the best candidates. This is a lower bound.
 idx<-if(length(modes)<=100) seq_along(modes) else unique(round(seq(1,length(modes),length.out=41)))
 vv<-vapply(modes[idx],getbest,0.0)
 if(length(modes)>100) {top<-idx[order(vv,decreasing=TRUE)[seq_len(min(3,length(idx)))]];more<-unique(unlist(lapply(top,function(i) seq(max(1,i-30),min(length(modes),i+30)))));more<-setdiff(more,idx);if(length(more))vv<-c(vv,vapply(modes[more],getbest,0.0))}
 best<-max(vv);if(best < -1e40) list(ll=NA_real_,why='infeasible') else list(ll=best,why='finite')
}
oracle <- function(id,x,h) {
 if(id=='triang')return(tri(x,h))
 if(id=='unif') {m<-min(x);M<-max(x);p<-c(min=m,max=M);p[names(h)]<-h
  if(p[1]>m||p[2]<M)return(list(ll=NA_real_,why='infeasible'))
  if(p[1]==p[2])return(list(ll=Inf,why='collapse'))
  # Boundary optimum is calculated exactly because exponential coordinates
  # only approach a free endpoint equal to an observation.
  return(list(ll=ll(id,x,p),why='finite'))}
 if(id %in% c('beta','kumar') && !length(h) && length(unique(x))==1) return(list(ll=Inf,why='collapse'))
 if(id=='logitnorm' && !('sigma'%in%names(h)) && length(unique(x))==1 && (!('mu'%in%names(h))||abs(qlogis(x[1])-h['mu'])<1e-12)) return(list(ll=Inf,why='collapse'))
 nm<-names(params[[id]]);free<-setdiff(nm,names(h))
 trans<-function(z){p<-params[[id]];p[names(h)]<-h;for(i in seq_along(free))p[free[i]]<-if(id=='logitnorm'&&free[i]=='mu')z[i] else exp(z[i]);p}
 fun<-function(z){v<-ll(id,x,trans(z));if(is.finite(v)) -v else 1e50}
 best<-Inf
 for(s in c(-2,-.5,.5,2)) {z<-rep(s,length(free));if(id=='logitnorm'&&'mu'%in%free) z[match('mu',free)]<-mean(qlogis(x))+s
  o<-tryCatch(optim(z,fun,method='BFGS',control=list(maxit=1000,reltol=1e-13)),error=function(e)NULL);if(!is.null(o))best<-min(best,o$value)}
 list(ll=-best,why=if(best>=1e49)'search-failed' else 'finite')
}
run <- function(id,label,x,h) {
 o<-tryCatch(oracle(id,x,h),error=function(e)list(ll=NA_real_,why=paste('oracle-error',conditionMessage(e))))
 fit<-tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),as.list(h)))),error=function(e)e)
 if(inherits(fit,'error')) {fll<-NA_real_;fixed<-'';msg<-conditionMessage(fit)} else {fll<-as.numeric(logLik(fit));fixed<-paste(sort(attr(fit,'fixed')),collapse='+');msg<-''}
 data.frame(id=id,sample=label,n=length(x),held=paste(sort(names(h)),collapse='+'),hold_values=paste(format(h,digits=9),collapse='+'),oracle=o$ll,oracle_status=o$why,fit=fll,fixed=fixed,error=msg,delta=if(is.finite(o$ll)&&is.finite(fll)&&fixed==paste(sort(names(h)),collapse='+')) fll-o$ll else NA_real_,stringsAsFactors=FALSE)
}
if (sys.nframe() == 0L) {
rows<-list(); k<-0
for(id in ids) {
 samples<-list();for(n in c(2,3,5,20,100,1000))samples[[paste0('default',n)]]<-simulate_default(paste0('ml',id),n)
 samples$tied<-rep(samples$default5,each=2);samples$constant<-rep(samples$default5[1],5)
 if(id%in%c('beta','kumar','logitnorm')){samples$near0<-rep(.0001,5);samples$near1<-rep(.9999,5);samples$ushape<-c(.001,.003,.01,.99,.997,.999);samples$uniform_limit<-runif(100);samples$point_limit<-rep(.4,5)}
 if(id%in%c('unif','triang'))samples$limit_uniform<-runif(100)
 for(label in names(samples)) for(mask in 0:(2^length(params[[id]])-2)) {
  h<-params[[id]][as.logical(intToBits(mask)[seq_along(params[[id]])])];k<-k+1;rows[[k]]<-run(id,label,samples[[label]],h)
 }
}
# Directly probe the package's own logit-normal dpqr against README formula.
if('logitnorm'%in%ids){x<-plogis(seq(-12,12,length.out=101)); own<-dlogitnorm(x,mu=2,sigma=3,log=TRUE);formula<- -log(3)-log(2*pi)/2-log(x)-log1p(-x)-(qlogis(x)-2)^2/18;writeLines(sprintf('max_abs_log_density_error=%.17g',max(abs(own-formula))),file.path(outdir,'logitnorm-density.txt'))}
result<-do.call(rbind,rows);write.csv(result,file.path(outdir,paste0('results-',group,'.csv')),row.names=FALSE,na='NA');cat('wrote',nrow(result),'rows\n');print(aggregate(delta~id,result,function(z)max(abs(z))))

}
