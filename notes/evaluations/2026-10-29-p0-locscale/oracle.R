# Independent likelihood/hold sweep. Run: Rscript .../oracle.R ID [ID ...]
# Per family: default samples at 20,100,1000; n=2,3,5; ties; four limits.
# All optimization uses reference density or the formulas below, never a fitter.
library(univariateML,lib.loc='/tmp/umvl-p0lib')
ids <- commandArgs(TRUE); if(!length(ids)) ids <- c('norm','cauchy','logis','laplace','gumbel','lst','std','ged','slash','huber','tlambda','bhatt')
pars <- list(norm=c(mean=0,sd=1),cauchy=c(location=0,scale=1),logis=c(location=0,scale=1),laplace=c(mu=0,sigma=1),gumbel=c(mu=3,sigma=3),lst=c(mu=0,sigma=1,df=3),std=c(mean=0,sd=1,nu=3),ged=c(mean=0,sd=1,nu=3),slash=c(mu=0,sigma=1),huber=c(mu=0,sigma=1,epsilon=1.345),tlambda=c(mu=0,sigma=1,lambda=.14),bhatt=c(mu=0,sigma=1,a=2))
accepted <- list(norm=list(character()),cauchy=list(character()),logis=list(character()),laplace=list(character()),gumbel=list(character()),lst=list(character(),'df'),std=list(character(),'nu'),ged=list(character()),slash=list(character(),'sigma'),huber=list(character(),'mu','sigma','epsilon',c('mu','sigma'),c('mu','epsilon'),c('sigma','epsilon')),tlambda=list(character(),'lambda'),bhatt=list(character(),'sigma','a',c('sigma','a')))
# For each x, invert Tukey's quantile by vectorized bisection, independently
# of the installed package's density and its numerical inversion.
tlambda_formula <- function(x,mu,sigma,lambda) {
 z <- (x-mu)/sigma; if(lambda==1) return(ifelse(abs(z)<=1,-log(2*sigma),-Inf))
 if(lambda>0 && any(abs(z)>=1/lambda)) return(rep(-Inf,length(x)))
 lo <- rep(1e-15,length(x)); hi <- rep(1-1e-15,length(x))
 for(i in 1:52) {u <- (lo+hi)/2; q <- if(abs(lambda)<1e-8) qlogis(u) else (u^lambda-(1-u)^lambda)/lambda; smaller <- q<z; lo[smaller]<-u[smaller]; hi[!smaller]<-u[!smaller]}
 u <- (lo+hi)/2; -log(sigma)-log(u^(lambda-1)+(1-u)^(lambda-1))
}
ll <- function(id,x,p) {z <- switch(id,
 norm=dnorm(x,p[1],p[2],log=TRUE),cauchy=dcauchy(x,p[1],p[2],log=TRUE),logis=dlogis(x,p[1],p[2],log=TRUE),
 laplace=extraDistr::dlaplace(x,p[1],p[2],log=TRUE),gumbel=extraDistr::dgumbel(x,p[1],p[2],log=TRUE),
 lst=extraDistr::dlst(x,mu=p[1],sigma=p[2],df=p[3],log=TRUE),
 std=dt((x-p[1])/p[2]*sqrt(p[3]/(p[3]-2)),df=p[3],log=TRUE)+.5*log(p[3]/(p[3]-2))-log(p[2]),
 ged={v<-p[3]; b<-p[2]*exp(.5*(-2*log(2)/v+lgamma(1/v)-lgamma(3/v))); -log(p[2])-abs((x-p[1])/b)^v/2+log(v)-1.5*lgamma(1/v)+.5*lgamma(3/v)-log(2)},
 slash=extraDistr::dslash(x,mu=p[1],sigma=p[2],log=TRUE),huber=extraDistr::dhuber(x,mu=p[1],sigma=p[2],epsilon=p[3],log=TRUE),
 tlambda=tlambda_formula(x,p[1],p[2],p[3]), bhatt=extraDistr::dbhatt(x,mu=p[1],sigma=p[2],a=p[3],log=TRUE)); if(any(!is.finite(z))) -Inf else sum(z)}
shape_enc <- function(id,z) switch(id,lst=log(max(z-1,1e-5)),std=log(max(z-2,1e-5)),ged=log(max(z-1,1e-5)),tlambda=atanh(max(-.999999,min(.999999,z))),log(z))
shape_dec <- function(id,z) switch(id,lst=1+exp(z),std=2+exp(z),ged=1+exp(z),tlambda=tanh(z),exp(z))
fit <- function(id,x,held) {
 p0 <- pars[[id]]; loc<-median(x); u<-median(abs(x-loc)); if(!is.finite(u)||u<1e-8)u<-max(diff(range(x))/2,1)
 free <- setdiff(names(p0),held); encode <- function(p) {z<-c((p[1]-loc)/u,log(p[2]/u)); if(length(p)==3)z<-c(z,shape_enc(id,p[3]));names(z)<-names(p);z[free]}
 decode <- function(z) {p<-p0; if(names(p)[1] %in% free)p[1]<-loc+u*z[match(names(p)[1],free)];if(names(p)[2] %in% free)p[2]<-u*exp(z[match(names(p)[2],free)]);if(length(p)==3 && names(p)[3]%in%free)p[3]<-shape_dec(id,z[match(names(p)[3],free)]);p}
 objective <- function(z) {if(any(!is.finite(z))||any(abs(z)>40))return(1e50);p<-decode(z);v<-suppressWarnings(ll(id,x,p));if(is.finite(v)) -v else 1e50}
 best <- -Inf; bestp <- rep(NA_real_,length(p0)); names(bestp)<-names(p0)
 for(sh in if(length(p0)==3 && names(p0)[3] %in% free) switch(id,lst=c(1.02,2,8,50),std=c(2.02,3,8,50),ged=c(1.02,2,5,20),tlambda=c(-.9,0,.5,.95),c(.2,1,3,8)) else p0[length(p0)]) {
  p<-p0;p[1]<-loc;p[2]<-if(id=='gumbel') max(u,diff(range(x))/3) else u;if(length(p)==3)p[3]<-sh;p[held]<-p0[held];if(id=='tlambda' && p[3]>0 && names(p)[2] %in% free)p[2]<-max(p[2],1.1*p[3]*max(abs(x-p[1])))
  o<-tryCatch(optim(encode(p),objective,method='Nelder-Mead',control=list(maxit=700,reltol=1e-9)),error=function(e)NULL)
  if(!is.null(o) && is.finite(o$value) && o$value<1e49 && -o$value>best){best <- -o$value;bestp<-decode(o$par)}
 }
 # Shape restriction boundaries are attained members and need separate fitting.
 if(length(p0)==3 && names(p0)[3]%in%free && id %in% c('lst','ged','tlambda')) for(edge in if(id=='tlambda')c(-1,1) else 1) {
  pn<-p0;pn[3]<-edge;shname<-names(p0)[3];fl<-setdiff(free,shname)
  obj<-function(z){p<-pn; if(names(p)[1]%in%fl)p[1]<-loc+u*z[match(names(p)[1],fl)];if(names(p)[2]%in%fl)p[2]<-u*exp(z[match(names(p)[2],fl)]);v<-suppressWarnings(ll(id,x,p));if(is.finite(v))-v else 1e50}
  start<-c((median(x)-loc)/u,0);names(start)<-names(p0)[1:2];o<-tryCatch(optim(start[fl],obj,method='Nelder-Mead',control=list(maxit=500,reltol=1e-9)),error=function(e)NULL)
  if(!is.null(o)&&is.finite(o$value)&&o$value<1e49&& -o$value>best){p<-pn;if(names(p)[1]%in%fl)p[1]<-loc+u*o$par[match(names(p)[1],fl)];if(names(p)[2]%in%fl)p[2]<-u*exp(o$par[match(names(p)[2],fl)]);best<- -o$value;bestp<-p}
 }
 if(id=='tlambda' && names(p0)[3] %in% free && all(names(p0)[1:2] %in% free)) {p<-p0;p[1]<-mean(range(x));p[2]<-diff(range(x))/2*(1+1e-12);p[3]<-1;v<-ll(id,x,p);if(v>best){best<-v;bestp<-p}}
 list(ll=best,p=bestp)
}
for(id in ids) {
 set.seed(20261029+match(id,names(pars))*1000); samp<-list(default20=simulate_default(paste0("ml",id),20),default100=simulate_default(paste0("ml",id),100),default1000=simulate_default(paste0("ml",id),1000),small2=simulate_default(paste0("ml",id),2),small3=simulate_default(paste0("ml",id),3),small5=simulate_default(paste0("ml",id),5),ties=c(rep(0,4),-2,-1,1,2),normal=rnorm(20),laplace=extraDistr::rlaplace(20),cauchy=rcauchy(20),t2=rt(20,2),logistic=rlogis(20),uniform=runif(20,-1,1),outside=seq(2,6,length.out=8))
 samples_file<-paste0('notes/evaluations/2026-10-29-p0-locscale/samples-',id,'.rds');saveRDS(samp,samples_file)
 pn<-names(pars[[id]]); masks<-0:(2^length(pn)-2); rows<-list();j<-0
 for(s in names(samp)) for(mask in masks) {
  held<-pn[as.logical(intToBits(mask)[seq_along(pn)])]; x<-samp[[s]]; oracle<-fit(id,x,held)
  accept<-any(vapply(accepted[[id]],function(z)setequal(z,held),logical(1))); result<-NULL
  if(accept){args<-as.list(pars[[id]][held]); if(id=='huber'&&!('epsilon'%in%held))args['epsilon']<-list(NULL);result<-tryCatch(do.call(get(paste0('ml',id)),c(list(x=x),args)),error=function(e)e)}
  pkg<-if(!accept)'unsupported' else if(inherits(result,'error'))conditionMessage(result) else 'finite'; pl<-if(accept&&!inherits(result,'error'))as.numeric(logLik(result)) else NA_real_
  j<-j+1;rows[[j]]<-data.frame(id,sample=s,n=length(x),held=paste(held,collapse=','),oracle_ll=oracle$ll,oracle_parameters=paste(signif(oracle$p,8),collapse=','),package=pkg,package_ll=pl,delta=oracle$ll-pl)
 }
 out<-do.call(rbind,rows);write.csv(out,paste0('notes/evaluations/2026-10-29-p0-locscale/oracle-',id,'.csv'),row.names=FALSE);cat(id,nrow(out),'cases; worst delta',max(abs(out$delta),na.rm=TRUE),'\n')
}
