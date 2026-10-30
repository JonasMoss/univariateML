# Independent transformed-coordinate multistart likelihood probes; run from repo root.
library(univariateML, lib.loc='/tmp/umvl-p0lib')
source('dev/research/evaluations/2026-10-29-p0-skewreal/density.R')
set.seed(20261029)
spec <- list(sn=c(xi=0,omega=1,alpha=3), snorm=c(mean=0,sd=1,xi=3),
 sstd=c(mean=0,sd=1,nu=3,xi=2), sged=c(mean=0,sd=1,nu=3,xi=3),
 emg=c(mu=0,sigma=1,lambda=1))
positive <- list(sn='omega',snorm=c('sd','xi'),sstd=c('sd','xi'),sged=c('sd','xi'),emg=c('sigma','lambda'))
encode <- function(p,id) { p[positive[[id]]] <- log(p[positive[[id]]]); if(id=='sstd') p['nu'] <- log(p['nu']-2); if(id=='sged') p['nu'] <- log(max(p['nu']-1,1e-10)); p }
decode <- function(p,id) { p[positive[[id]]] <- exp(p[positive[[id]]]); if(id=='sstd') p['nu'] <- 2+exp(p['nu']); if(id=='sged') p['nu'] <- 1+exp(p['nu']); p }
rows <- list()
for(id in names(spec)) {
  x20 <- simulate_default(paste0('ml',id),20)
  samples <- list(n20=x20,n100=simulate_default(paste0('ml',id),100),n1000=simulate_default(paste0('ml',id),1000),n2=x20[1:2],n3=x20[1:3],n5=x20[1:5],ties=round(x20,0),normal=rnorm(20),exponential=rexp(20), reflected_exp=-rexp(20), halfnormal=abs(rnorm(20)), lefthalfnormal=-abs(rnorm(20)), halft=abs(rt(20,4)), lefthalft=-abs(rt(20,4)), halfged=abs(rsged(20,nu=3,xi=1)), lefthalfged=-abs(rsged(20,nu=3,xi=1)), uniform=runif(20,-1,1), t2=rt(20,2), skewt2=ifelse(runif(20)<.8,2*abs(rt(20,2)),-abs(rt(20,2))/2), skewnormal=rsnorm(20,xi=2))
  params <- spec[[id]]; ps <- names(params); dens <- get(paste0('d',id)); fitter <- get(paste0('ml',id))
  for(case in names(samples)) for(mask in 0:(2^length(ps)-2)) {
    x <- samples[[case]]; held <- ps[as.logical(intToBits(mask)[seq_along(ps)])]; free <- setdiff(ps,held); init <- encode(params,id)
    fun <- function(z) { q <- init; q[free] <- z; if(any(!is.finite(q)) || any(abs(q)>25)) return(1e100); p <- decode(q,id); if(id=='emg' && (p['lambda']*p['sigma']>1e4 || max(abs((x-p['mu'])/p['sigma']))>1e4)) return(1e100); v <- tryCatch(logref(id,x,p),error=function(e) rep(-Inf,length(x))); if(any(!is.finite(v))) return(1e100); -sum(v) }
    pkg <- tryCatch(do.call(fitter,c(list(x=x),as.list(params[held]))),error=function(e)e)
    starts <- list(init[free], init[free]+0.4,init[free]-0.4)
    if(!inherits(pkg,'error')) { fitted <- tryCatch(coef(pkg),error=function(e) NULL); if(!is.null(fitted) && all(ps %in% names(fitted)) && all(is.finite(fitted[ps]))) starts <- c(starts,list(encode(fitted[ps],id)[free])) }
    fits <- lapply(starts,function(z) tryCatch(optim(z,fun,method='BFGS',control=list(maxit=300,reltol=1e-10)),error=function(e) NULL))
    vals <- vapply(fits,function(o) if(is.null(o)) Inf else o$value,0.0); best <- min(vals)
    msg <- if(inherits(pkg,'error')) conditionMessage(pkg) else 'returned'
    ll <- if(inherits(pkg,'error')) NA_real_ else as.numeric(logLik(pkg))
    rows[[length(rows)+1]] <- data.frame(id,case,n=length(x),held=paste(held,collapse='+'),oracle=-best,package=ll,difference=ll+best,status=msg)
  }
}
out <- do.call(rbind,rows); write.csv(out,'dev/research/evaluations/2026-10-29-p0-skewreal/oracle-results.csv',row.names=FALSE)
print(table(out$id,out$status=='returned')); print(subset(out,is.finite(difference) & abs(difference)>1e-6*n))
