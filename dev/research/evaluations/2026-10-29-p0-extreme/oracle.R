# Run from repository root: /tmp/umvl-cpu/run Rscript dev/research/evaluations/2026-10-29-p0-extreme/oracle.R
# Bounded, multistart numerical exploration, not a proof of global optimality.
library(univariateML, lib.loc='/tmp/umvl-p0lib')
library(extraDistr)
set.seed(20261029)
out <- 'dev/research/evaluations/2026-10-29-p0-extreme'; dir.create(out, recursive=TRUE, showWarnings=FALSE)
ids <- c('gev','gpd','frechet')
pars <- list(gev=c('mu','sigma','xi'),gpd=c('mu','sigma','xi'),frechet=c('lambda','mu','sigma'))
dens <- list(gev=dgev,gpd=dgpd,frechet=dfrechet)
# Free mu is parameterized below the minimum for threshold families.
fit_oracle <- function(id,x,held=list(),starts=12, bounds='master') {
 p <- pars[[id]]; free <- setdiff(p,names(held)); m <- min(x); r <- max(diff(range(x)),sd(x),.1)
 encode <- function(n,v) switch(n, sigma=log(v/r), lambda=log(v),
   mu=if(id=='gev') (v-m)/r else log((m-v)/r), xi=v)
 decode <- function(n,v) switch(n,sigma=r*exp(v),lambda=exp(v),
   mu=if(id=='gev') m+r*v else m-r*exp(v),xi=v)
 base <- if(id=='gev') list(mu=median(x),sigma=r,xi=.2) else if(id=='gpd') list(mu=m-r,sigma=r,xi=.2) else list(lambda=2,mu=m-r,sigma=r)
 base[names(held)] <- held
 eval <- function(z, give=FALSE) {
   a <- base; for(j in seq_along(free)) a[[free[j]]] <- decode(free[j],z[j])
   if(any(!is.finite(unlist(a)))) return(if(give) NULL else 1e90)
   lower <- if(bounds=='master') -.5 else -1
   if(id %in% c('gev','gpd') && (a$xi<lower || (id=='gev' && a$xi>1) || (id=='gpd' && !('mu' %in% names(held)) && a$xi>1))) return(if(give) NULL else 1e90)
   if(id=='frechet' && !('mu' %in% names(held)) && a$lambda<1) return(if(give) NULL else 1e90)
   v <- suppressWarnings(tryCatch(do.call(dens[[id]],c(list(x=x,log=TRUE),a)),error=function(e) NA_real_))
   if(length(v)!=length(x) || any(!is.finite(v))) return(if(give) NULL else 1e90)
   if(give) list(par=a,ll=sum(v)) else -sum(v)
 }
 z0 <- vapply(free,function(n) encode(n,base[[n]]),numeric(1)); best <- NULL
 for(i in seq_len(starts)) {
   z <- z0 + if(i==1) 0 else rnorm(length(z0),0,.7)
   o <- tryCatch(optim(z,eval,method='Nelder-Mead',control=list(maxit=2500,reltol=1e-11)),error=function(e) NULL)
   if(!is.null(o) && is.finite(o$value) && o$value<1e80 && (is.null(best)||o$value<best$value)) best <- o
 }
 if(is.null(best)) return(NULL)
 c(eval(best$par,TRUE),list(convergence=best$convergence))
}
# sample types, including the Gumbel and exponential limit models
samples <- list()
for(id in ids) {
 for(n in c(20,100,1000)) samples[[paste(id,'default',n,sep='_')]] <- simulate_default(paste0('ml',id),n)
 for(n in c(2,3,5)) samples[[paste(id,'small',n,sep='_')]] <- simulate_default(paste0('ml',id),n)
 samples[[paste0(id,'_ties')]] <- if(id=='gev') c(-1,0,0,1,2,3,4) else c(.2,.5,.5,1,2,3,4)
}
samples$gev_gumbel <- rgumbel(100,0,1); samples$gpd_exp <- rexp(100); samples$frechet_gumbel <- rgumbel(100,0,1)
samples$gev_reverse_exp <- rgev(100,0,1,-1); samples$gpd_uniform <- runif(100); samples$frechet_inverse_weibull <- rfrechet(100,2,0,1)
for (id in ids) {
 samples[[paste0(id,'_min_half')]] <- c(1,1,2,3)
 samples[[paste0(id,'_min_majority')]] <- c(1,1,1,2,3)
 samples[[paste0(id,'_max_half')]] <- c(1,2,3,3)
 samples[[paste0(id,'_constant')]] <- rep(1,5)
}
rows <- list()
for(nm in names(samples)) {
 id <- strsplit(nm,'_')[[1]][1]; x <- samples[[nm]]; p <- pars[[id]]
 for(mask in 0:6) {
  h <- p[as.logical(intToBits(mask)[1:3])]; if(length(h)==3) next
  values <- if(id=='gev') list(mu=median(x),sigma=max(sd(x),.1),xi=.2) else if(id=='gpd') list(mu=if(min(x)>0) 0 else min(x)-max(sd(x),.1),sigma=max(sd(x),.1),xi=.2) else list(mu=if(min(x)>0) 0 else min(x)-max(sd(x),.1),sigma=max(sd(x),.1),lambda=2)
  held <- values[h]; oracle <- fit_oracle(id,x,held,starts=if(length(x)>500) 4 else 8)
  # master supports GEV: {} and {xi}; GPD: {mu} and {mu,xi}; Fréchet: {} and {mu}.
  implemented <- if(id=='gev') setequal(h,character()) || setequal(h,'xi') else if(id=='gpd') setequal(h,'mu') || setequal(h,c('mu','xi')) else setequal(h,character()) || setequal(h,'mu')
  fit <- if(implemented) tryCatch(do.call(get(paste0('ml',id)),c(list(x=x),held)),error=function(e) e) else NULL
  pll <- if(!is.null(fit)&&!inherits(fit,'error')) as.numeric(logLik(fit)) else NA_real_
  msg <- if(inherits(fit,'error')) conditionMessage(fit) else ''
  rows[[length(rows)+1]] <- data.frame(sample=nm,n=length(x),held=paste(h,collapse=','),oracle=if(is.null(oracle)) NA else oracle$ll,package=pll,diff=if(is.null(oracle)) NA else pll-oracle$ll,package_error=msg)
 }
}
write.csv(do.call(rbind,rows),file.path(out,'oracle-results.csv'),row.names=FALSE)
cat('Wrote',file.path(out,'oracle-results.csv'),'\n')
