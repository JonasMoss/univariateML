# Independent interior multistart reference-density oracle; run from repo root.
suppressPackageStartupMessages(library(univariateML, lib.loc='/tmp/umvl-p0lib'))
ids <- c('pois','geom','nbinom','zip','zinb','zmpois','ztpois','zmgeom','ztgeom','zmnbinom','ztnbinom','zmlogarithmic','lgser','pig')
pars <- list(pois=c(lambda=2), geom=c(prob=.5), nbinom=c(size=10,prob=.3), zip=c(lambda=1,pi=.5), zinb=c(size=2,prob=.4,pi=.3), zmpois=c(lambda=2,p0=.3), ztpois=c(lambda=2), zmgeom=c(prob=.4,p0=.3), ztgeom=c(prob=.4), zmnbinom=c(size=2,prob=.4,p0=.3), ztnbinom=c(size=2,prob=.4), zmlogarithmic=c(prob=.4,p0=.3), lgser=c(theta=.9), pig=c(mean=2,shape=3))
dens <- list(pois=stats::dpois,geom=stats::dgeom,nbinom=stats::dnbinom,zip=extraDistr::dzip,zinb=extraDistr::dzinb,zmpois=actuar::dzmpois,ztpois=actuar::dztpois,zmgeom=actuar::dzmgeom,ztgeom=actuar::dztgeom,zmnbinom=actuar::dzmnbinom,ztnbinom=actuar::dztnbinom,zmlogarithmic=actuar::dzmlogarithmic,lgser=extraDistr::dlgser,pig=actuar::dpoisinvgauss)
set.seed(791029)
out <- list(); j <- 0L
for (id in ids) {
  base <- pars[[id]]; nm <- names(base); positive <- id %in% c('ztpois','ztgeom','ztnbinom','lgser')
  smp <- lapply(c(20,100,1000), function(n) simulate_default(paste0('ml',id),n))
  names(smp) <- c('default20','default100','default1000')
  smp <- c(smp,list(ties=if(positive) c(1,1,1,2,2,4) else c(0,0,0,1,1,4), small2=if(positive)c(1,2) else c(0,2), small3=if(positive)c(1,1,4) else c(0,0,4), small5=if(positive)c(1,2,2,3,5) else c(0,1,1,2,4), limit=if(positive) pmax(1,rpois(100,2)) else rpois(100,2)))
  for (case in names(smp)) for (mask in 0:(2^length(nm)-2)) {
    x <- smp[[case]]; held <- nm[as.logical(intToBits(mask)[seq_along(nm)])]; free <- setdiff(nm,held)
    probability <- free %in% c('prob','p0','pi','theta')
    unpack <- function(z) { q <- base; q[free] <- ifelse(probability,plogis(z),exp(z)); q }
    ll <- function(q) { v <- suppressWarnings(tryCatch(do.call(dens[[id]],c(list(x=x),as.list(q),list(log=TRUE))),error=function(e) rep(NA_real_,length(x)))); if(length(v)!=length(x)||any(!is.finite(v))) return(-1e100); sum(v) }
    z0 <- ifelse(probability,qlogis(base[free]),log(base[free])); best <- -1e100
    for (shift in c(-2,0,2)) { opt <- tryCatch(optim(z0+shift,function(z)-ll(unpack(z)),method='BFGS',control=list(maxit=300,reltol=1e-10)), error=function(e) NULL); if(!is.null(opt)) best <- max(best,-opt$value) }
    fit <- tryCatch(suppressWarnings(do.call(get(paste0('ml',id)),c(list(x=x),as.list(base[held])))),error=function(e)e)
    fitll <- if(inherits(fit,'error')) NA_real_ else as.numeric(logLik(fit)); msg <- if(inherits(fit,'error')) conditionMessage(fit) else ''
    j <- j+1L; out[[j]] <- data.frame(id,case,n=length(x),held=paste(held,collapse='+'),oracle=best,fit=fitll,diff=fitll-best,error=msg)
  }
  cat(id,'done\n')
}
write.csv(do.call(rbind,out),'dev/research/evaluations/2026-10-29-p0-counts/first-oracle.csv',row.names=FALSE)
