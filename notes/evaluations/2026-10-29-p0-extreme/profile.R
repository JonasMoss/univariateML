# Run from repo root via /tmp/umvl-codex/simbox-run.sh Rscript .../profile.R
library(extraDistr)
set.seed(20261030)
out <- 'notes/evaluations/2026-10-29-p0-extreme'
profiles <- list(); counters <- 0L
for (id in c('gev','gpd')) for (truth in c(-1,-.85,-.6,-.5,0)) for (n in c(20L,100L,1000L)) {
  x <- if(id=='gev') rgev(n,mu=0,sigma=1,xi=truth) else rgpd(n,mu=0,sigma=1,xi=truth)
  M<-max(x); R<-diff(range(x)); grid<-sort(unique(c(seq(-1,-.5,by=.025), -1+10^-(2:8))))
  if(id=='gev') {
    # At -1, endpoint only approached; profile over sigma analytically.
    s<-mean(M-x); ll1<- -n*log(s)-n
    run <- function(xi) {
      f<-function(z) {s<-exp(z[2]); b<-M+exp(z[1]); v<-dgev(x,mu=b+s/xi,sigma=s,xi=xi,log=TRUE); if(any(!is.finite(v))) 1e100 else -sum(v)}
      fits<-lapply(c(-5,-2,0,2),function(a) optim(c(log(R)+a,log(R)),f,method='Nelder-Mead',control=list(maxit=3000,reltol=1e-11)))
      -min(vapply(fits,`[[`,0.0,'value'))
    }
  } else {
    ll1<- -n*log(M) # strict endpoint limit
    run <- function(xi) {
      f<-function(u) {s<- -xi*M+exp(u); v<-dgpd(x,mu=0,sigma=s,xi=xi,log=TRUE); if(any(!is.finite(v))) 1e100 else -sum(v)}
      z<-optimize(f,c(log(M)-32,log(M)+6),tol=1e-11); -z$objective
    }
  }
  for(xi in grid) {counters<-counters+1L; profiles[[counters]]<-data.frame(id=id,truth=truth,n=n,xi=xi,ll=if(xi == -1) ll1 else run(xi))}
}
d<-do.call(rbind,profiles);write.csv(d,file.path(out,'profiles.csv'),row.names=FALSE)
cat('wrote',nrow(d),'profile points\n')
