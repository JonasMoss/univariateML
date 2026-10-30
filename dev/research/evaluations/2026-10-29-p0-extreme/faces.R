# Held-position and boundary comparisons on the literal reference density.
# /tmp/umvl-codex/simbox-run.sh Rscript dev/research/evaluations/2026-10-29-p0-extreme/faces.R
lines<-readLines('dev/research/evaluations/2026-10-29-p0-extreme/oracle.R')
eval(parse(text=lines[seq_len(grep('^# sample types',lines)[1]-1)]))
set.seed(20261101); z<-list()
for(n in c(2,3,5,20,100)) for(id in c('gev','gpd','frechet')) for(truth in c('default','boundary')) {
 x<-if(truth=='default') simulate_default(paste0('ml',id),n) else switch(id,gev=rgev(n,0,1,-1),gpd=runif(n),frechet=rgumbel(n))
 m<-min(x); M<-max(x); r<-max(diff(range(x)),.01)
 for(h in if(id=='gev') list(list(mu=m-r),list(mu=m),list(mu=m+r/2),list(mu=M+r),list(sigma=r/2),list(sigma=r),list(sigma=2*r)) else if(id=='gpd') list(list(sigma=r/2),list(sigma=r),list(sigma=2*r),list(mu=m-r),list(mu=m-r,sigma=r)) else list(list(sigma=r/2),list(sigma=r),list(sigma=2*r),list(mu=m-r))) {
   o<-fit_oracle(id,x,h,starts=12,bounds='new')
   face<-if(id=='gev' && 'sigma'%in%names(h)) -n*log(h$sigma)-sum((M-x)/h$sigma) else if(id=='gev' && 'mu'%in%names(h) && M>h$mu) {s<-M-h$mu;-n*log(s)-sum((M-x)/s)} else if(id=='gpd' && 'sigma'%in%names(h)) -n*log(h$sigma) else if(id=='gpd' && 'mu'%in%names(h)) -n*log(M-h$mu) else NA_real_
   z[[length(z)+1]]<-data.frame(id=id,n=n,truth=truth,held=paste(paste(names(h),unlist(h),sep='='),collapse=','),oracle=if(is.null(o)) NA_real_ else o$ll,face=face,delta=if(is.null(o)) NA_real_ else o$ll-face)
 }
}
d<-do.call(rbind,z);write.csv(d,file.path(out,'faces.csv'),row.names=FALSE)
cat('rows',nrow(d),'finite oracles',sum(is.finite(d$oracle)),'\n')
