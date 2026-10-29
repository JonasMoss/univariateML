# Reference-density multistart comparison, including master fixed-shape faces.
# /tmp/umvl-codex/simbox-run.sh Rscript notes/evaluations/2026-10-29-p0-extreme/master-check.R
lines<-readLines('notes/evaluations/2026-10-29-p0-extreme/oracle.R')
eval(parse(text=lines[seq_len(grep('^# sample types',lines)[1]-1)]))
set.seed(20261031); rows<-list()
for(id in ids) for(n in c(2,3,5,20,100,1000)) for(kind in c('default','limit')) {
 x<-if(kind=='default') simulate_default(paste0('ml',id),n) else switch(id,gev=rgumbel(n),gpd=rexp(n),frechet=rgumbel(n))
 m<-min(x); held0<-if(id=='gpd') list(mu=0) else if(id=='frechet') list(mu=if(m>0) 0 else m-max(sd(x),.1)) else list()
 holds<-if(id=='gev') c(list(list()),lapply(c(-.5,-.3,0,.5,1),function(z) list(xi=z))) else if(id=='gpd') c(list(held0),lapply(c(-.5,-.3,0,.5,1),function(z) c(held0,list(xi=z)))) else list(list(),held0)
 for(h in holds) {
  o<-fit_oracle(id,x,h,starts=if(n==1000) 4 else 10)
  p<-tryCatch(do.call(get(paste0('ml',id)),c(list(x=x),h)),error=function(e)e)
  ll<-if(inherits(p,'error')) NA_real_ else as.numeric(logLik(p))
  rows[[length(rows)+1]]<-data.frame(id=id,n=n,kind=kind,held=paste(paste(names(h),unlist(h),sep='='),collapse=','),oracle=if(is.null(o)) NA_real_ else o$ll,master=ll,diff=if(is.null(o)) NA_real_ else ll-o$ll,error=if(inherits(p,'error')) conditionMessage(p) else '')
 }
}
d<-do.call(rbind,rows);write.csv(d,file.path(out,'master-check.csv'),row.names=FALSE)
cat('comparisons',nrow(d),'successful',sum(!is.na(d$master)),'mismatch',sum(!is.na(d$diff)&abs(d$diff)>d$n*1e-6),'\n')
