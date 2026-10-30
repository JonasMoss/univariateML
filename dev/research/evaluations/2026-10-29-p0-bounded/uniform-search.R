# A numerical search against dunif, in addition to the exact order-statistic oracle.
library(univariateML,lib.loc='/tmp/umvl-p0lib');set.seed(791029)
outdir<-'dev/research/evaluations/2026-10-29-p0-bounded';rows<-list();i<-0
for(n in c(2,3,5,20,100,1000)){x<-simulate_default('mlunif',n);m<-min(x);M<-max(x);for(h in list(numeric(),c(min=-1),c(max=2))){
 free<-setdiff(c('min','max'),names(h));p<-c(min=m,max=M);p[names(h)]<-h;exact<-sum(dunif(x,p[1],p[2],log=TRUE))
 objective<-function(z){v<-p;for(j in seq_along(free))v[free[j]]<-if(free[j]=='min')m-exp(z[j]) else M+exp(z[j]);-sum(dunif(x,v[1],v[2],log=TRUE))}
 opt<-min(vapply(c(-3,0,2),function(start)optim(rep(start,length(free)),objective,method='L-BFGS-B',lower=rep(-25,length(free)),upper=rep(5,length(free)),control=list(maxit=500,factr=1e4))$value,0.0))
 i<-i+1;rows[[i]]<-data.frame(n=n,held=paste(names(h),collapse='+'),exact=exact,numerical=-opt,gap=exact+opt)
}}
z<-do.call(rbind,rows);write.csv(z,file.path(outdir,'uniform-search.csv'),row.names=FALSE);cat('Max exact minus numerical:',max(z$gap),'\n')
