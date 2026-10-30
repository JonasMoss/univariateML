# Analytic alpha profile, beta fixed, and endpoint equality comparisons.
library(extraDistr)
outdir='dev/research/evaluations/2026-10-29-p0-halfthreshold'
profile=function(x,b,delta) {t=x-min(x)+delta; alpha=sqrt(mean(t/b+b/t-2)); sum(dfatigue(x,alpha,b,min(x)-delta,log=TRUE))}
edge=function(x,b) {n=length(x);k=sum(x==min(x));stopifnot(3*k==n);A=k*b/n;t=x[x>min(x)]-min(x); k*(.5*log(b)-.5*log(A)-log(2)-.5*log(2*pi)-b/(2*A))+sum(log(t+b)-1.5*log(t)-.5*log(b)-.5*log(A)-log(2)-.5*log(2*pi))}
examples=list(interior=c(0,1,1),edge_candidate=c(0,.1,1),large_k=c(0,0,1,1,1,2),small_k=c(0,1,2,3,4))
ans=list(); i=0
for(nm in names(examples)) for(b in c(.1,1,10)) {x=examples[[nm]]; g=seq(-18,12,length.out=1000); vals=vapply(g,function(u) profile(x,b,exp(u)),0.); inds=which(diff(sign(diff(vals)))<0)+1L; local=sapply(inds,function(j) optimize(function(u)-profile(x,b,exp(u)),range(g[(j-1):(j+1)]))$objective); best=if(length(local)) -min(local) else NA_real_; i=i+1; ans[[i]]=data.frame(sample=nm,beta=b,k=sum(x==min(x)),n=length(x),edge=if(3*sum(x==min(x))==length(x)) edge(x,b) else NA_real_,best_local=best,edge_probe=vals[1])}
write.csv(do.call(rbind,ans),file.path(outdir,'fatigue-profile-results.csv'),row.names=FALSE)
cat('fatigue profile examples written\n')
