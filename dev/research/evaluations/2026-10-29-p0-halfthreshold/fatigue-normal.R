# Bounded-alpha fatigue search against the independently fitted normal face.
library(extraDistr)
set.seed(20261029)
outdir='dev/research/evaluations/2026-10-29-p0-halfthreshold'
samples=list(left=c(-3,-1,0,.2,.3),symmetric=c(-2,-1,0,1,2),normal=rnorm(40))
results=list(); j=0
for(nm in names(samples)) for(A in c(.1,.5,1)) {
 x=samples[[nm]]; normal=sum(dnorm(x,mean(x),sqrt(mean((x-mean(x))^2)),log=TRUE))
 obj=function(v) {mu=min(x)-exp(v[1]); beta=exp(v[2]); alpha=exp(v[3]); -sum(dfatigue(x,alpha,beta,mu,log=TRUE))}
 fits=lapply(c(1,10,100,1000,10000),function(M) {start=c(log(M),log(M+mean(x)-min(x)),log(min(A/2,sd(x)/M))); tryCatch(nlminb(start,obj,lower=c(-15,-15,-18),upper=c(15,15,log(A)),control=list(iter.max=500,eval.max=1000,rel.tol=1e-11)),error=function(e) NULL)})
 fits=Filter(Negate(is.null),fits); f=fits[[which.min(vapply(fits,`[[`,0.,'objective'))]]; j=j+1;results[[j]]=data.frame(sample=nm,upper=A,normal_ll=normal,finite_ll=-f$objective,mu=min(x)-exp(f$par[1]),beta=exp(f$par[2]),alpha=exp(f$par[3]),convergence=f$convergence)
}
write.csv(do.call(rbind,results),file.path(outdir,'fatigue-normal-results.csv'),row.names=FALSE)
