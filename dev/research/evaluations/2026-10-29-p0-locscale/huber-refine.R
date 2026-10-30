# Independent profile refinement for Huber near-Laplace disagreements.
# Rscript .../huber-refine.R laplace 1000   (or cauchy 1000)
library(univariateML,lib.loc='/tmp/umvl-p0lib')
a<-commandArgs(TRUE);type<-a[1];n<-as.integer(a[2]);types<-c('normal','laplace','t3','cauchy')
d<-read.csv(sprintf('dev/research/evaluations/2026-10-29-p0-locscale/huber-%s-%d.csv',type,n)); target<-which(d$package!=d$oracle)
set.seed(20261029+10000*match(type,types)+n);random<-switch(type,normal=function(n)rnorm(n),laplace=function(n)extraDistr::rlaplace(n),t3=function(n)rt(n,3),cauchy=function(n)rcauchy(n));samples<-lapply(seq_len(100),function(i)random(n)); out<-list()
for(i in target) {
 x<-samples[[i]]; m<-median(x);u<-median(abs(x-m)); y<-(x-m)/u; l<-mean(abs(y-median(y))); starts<-c(median(y),log(l))
 profile<-function(t) {e<-exp(t); objective<-function(p){z<-suppressWarnings(extraDistr::dhuber(y,mu=p[1],sigma=exp(p[2])*e,epsilon=e,log=TRUE));if(any(!is.finite(z))) 1e100 else -sum(z)}; a<-optim(starts,objective,method='BFGS',control=list(maxit=700,reltol=1e-13)); -a$value}
 grid<-seq(log(.001),log(2),length.out=80);v<-vapply(grid,profile,0);cand<-which(v>=c(-Inf,head(v,-1)) & v>=c(tail(v,-1),-Inf)); opt<-lapply(cand,function(j)optimize(profile,interval=grid[c(max(1,j-1),min(length(grid),j+1))],maximum=TRUE,tol=1e-9));
 best<-max(c(v,vapply(opt,`[[`,0,'objective'))); p<-mlhuber(x,epsilon=NULL);pll<-as.numeric(logLik(p))+n*log(u)
 out[[length(out)+1]]<-data.frame(rep=i,profile=best,face=max(d$normal[i],d$laplace[i]),package_ll=pll,difference=best-pll,profile_above_face=best-max(d$normal[i],d$laplace[i]))
}
if(length(out)){out<-do.call(rbind,out);write.csv(out,sprintf('dev/research/evaluations/2026-10-29-p0-locscale/huber-refine-%s-%d.csv',type,n),row.names=FALSE);print(out)}
