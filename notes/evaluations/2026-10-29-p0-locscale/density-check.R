# Check owned densities against formulas in README; Rscript .../density-check.R
library(univariateML,lib.loc='/tmp/umvl-p0lib'); set.seed(20261029)
x<-c(seq(-5,5,length.out=101),rnorm(100)); rows<-list()
for(nu in c(2.01,3,10,100)) {v<-dt(x*sqrt(nu/(nu-2)),nu,log=TRUE)+.5*log(nu/(nu-2));rows[[length(rows)+1]]<-data.frame(id='std',parameter=nu,maxerror=max(abs(v-dstd(x,nu=nu,log=TRUE))))}
for(nu in c(.5,1,2,3,10)) {b<-exp(.5*(-2*log(2)/nu+lgamma(1/nu)-lgamma(3/nu))); v<- -abs(x/b)^nu/2+log(nu)-1.5*lgamma(1/nu)+.5*lgamma(3/nu)-log(2);rows[[length(rows)+1]]<-data.frame(id='ged',parameter=nu,maxerror=max(abs(v-dged(x,nu=nu,log=TRUE))))}
for(lambda in c(-1,-.5,0,.14,.5,.99,1)) {u<-seq(.01,.99,length.out=99);z<-if(lambda==0)qlogis(u) else (u^lambda-(1-u)^lambda)/lambda;v<- -log(u^(lambda-1)+(1-u)^(lambda-1));rows[[length(rows)+1]]<-data.frame(id='tlambda',parameter=lambda,maxerror=max(abs(v-dtlambda(z,lambda=lambda,log=TRUE))))}
out<-do.call(rbind,rows);print(out);write.csv(out,'notes/evaluations/2026-10-29-p0-locscale/density-check.csv',row.names=FALSE)
