# Reference log densities at parameter edges on a nonconstant interior sample.
library(univariateML,lib.loc='/tmp/umvl-p0lib')
outdir<-'notes/evaluations/2026-10-29-p0-bounded';x<-c(.08,.2,.4,.75,.92);k<-c(2,4,8,12)
rows<-list();i<-0;add<-function(family,held,path,t,ll){i<<-i+1;rows[[i]]<<-data.frame(family,held,path,t,logLik=ll)}
for(t in k){lo<-exp(-t);hi<-exp(t)
 for(s in c(lo,hi)){
 add('beta','shape1','shape2 -> 0 / Inf',s,sum(dbeta(x,2,s,log=TRUE)))
 add('beta','shape2','shape1 -> 0 / Inf',s,sum(dbeta(x,s,3,log=TRUE)))
 add('kumar','a','b -> 0 / Inf',s,sum(extraDistr::dkumar(x,2,s,log=TRUE)))
 add('kumar','b','a -> 0 / Inf',s,sum(extraDistr::dkumar(x,s,3,log=TRUE)))
 add('logitnorm','mu','sigma -> 0 / Inf',s,sum(dlogitnorm(x,0,s,log=TRUE)))
 }
 add('beta','','both shapes -> 0',lo,sum(dbeta(x,lo,lo,log=TRUE)))
 add('beta','','both shapes -> Inf',hi,sum(dbeta(x,hi,hi,log=TRUE)))
 add('kumar','','a -> 0, b = 1/t',lo,sum(extraDistr::dkumar(x,lo,1/t,log=TRUE)))
 add('logitnorm','sigma','mu -> +Inf',t,sum(dlogitnorm(x,t,3,log=TRUE)))
 add('logitnorm','sigma','mu -> -Inf',-t,sum(dlogitnorm(x,-t,3,log=TRUE)))
}
write.csv(do.call(rbind,rows),file.path(outdir,'boundaries.csv'),row.names=FALSE)
# Exact held-mu matching gives an unbounded likelihood; a different held mu
# yields a finite maximum, even for a constant sample.
y<-rep(.4,5);mu<-qlogis(.4)
cat('held mu matching sample logit, sigma -> 0:\n');for(s in c(.1,.01,.001))cat(s,sum(dlogitnorm(y,mu,s,log=TRUE)),'\n')
cat('held mu = 0, same sample: sigma optimum',abs(mu),'logLik',sum(dlogitnorm(y,0,abs(mu),log=TRUE)),'\n')
