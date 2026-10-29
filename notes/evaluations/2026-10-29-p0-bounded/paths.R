sink('notes/evaluations/2026-10-29-p0-bounded/paths-output.txt', split=TRUE)
library(univariateML,lib.loc='/tmp/umvl-p0lib')
x <- rep(.4,5); t <- c(10,50,100,500)
cat('constant interior paths, k, beta, kumar, logitnorm, unif, triang\n')
for(k in t) { a<-k;b<- -length(x)/sum(log1p(-x^a)); if(!is.finite(b)) b<-exp(min(700,-a*log(.4)))*length(x)/length(x); cat(k,sum(dbeta(x,.4*k,.6*k,log=TRUE)),sum(extraDistr::dkumar(x,a,b,log=TRUE)),sum(dnorm(qlogis(x),qlogis(.4),1/k,log=TRUE)-log(x)-log1p(-x)),sum(dunif(x,.4-1/k,.4+1/k,log=TRUE)),sum(extraDistr::dtriang(x,.4-1/k,.4+1/k,.4,log=TRUE)),'\n') }
cat('Held-parameter collapse paths (log likelihood; n=5)\n')
for(k in t) {e<-1/k; cat(k, 'logitnorm-mu',sum(dnorm(qlogis(x),qlogis(.4),e,log=TRUE)-log(x)-log1p(-x)), 'unif-min',sum(dunif(x,.4,.4+e,log=TRUE)), 'unif-max',sum(dunif(x,.4-e,.4,log=TRUE)), 'triang-a-or-a+c',sum(extraDistr::dtriang(x,.4,.4+e,.4,log=TRUE)), 'triang-b-or-b+c',sum(extraDistr::dtriang(x,.4-e,.4,.4,log=TRUE)), 'triang-c',sum(extraDistr::dtriang(x,.4-e,.4+e,.4,log=TRUE)),'\n') }
cat('Reference boundary probes\n'); for(v in list(c(0,2),c(2,0),c(0,0),c(Inf,2),c(2,Inf),c(Inf,Inf)))cat('beta',v,dbeta(.4,v[1],v[2]),'\n')
cat('unif collapsed',suppressWarnings(dunif(.4,.4,.4)),'\n')
for(id in c('beta','kumar','logitnorm','unif','triang')) {f<-get(paste0('ml',id));e<-tryCatch(f(rep(.4,5)),error=function(e)conditionMessage(e));cat(id,'constant:',if(is.character(e))e else paste(coef(e),collapse=','),'\n')}

sink()
