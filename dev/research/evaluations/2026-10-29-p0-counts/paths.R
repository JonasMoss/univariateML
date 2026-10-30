# Explicit likelihood paths to every unattained count-family face discussed.
# Fixed sample examples; no package fitter is used.
suppressPackageStartupMessages(library(actuar)); suppressPackageStartupMessages(library(extraDistr))
out<-list();i<-0L
add<-function(path,t,ll) {i<<-i+1L;out[[i]]<<-data.frame(path=path,t=t,logLik=ll)}
x<-c(0,0,1,2,3); xp<-c(1,1,2,3); rseq<-c(1,10,100,1000,10000)
for(r in rseq) {
 p<-r/(r+2)
 add('nbinom_to_pois',r,sum(stats::dnbinom(x,r,p,log=TRUE)))
 add('zinb_to_zip',r,sum(extraDistr::dzinb(x,r,p,.3,log=TRUE)))
 add('ztnbinom_to_ztpois',r,sum(actuar::dztnbinom(xp,r,p,log=TRUE)))
 add('zmnbinom_to_zmpois',r,sum(actuar::dzmnbinom(x,r,p,.3,log=TRUE)))
 add('pig_to_pois',r,sum(actuar::dpoisinvgauss(x,2,r,log=TRUE)))
}
add('nbinom_to_pois',Inf,sum(stats::dpois(x,2,log=TRUE)))
add('zinb_to_zip',Inf,sum(extraDistr::dzip(x,2,.3,log=TRUE)))
add('ztnbinom_to_ztpois',Inf,sum(actuar::dztpois(xp,2,log=TRUE)))
add('zmnbinom_to_zmpois',Inf,sum(actuar::dzmpois(x,2,.3,log=TRUE)))
add('pig_to_pois',Inf,sum(stats::dpois(x,2,log=TRUE)))
for(t in c(.5,.1,.01,.001,1e-5)) {
 add('lgser_to_point_one',t,sum(extraDistr::dlgser(rep(1,5),t,log=TRUE)))
 add('zip_held_pi_to_point_zero',t,sum(extraDistr::dzip(rep(0,5),t,.3,log=TRUE)))
 add('pig_held_shape_to_point_zero',t,sum(actuar::dpoisinvgauss(rep(0,5),t,3,log=TRUE)))
}
for(mu in c(2,10,100,1000,Inf)) add('pig_to_levy',mu,sum(actuar::dpoisinvgauss(x,mu,3,log=TRUE)))
for(r in c(2,1,.1,.01,.001,0)) add('ztnbinom_to_logseries',r,sum(actuar::dztnbinom(xp,r,.4,log=TRUE)))
write.csv(do.call(rbind,out),'dev/research/evaluations/2026-10-29-p0-counts/paths.csv',row.names=FALSE)
