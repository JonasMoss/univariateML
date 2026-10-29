# Reproduce first sweep's three PIG apparent differences. Independent Bessel
# formula is a precision cross-check of actuar's external reference.
suppressPackageStartupMessages(library(univariateML,lib.loc='/tmp/umvl-p0lib'))
ids<-c('pois','geom','nbinom','zip','zinb','zmpois','ztpois','zmgeom','ztgeom','zmnbinom','ztnbinom','zmlogarithmic','lgser','pig')
set.seed(791029)
for(id in ids) { smp<-lapply(c(20,100,1000),function(n) simulate_default(paste0('ml',id),n)); lim<-if(id %in% c('ztpois','ztgeom','ztnbinom','lgser')) pmax(1,rpois(100,2)) else rpois(100,2); if(id=='pig') { x1000<-smp[[3]]; xlim<-lim } }
pig_bessel<-function(x,mean,shape) { A<-2+shape/mean^2;z<-sqrt(shape*A);.5*log(2*shape/pi)+shape/mean+(x-.5)/2*log(shape/A)+log(besselK(z,abs(x-.5),expon.scaled=TRUE))-z-lgamma(x+1) }
cases<-list(default1000=list(x=x1000,heldmean=TRUE),ties=list(x=c(0,0,0,1,1,4),heldmean=FALSE),limit=list(x=xlim,heldmean=TRUE))
out<-list();j<-0L
for(case in names(cases)) { a<-cases[[case]]; ux<-sort(unique(a$x));freq<-tabulate(match(a$x,ux)); f<-function(z) {q<-if(a$heldmean)c(mean=2,shape=exp(z)) else c(mean=exp(z[1]),shape=exp(z[2])); val<-suppressWarnings(actuar::dpoisinvgauss(ux,q[1],q[2],log=TRUE)); if(any(!is.finite(val))) return(1e100); -sum(freq*val) }; z<-if(a$heldmean)log(3) else log(c(2,3));best<-NULL;for(s in c(-2,0,2)){o<-optim(z+s,f,method='BFGS',control=list(maxit=300,reltol=1e-10));if(is.null(best)||o$value<best$value)best<-o};q<-if(a$heldmean)c(mean=2,shape=exp(best$par)) else c(mean=exp(best$par[1]),shape=exp(best$par[2])); stable<-sum(freq*pig_bessel(ux,q[1],q[2]));fit<-if(a$heldmean)mlpig(a$x,mean=2) else mlpig(a$x);j<-j+1L;out[[j]]<-data.frame(case=case,mean=q[1],shape=q[2],actuar=-best$value,bessel_double_unstable=stable,package=as.numeric(logLik(fit))) }
# The Bessel expression suffers double-precision cancellation at these extreme
# optimizer points. Re-evaluate its finite half-integer polynomial with 120
# decimal digits. mpmath is used only here, in a temporary Python program.
tab<-do.call(rbind,out)
tmpin<-tempfile(fileext='.csv'); tmpout<-tempfile(fileext='.csv'); py<-tempfile(fileext='.py')
input<-do.call(rbind,lapply(seq_along(cases),function(i) {a<-cases[[i]]; data.frame(case=names(cases)[i],x=sort(unique(a$x)),count=tabulate(match(a$x,sort(unique(a$x)))),mean=tab$mean[i],shape=tab$shape[i])}))
write.csv(input,tmpin,row.names=FALSE,quote=FALSE)
writeLines(c("import sys,csv,math", "import mpmath as m", "m.mp.dps=120", "rows=list(csv.DictReader(open(sys.argv[1])))", "s={}", "for r in rows:", " k=int(r['x']); mu=m.mpf(r['mean']); lam=m.mpf(r['shape']); A=2+lam/mu**2; z=m.sqrt(lam*A); n=max(0,k-1)", " poly=sum(m.factorial(n+j)/(m.factorial(j)*m.factorial(n-j)*(2*z)**j) for j in range(n+1))", " logk=(m.log(m.pi)-m.log(2*z))/2-z+m.log(poly)", " lp=m.log(2*lam/m.pi)/2+lam/mu+(m.mpf(k)-m.mpf('0.5'))/2*m.log(lam/A)+logk-m.loggamma(k+1)", " s[r['case']]=s.get(r['case'],m.mpf('0'))+int(r['count'])*lp", "with open(sys.argv[2],'w') as f:", " w=csv.writer(f);w.writerow(['case','bessel_mp']);w.writerows((k,m.nstr(v,25)) for k,v in s.items())"),py)
status<-system2('python3',c(py,tmpin,tmpout));if(status!=0)stop('mpmath cross-check failed')
tab<-merge(tab,read.csv(tmpout),by='case');write.csv(tab,'notes/evaluations/2026-10-29-p0-counts/pig-reference-probe.csv',row.names=FALSE)

