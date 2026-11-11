pkgload::load_all('univariateML-r', compile=FALSE, quiet=TRUE)
x <- c(-3,-1,-.2,0,.3,.4,2)
rows <- list()
for (nu in c(.3,.5,.9)) for (xi in c(.3,1.3,3)) {
 f <- mlsged(x, mean=.1, nu=nu, xi=xi)
 rows[[length(rows)+1]] <- data.frame(mean=.1,sd=unname(coef(f)['sd']),nu=nu,xi=xi,reported=as.numeric(logLik(f)),dpqr=sum(dml(x,f,log=TRUE)))
}
options(digits=17)
out <- do.call(rbind,rows)
out[] <- lapply(out,function(x) sprintf('%.17g',x))
write.table(out,'dev/research/evaluations/2026-11-11-dpqr-extreme-data/sged-cusp-R.csv',row.names=FALSE,col.names=TRUE,sep=',',quote=FALSE)
