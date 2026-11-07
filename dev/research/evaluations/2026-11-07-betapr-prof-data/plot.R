# Numerical illustration; the exact tie argument is in the family dossier.
case <- jsonlite::fromJSON("dev/research/evaluations/2026-11-07-betapr-prof-data/counterexample-refined.json")
a <- case$a
x <- case$x
x[length(x)] <- x[length(x)]*exp(case$crossing$t)
lx <- log(x); n <- length(x)
profile <- function(b) {
  ls <- uniroot(function(t)(a+b)*mean(plogis(lx-t))-a,
    c(log(b)+min(lx)-log(a)-1,log(b)+max(lx)-log(a)+1),tol=1e-12)$root
  (a-1)*sum(lx)-n*a*ls-(a+b)*sum(log1p(x/exp(ls)))-n*lbeta(a,b)
}
b <- seq(.06,.22,length.out=401)
peaks <- case$crossing$maxima
peak <- max(peaks$logLik)
y <- vapply(b,profile,0.)-peak
png("build/scratch/betapr-conditional-crossing.png",width=1750,height=900,res=250,type="cairo-png")
par(mar=c(4.2,4.7,2.8,.9),mgp=c(2.8,.7,0),tcl=-.25,las=1,family="sans")
plot(b,y,type="n",xlab="Free shape2",ylab="Profile log-likelihood minus peak",
  ylim=range(c(y,.12)),axes=FALSE,
  main="Numerical crossing: two competing beta-prime maxima",cex.main=.95,font.main=1)
axis(1);axis(2);box(bty="l",col="#555555")
abline(h=0,lty=2,col="#8a8a8a",lwd=.7)
lines(b,y,col="#245a81",lwd=1.8)
colors <- c("#b55020","#237f68")
for(i in 1:2) {
  points(peaks$b[i],peaks$logLik[i]-peak,pch=21,bg=colors[i],col=colors[i],cex=1.1)
  text(peaks$b[i],.095,
    sprintf("shape2 = %.5f\nscale = %.6g",peaks$b[i],peaks$scale[i]),
    adj=c(.5,1),col=colors[i],cex=.8)
}
invisible(dev.off())
