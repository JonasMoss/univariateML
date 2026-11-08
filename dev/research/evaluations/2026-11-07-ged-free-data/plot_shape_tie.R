# Run from repository root under the workstation CPU-slot wrapper.
a <- .32998660809120006
b <- 1.4239025460520294
shape <- exp(seq(log(1),log(65),length.out=800))
profile <- function(p) {
  logb <- (lgamma(1/p)-lgamma(3/p))/2
  -log(2)-logb-lgamma(1+1/p)-(23*exp(p*(log(a)-logb))+12*exp(p*(log(b)-logb)))/39
}
ll <- vapply(shape,profile,numeric(1))
roots <- c(1.108145553391854,25.39783214175462)
file <- "dev/research/evaluations/2026-11-07-ged-free/separated-shape-maxima.png"
png(file,width=1000,height=540,res=120,type="cairo")
par(mar=c(4.5,5,4.5,1),las=1)
plot(shape,ll,type="l",log="x",lwd=2.5,col="#1768a6",xlab=expression("Shape "*nu),
     ylab="Profile log likelihood per observation",ylim=range(c(ll,-1.237)),
     main="Two separated finite GED shape maxima")
abline(h=-log(2*sqrt(3)),col="#9b9b9b",lty=3)
points(roots,vapply(roots,profile,numeric(1)),pch=19,col="#ac3b23",cex=1.1)
text(roots,vapply(roots,profile,numeric(1))+.0016,labels=c("1.10815","25.39783"),col="#ac3b23",cex=.9)
legend("bottomleft",legend=c("Profile likelihood","Uniform height"),col=c("#1768a6","#9b9b9b"),lwd=c(2.5,1),lty=c(1,3),bty="n")
dev.off()
