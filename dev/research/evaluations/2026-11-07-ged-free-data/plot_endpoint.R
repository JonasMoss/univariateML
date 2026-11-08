args <- commandArgs(TRUE)
p <- read.csv(args[1])
limit <- as.numeric(args[3]); old <- as.numeric(args[4]); ceiling <- as.numeric(args[5])
grDevices::png(args[2],width=1000,height=525,res=125,pointsize=10,type="cairo")
par(mar=c(4,4.8,3.5,1),mgp=c(2.7,.8,0),las=1,cex.axis=.85,cex.lab=.9)
plot(p$nu,p$loglik,type="l",log="x",xaxt="n",bty="l",xaxs="i",yaxs="i",
     xlim=c(1,1e20),ylim=c(-5.8,-2.25),lwd=2,col="#205a83",
     xlab=expression(paste("Shape ",nu," (log scale)")),
     ylab="Total log likelihood (two observations)")
axis(1,at=10^seq(0,20,5),labels=parse(text=paste0("10^",seq(0,20,5))))
abline(h=old,col="#b14d41",lty=2,lwd=1.4)
abline(h=limit,col="#3b7856",lty=3,lwd=1.6)
abline(v=ceiling,col="#888888",lty=3)
text(ceiling*1.3,-5.2,"Legacy held-scale\nsearch ceiling",adj=c(0,.5),col="#666666",cex=.7)
title(main="Rounded endpoints change the large-shape path",adj=0,line=1.8,cex.main=1.05)
mtext("Mean 0 and sd 1 held; data +/- float(sqrt(3))",side=3,line=.35,adj=0,cex=.8)
legend("bottomright",inset=c(.02,.03),bty="n",cex=.72,
       legend=c("80-digit GED likelihood","Previous endpoint face comparison",
                "Large-shape limit for the binary observations"),
       col=c("#205a83","#b14d41","#3b7856"),lty=c(1,2,3),lwd=c(2,1.4,1.6))
invisible(dev.off())
