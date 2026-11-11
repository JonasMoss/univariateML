# No package installation: these two source-level defects use base R only.
source("dev/research/evaluations/2026-11-10-qgauss-lit-data/dqgauss.R")
source("dev/research/evaluations/2026-11-10-qgauss-lit-data/cqgauss.R")
v <- dqgauss(2, q=0)
stopifnot(is.finite(v), v > 0, 2 > sqrt(3))
cat("Confirmed: dqgauss(2,q=0) =", format(v, digits=16), "outside support [-sqrt(3),sqrt(3)]\n")
v <- suppressMessages(capture.output(ans <- dqgauss(0,q=1)))
stopifnot(is.character(ans), length(v)>0)
cat("Confirmed: dqgauss(q=1) prints and returns a character value\n")
e <- tryCatch(cqgauss(c(.25,.75),q=0), error=identity)
stopifnot(inherits(e,"error"), grepl("length",conditionMessage(e)))
cat("Confirmed: vector cqgauss fails before any zipfR call:",conditionMessage(e),"\n")
# Exact Gaussian moments, E[Z^(2k)] = (2k-1)!!.
moment <- function(k) if(k==0) 1 else prod(seq(1,2*k-1,by=2))
p <- c(-1,-2,1)/4 # shape score polynomial in Z^2
info <- sum(outer(seq_along(p),seq_along(p),Vectorize(function(i,j) p[i]*p[j]*moment(i+j-2))))
cross <- sum(sapply(seq_along(p),function(i) p[i]*(moment(i)-moment(i-1))))
stopifnot(info==3.5,cross==2,info-cross^2/2==1.5)
cat("Gaussian information: I_eta_eta=",info,"; I_logscale_eta=",cross,"; efficient=",info-cross^2/2,"\n")
