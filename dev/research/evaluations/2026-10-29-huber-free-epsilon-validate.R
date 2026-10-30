# Reproducible research probes, after `just test`; not part of routine tests.
# /tmp/umvl-cpu/run Rscript <this file> > /tmp/umvl-codex/huberfree/probes.log
pkgload::load_all("univariateML-r", compile=FALSE, quiet=TRUE)
classify <- function(x, ...) {
  tryCatch({ f <- mlhuber(x,epsilon=NULL,...)
    c(face="finite",epsilon=unname(f["epsilon"]),logLik=as.numeric(logLik(f))) },
    error=function(e) c(face=conditionMessage(e),epsilon=NA,logLik=NA))
}
cat("Numerical shape map: symmetric generalized-normal midpoint quantiles, n=400\n")
p <- ((1:400)-.5)/400
for (power in c(.6,.8,1,1.2,1.5,2,3,4)) {
  x <- sign(p-.5)*qgamma(2*abs(p-.5),shape=1/power)^(1/power)
  kurt <- mean((x-mean(x))^4)/mean((x-mean(x))^2)^2
  cat("power",power,"kurtosis",kurt,classify(x),"\n")
}
cat("Seeded Huber recovery, n=100000\n")
set.seed(7931)
for (e in c(.5,.8,1.345)) {
  x <- extraDistr::rhuber(100000,mu=2,sigma=3,epsilon=e)
  cat("truth",e,classify(x),"\n")
}
cat("Timing n=1000000: generation excluded; elapsed seconds\n")
x <- extraDistr::rhuber(1000000,mu=2,sigma=3,epsilon=.8)
print(system.time(fixed <- mlhuber(x)))
print(system.time(free <- mlhuber(x,epsilon=NULL)))
print(free)
