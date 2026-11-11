# Independent exact-binary64 references are written by priority.py.
args <- commandArgs(TRUE)
for (shape in c(1e8,1e12,1e16)) {
  s <- 1000; x <- s*(1+0.5/shape)
  cat(sprintf('%.17g,%.17g,%.17g,%.17g,%.17g\n',shape,s,x,
              stats::dweibull(x,shape,s,log=TRUE),stats::pweibull(x,shape,s,lower.tail=FALSE,log.p=TRUE)))
}
